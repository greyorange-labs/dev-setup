# High-Level Design: Distributed Tracing System

## Table of Contents
1. [Introduction](#introduction)
2. [Architecture Overview](#architecture-overview)
3. [Core Components](#core-components)
4. [Data Flow](#data-flow)
5. [Integration Architecture](#integration-architecture)
6. [Deployment Architecture](#deployment-architecture)
7. [Scalability & Performance](#scalability--performance)
8. [Security & Compliance](#security--compliance)
9. [Design Decisions](#design-decisions)

---

## 1. Introduction

### 1.1 Purpose
This document describes the high-level architecture for implementing distributed tracing across our multi-language microservices ecosystem using OpenTelemetry and Jaeger.

### 1.2 Scope
- All microservices (Java, Erlang, Python, Node.js)
- Infrastructure services (Kafka, PostgreSQL, Redis, RabbitMQ, EMQX, Mnesia, InfluxDB, Node-RED)
- API Gateway integration
- Cross-service request tracking

### 1.3 Goals
- **End-to-end visibility**: Track requests across all services
- **Performance monitoring**: Identify bottlenecks and latency issues
- **Error tracking**: Correlate errors across service boundaries
- **Dependency mapping**: Visualize service relationships
- **Minimal overhead**: <1% performance impact

### 1.4 Non-Goals
- Application Performance Monitoring (APM) features like code profiling
- Log aggregation (use ELK/Loki for logs)
- Business metrics/analytics

---

## 2. Architecture Overview

### 2.1 Conceptual Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                          CLIENT APPLICATIONS                             │
└────────────────────────────────┬────────────────────────────────────────┘
                                 │
                                 ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                            API GATEWAY                                   │
│                    (Trace Context Propagation)                           │
└────────────────────────────────┬────────────────────────────────────────┘
                                 │
         ┌───────────────────────┼───────────────────────┐
         │                       │                       │
         ▼                       ▼                       ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│  Java Services  │    │ Python Services │    │ Erlang Services │
│   (OTel Java)   │    │  (OTel Python)  │    │  (OTel Erlang)  │
└────────┬────────┘    └────────┬────────┘    └────────┬────────┘
         │                      │                       │
         │         ┌────────────┼────────────┐         │
         │         │            │            │         │
         ▼         ▼            ▼            ▼         ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                    OPENTELEMETRY COLLECTORS                              │
│                  (Receive, Process, Export)                              │
└────────────────────────────────┬────────────────────────────────────────┘
                                 │
                     ┌───────────┼───────────┐
                     │           │           │
                     ▼           ▼           ▼
            ┌─────────────┐ ┌─────────┐ ┌─────────┐
            │    Kafka    │ │ Jaeger  │ │  Other  │
            │   (Buffer)  │ │ Backend │ │ Backends│
            └──────┬──────┘ └────┬────┘ └────┬────┘
                   │             │           │
                   └─────────────┼───────────┘
                                 ▼
                   ┌──────────────────────────┐
                   │    JAEGER BACKEND        │
                   │  - Collector             │
                   │  - Query Service         │
                   │  - UI                    │
                   └────────────┬─────────────┘
                                │
                                ▼
                   ┌──────────────────────────┐
                   │   STORAGE BACKEND        │
                   │  (Elasticsearch/         │
                   │   Cassandra)             │
                   └──────────────────────────┘
```

### 2.2 Logical Architecture Layers

| Layer               | Components             | Responsibility                       |
| ------------------- | ---------------------- | ------------------------------------ |
| **Instrumentation** | OTel SDKs              | Generate and propagate trace context |
| **Collection**      | OTel Collector         | Receive, batch, and route traces     |
| **Transport**       | Kafka, gRPC            | Reliable trace data transmission     |
| **Storage**         | Jaeger + Elasticsearch | Persist and index traces             |
| **Query**           | Jaeger Query Service   | Search and retrieve traces           |
| **Visualization**   | Jaeger UI              | Display and analyze traces           |

---

## 3. Core Components

### 3.1 OpenTelemetry SDKs

**Purpose**: Instrument application code to generate traces

**Language-Specific SDKs**:
- **Java**: `opentelemetry-java` + auto-instrumentation agent
- **Python**: `opentelemetry-python` with auto-instrumentation
- **Erlang**: `opentelemetry-erlang` SDK
- **Node.js**: `@opentelemetry/node` SDK

**Key Features**:
- Automatic instrumentation for popular frameworks
- Manual span creation for custom logic
- Context propagation (W3C Trace Context standard)
- Sampling configuration
- Resource attributes (service name, version, etc.)

**Configuration Parameters**:
```yaml
service.name: <service-name>
service.version: <version>
deployment.environment: <env>
otel.exporter.otlp.endpoint: <collector-endpoint>
otel.traces.sampler: parentbased_always_on
```

### 3.2 OpenTelemetry Collector

**Purpose**: Central aggregation point for trace data

**Deployment Models**:
1. **Agent Mode**: Sidecar/daemon on each node
2. **Gateway Mode**: Centralized collector cluster

**Recommendation**: Hybrid approach
- Agent collectors on each K8s node/VM
- Gateway collectors for aggregation

**Components**:
- **Receivers**: Accept traces (OTLP, Jaeger, Zipkin protocols)
- **Processors**: Batch, sample, enrich, filter traces
- **Exporters**: Send to Jaeger, Kafka, or other backends

**Pipeline Configuration**:
```yaml
receivers:
  otlp:
    protocols:
      grpc:
        endpoint: 0.0.0.0:4317
      http:
        endpoint: 0.0.0.0:4318

processors:
  batch:
    timeout: 10s
    send_batch_size: 1024
  memory_limiter:
    check_interval: 1s
    limit_mib: 512

exporters:
  jaeger:
    endpoint: jaeger-collector:14250
    tls:
      insecure: false
  kafka:
    brokers: [kafka1:9092, kafka2:9092]
    topic: traces

service:
  pipelines:
    traces:
      receivers: [otlp]
      processors: [memory_limiter, batch]
      exporters: [jaeger, kafka]
```

### 3.3 Jaeger Backend

**Purpose**: Store, query, and visualize traces

**Components**:

#### 3.3.1 Jaeger Collector
- Receives traces from OTel Collectors
- Validates and processes spans
- Writes to storage backend
- Supports Kafka for buffering

#### 3.3.2 Jaeger Query Service
- REST API for trace retrieval
- Span aggregation and filtering
- Service dependency graph generation

#### 3.3.3 Jaeger UI
- Web-based trace visualization
- Service dependency graph
- Trace comparison
- Search and filtering

**Storage Options**:

| Storage           | Use Case                 | Pros                           | Cons                  |
| ----------------- | ------------------------ | ------------------------------ | --------------------- |
| **Elasticsearch** | Production (recommended) | Fast queries, scalable, mature | Higher resource usage |
| **Cassandra**     | Very high scale          | Highly scalable, distributed   | Complex ops           |
| **BadgerDB**      | Development/testing      | Simple, embedded               | Not distributed       |
| **Memory**        | Testing only             | Fast                           | Data loss on restart  |

**Recommendation**: Elasticsearch for production

### 3.4 Kafka Integration

**Purpose**: Buffer traces during high load and provide fault tolerance

**Benefits**:
- Decouples producers from consumers
- Handles traffic spikes
- Provides replay capability
- Enables multiple consumers

**Architecture**:
```
OTel Collector → Kafka Topic (traces) → Jaeger Ingester → Storage
```

**Configuration**:
- Topic: `jaeger-spans`
- Partitions: 12 (based on throughput)
- Replication: 3
- Retention: 1 hour (buffer only)

---

## 4. Data Flow

### 4.1 Request Flow with Tracing

```
1. Client Request → API Gateway
   ├─ Generate Trace ID (if new request)
   ├─ Create Root Span
   └─ Add trace context to headers (traceparent)

2. API Gateway → Service A (Java)
   ├─ Extract trace context from headers
   ├─ Create child span
   ├─ Execute business logic
   └─ Export span to OTel Collector

3. Service A → Service B (Python)
   ├─ Propagate trace context in headers
   ├─ Service B creates child span
   ├─ Service B calls Database
   │  └─ Create DB span (auto-instrumented)
   └─ Export span to OTel Collector

4. Service A → Message Queue (Kafka)
   ├─ Inject trace context into message headers
   └─ Async processing continues trace

5. OTel Collector Processing
   ├─ Receive spans via OTLP/gRPC
   ├─ Batch spans
   ├─ Add resource attributes
   └─ Export to Jaeger

6. Jaeger Backend
   ├─ Store spans in Elasticsearch
   ├─ Index by trace ID, service, operation
   └─ Make available for querying

7. User Queries Trace
   ├─ Search by trace ID, service, tags
   ├─ Jaeger Query retrieves from Elasticsearch
   └─ Jaeger UI displays complete trace
```

### 4.2 Trace Structure

```
Trace (single request across services)
│
├─ Span (API Gateway) - Root Span
│  ├─ trace_id: abc123
│  ├─ span_id: span1
│  ├─ parent_span_id: null
│  ├─ service: api-gateway
│  ├─ operation: GET /api/orders
│  └─ duration: 250ms
│
   ├─ Span (Order Service - Java)
   │  ├─ trace_id: abc123
   │  ├─ span_id: span2
   │  ├─ parent_span_id: span1
   │  ├─ service: order-service
   │  ├─ operation: createOrder
   │  └─ duration: 200ms
   │
      ├─ Span (PostgreSQL Query)
      │  ├─ trace_id: abc123
      │  ├─ span_id: span3
      │  ├─ parent_span_id: span2
      │  ├─ service: order-service
      │  ├─ operation: INSERT INTO orders
      │  └─ duration: 50ms
      │
      └─ Span (Inventory Service - Python)
         ├─ trace_id: abc123
         ├─ span_id: span4
         ├─ parent_span_id: span2
         ├─ service: inventory-service
         ├─ operation: checkInventory
         └─ duration: 100ms
```

### 4.3 Context Propagation

**W3C Trace Context Standard** (used by OpenTelemetry):

```
HTTP Headers:
traceparent: 00-<trace-id>-<span-id>-<flags>
tracestate: vendor1=value1,vendor2=value2

Example:
traceparent: 00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01
```

**Message Queue Context**:
```
Kafka Message Headers:
traceparent: 00-<trace-id>-<span-id>-<flags>
```

---

## 5. Integration Architecture

### 5.1 API Gateway Integration

**Purpose**: Entry point for all external requests, initiates traces

**Implementation**:
- OTel Java agent for auto-instrumentation
- Custom middleware for trace context injection
- Header propagation to downstream services

**Key Responsibilities**:
- Generate trace ID for new requests
- Extract trace context from incoming requests
- Add service tags and metadata
- Propagate context to all downstream calls

### 5.2 Service-Level Integration

#### 5.2.1 Java Services (Spring Boot)
```
┌──────────────────────────────┐
│   Spring Boot Application    │
│                              │
│  ┌────────────────────────┐  │
│  │ OTel Java Agent        │  │
│  │ (Auto-instrumentation) │  │
│  └────────────────────────┘  │
│                              │
│  Auto-traced:                │
│  - HTTP clients/servers      │
│  - JDBC calls                │
│  - Kafka producers/consumers │
│  - Redis operations          │
└──────────────────────────────┘
```

#### 5.2.2 Python Services (FastAPI/Django)
```
┌──────────────────────────────┐
│   Python Application         │
│                              │
│  ┌────────────────────────┐  │
│  │ opentelemetry-           │  │
│  │ instrumentation-*        │  │
│  └────────────────────────┘  │
│                              │
│  Auto-traced:                │
│  - HTTP requests (requests)  │
│  - FastAPI/Django routes     │
│  - psycopg2 (PostgreSQL)     │
│  - Redis clients             │
└──────────────────────────────┘
```

#### 5.2.3 Erlang Services (OTP)
```
┌──────────────────────────────┐
│   Erlang/OTP Application     │
│                              │
│  ┌────────────────────────┐  │
│  │ opentelemetry library    │  │
│  │ (Manual + Auto)          │  │
│  └────────────────────────┘  │
│                              │
│  Traced:                     │
│  - HTTP handlers (cowboy)    │
│  - GenServer calls           │
│  - Database calls (Mnesia)   │
│  - EMQX/RabbitMQ             │
└──────────────────────────────┘
```

### 5.3 Infrastructure Integration

#### Database Tracing
```
Application → OTel SDK → Span with:
  - db.system: postgresql/redis/mnesia
  - db.statement: SQL/command
  - db.connection_string: host:port
  - span.kind: CLIENT
  - duration: query time
```

#### Message Queue Tracing
```
Producer → Inject trace context → Message headers
Consumer → Extract trace context → Continue trace

Kafka:
  - Headers: traceparent, tracestate

RabbitMQ:
  - Message properties: traceparent, tracestate

EMQX (MQTT):
  - User properties: traceparent, tracestate
```

---

## 6. Deployment Architecture

### 6.1 Kubernetes Deployment

```
┌─────────────────────────────────────────────────────────────────┐
│                    Kubernetes Cluster                           │
│                                                                 │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │              Application Namespace                         │ │
│  │                                                            │ │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │ │
│  │  │  Service A  │  │  Service B  │  │  Service C  │         │ │
│  │  │  (Java)     │  │  (Python)   │  │  (Erlang)   │         │ │
│  │  │             │  │             │  │             │         │ │
│  │  │ OTel SDK    │  │ OTel SDK    │  │ OTel SDK    │         │ │
│  │  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘         │ │
│  │         │                │                │                │ │
│  │         └────────────────┼────────────────┘                │ │
│  │                          │                                 │ │
│  └──────────────────────────┼─────────────────────────────────┘ │
│                             │                                   │
│  ┌──────────────────────────┼─────────────────────────────────┐ │
│  │        Observability Namespace                             │ │
│  │                          │                                 │ │
│  │         ┌────────────────▼───────────────────┐             │ │
│  │         │  OTel Collector DaemonSet          │             │ │
│  │         │  (Agent on each node)              │             │ │
│  │         └────────────────┬───────────────────┘             │ │
│  │                          │                                 │ │
│  │         ┌────────────────▼───────────────────┐             │ │
│  │         │  OTel Collector Deployment         │             │ │
│  │         │  (Gateway - Load Balanced)         │             │ │
│  │         └────────────────┬───────────────────┘             │ │
│  │                          │                                 │ │
│  │         ┌────────────────┼───────────────────┐             │ │
│  │         │                ▼                   │             │ │
│  │    ┌────▼─────┐    ┌──────────┐    ┌───────▼──────┐        │ │
│  │    │  Kafka   │    │  Jaeger  │    │ Elasticsearch│        │ │
│  │    │ (Buffer) │    │ Collector│    │   Cluster    │        │ │
│  │    └────┬─────┘    └────┬─────┘    └───────┬──────┘        │ │
│  │         │               │                  │               │ │
│  │         └───────────────┼──────────────────┘               │ │
│  │                         │                                  │ │
│  │                ┌────────▼────────┐                         │ │
│  │                │  Jaeger Query   │                         │ │
│  │                │     Service     │                         │ │
│  │                └────────┬────────┘                         │ │
│  │                         │                                  │ │
│  │                ┌────────▼────────┐                         │ │
│  │                │   Jaeger UI     │                         │ │
│  │                │  (Service/      │                         │ │
│  │                │   Ingress)      │                         │ │
│  │                └─────────────────┘                         │ │
│  └────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────┘
```

### 6.2 Resource Allocation

| Component                | CPU     | Memory | Replicas | Notes       |
| ------------------------ | ------- | ------ | -------- | ----------- |
| OTel Collector (Agent)   | 200m    | 512Mi  | 1/node   | DaemonSet   |
| OTel Collector (Gateway) | 1 core  | 2Gi    | 3        | Deployment  |
| Jaeger Collector         | 2 cores | 4Gi    | 3        | StatefulSet |
| Jaeger Query             | 1 core  | 2Gi    | 2        | Deployment  |
| Jaeger UI                | 500m    | 1Gi    | 2        | Deployment  |
| Elasticsearch            | 4 cores | 16Gi   | 3        | StatefulSet |

### 6.3 Network Architecture

**Endpoints**:
- Applications → OTel Collector Agent: `otel-agent:4317` (gRPC)
- OTel Agent → OTel Gateway: `otel-gateway:4317` (gRPC)
- OTel Gateway → Jaeger: `jaeger-collector:14250` (gRPC)
- OTel Gateway → Kafka: `kafka-brokers:9092`
- Jaeger Collector → Elasticsearch: `elasticsearch:9200`
- Users → Jaeger UI: `https://jaeger.company.com`

**Service Mesh Consideration**:
If using Istio/Linkerd:
- Disable automatic tracing (conflicts with OTel)
- Or use Envoy's native OTLP support
- Ensure trace context propagation compatibility

---

## 7. Scalability & Performance

### 7.1 Scaling Strategy

#### 7.1.1 Horizontal Scaling
```
Component                Scaling Metric              Target
─────────────────────────────────────────────────────────────
OTel Collector Gateway   CPU > 70%                   Add replica
Jaeger Collector        Ingestion rate > 10k/s       Add replica
Jaeger Query            Request rate > 100/s         Add replica
Elasticsearch           Disk usage > 80%             Add node
```

#### 7.1.2 Vertical Scaling
- Elasticsearch nodes: Scale memory for better query performance
- Jaeger Collector: Scale memory for larger batch sizes

### 7.2 Performance Considerations

#### 7.2.1 Sampling Strategy
```
┌─────────────────────────────────────────────────────────┐
│                  Sampling Strategies                    │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  1. Head-based Sampling (at application)                │
│     - AlwaysOn: 100% (dev/staging)                      │
│     - Probability: 10% (production)                     │
│     - RateLimiting: 100 traces/sec/service              │
│                                                         │
│  2. Tail-based Sampling (at collector)                  │
│     - Always sample errors (status=error)               │
│     - Always sample slow requests (duration > 1s)       │
│     - Sample 10% of successful fast requests            │
│                                                         │
│  Recommended: Hybrid approach                           │
│  - 100% sampling at app (low overhead)                  │
│  - Intelligent filtering at collector                   │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

#### 7.2.2 Performance Impact
| Component            | Latency Overhead | CPU Overhead | Memory Overhead |
| -------------------- | ---------------- | ------------ | --------------- |
| Auto-instrumentation | <1ms             | 2-5%         | 50-100MB        |
| OTel SDK             | <0.5ms           | 1-2%         | 20-50MB         |
| Context propagation  | <0.1ms           | <1%          | Negligible      |

#### 7.2.3 Batching Configuration
```yaml
# Optimal batching for performance
batch_processor:
  timeout: 10s              # Send batch every 10s
  send_batch_size: 1024     # Or when 1024 spans collected
  send_batch_max_size: 2048 # Maximum batch size
```

### 7.3 Data Retention

| Environment | Retention Period | Storage Size (Est.) |
| ----------- | ---------------- | ------------------- |
| Development | 7 days           | 50GB                |
| Staging     | 14 days          | 100GB               |
| Production  | 30 days          | 500GB               |

**Elasticsearch Index Strategy**:
- Daily indices: `jaeger-span-2025-10-15`
- Auto-delete old indices via ILM policy
- Hot-warm-cold architecture for cost optimization

---

## 8. Security & Compliance

### 8.1 Data Security

#### 8.1.1 Transport Security
- **TLS/mTLS**: All communication encrypted
- **Certificate Management**: Cert-manager in K8s
- **Endpoints**:
  - Application → Collector: TLS
  - Collector → Jaeger: TLS
  - Collector → Kafka: SASL_SSL

#### 8.1.2 Authentication & Authorization
```
┌───────────────────────────────────────────────────┐
│              Security Layers                      │
├───────────────────────────────────────────────────┤
│                                                   │
│  1. Jaeger UI Access                             │
│     - OAuth2/OIDC (e.g., Okta, Auth0)           │
│     - Role-based access control                  │
│     - Teams can only see their services          │
│                                                   │
│  2. API Access                                   │
│     - API keys for programmatic access           │
│     - JWT tokens for service-to-service          │
│                                                   │
│  3. Data Access                                  │
│     - Elasticsearch secured with X-Pack          │
│     - Service accounts with minimal permissions  │
│                                                   │
└───────────────────────────────────────────────────┘
```

### 8.2 Data Privacy

#### 8.2.1 PII Protection
```
Strategy:
1. Automatic scrubbing at OTel Collector
   - Regex patterns for emails, SSN, credit cards
   - Hash sensitive IDs before export

2. Application-level masking
   - Custom span processors
   - Redact sensitive fields in span attributes

3. Access controls
   - Audit logs for trace access
   - Restricted access to traces with sensitive data
```

#### 8.2.2 Compliance
- **GDPR**: Right to be forgotten (delete traces by user ID)
- **PCI-DSS**: No cardholder data in traces
- **HIPAA**: Encrypt traces at rest and in transit

### 8.3 Network Security

```
┌─────────────────────────────────────────────────┐
│           Network Security Controls             │
├─────────────────────────────────────────────────┤
│                                                 │
│  1. Network Policies (K8s)                     │
│     - Isolate observability namespace          │
│     - Allow only required ingress/egress       │
│                                                 │
│  2. Service Mesh (Optional)                    │
│     - mTLS between all services                │
│     - Policy-based access control              │
│                                                 │
│  3. Firewall Rules                             │
│     - Restrict Jaeger UI to VPN/corporate net  │
│     - Collector endpoints not publicly exposed │
│                                                 │
└─────────────────────────────────────────────────┘
```

---

## 9. Design Decisions

### 9.1 Why OpenTelemetry?

| Criteria                 | OpenTelemetry         | Alternatives (Jaeger SDK, Zipkin) |
| ------------------------ | --------------------- | --------------------------------- |
| **Vendor neutrality**    | ✅ CNCF standard       | ❌ Vendor-specific                 |
| **Future-proof**         | ✅ Active development  | ⚠️ Legacy                          |
| **Language support**     | ✅ All major languages | ⚠️ Limited                         |
| **Auto-instrumentation** | ✅ Extensive           | ⚠️ Partial                         |
| **Ecosystem**            | ✅ Large community     | ⚠️ Smaller                         |
| **Flexibility**          | ✅ Multiple backends   | ❌ Single backend                  |

### 9.2 Why Jaeger Backend?

| Criteria            | Jaeger             | Zipkin    | Tempo       | X-Ray          |
| ------------------- | ------------------ | --------- | ----------- | -------------- |
| **Maturity**        | ✅ Production-ready | ✅ Mature  | ⚠️ Newer     | ✅ AWS-specific |
| **UI Features**     | ✅ Rich UI          | ⚠️ Basic   | ⚠️ Limited   | ⚠️ AWS Console  |
| **Scalability**     | ✅ High             | ⚠️ Medium  | ✅ Very high | ✅ Managed      |
| **Cost**            | ✅ Free             | ✅ Free    | ✅ Free      | ❌ AWS costs    |
| **Storage options** | ✅ Multiple         | ⚠️ Limited | ✅ Multiple  | ❌ AWS only     |
| **Community**       | ✅ Large            | ✅ Large   | ⚠️ Growing   | ⚠️ AWS-focused  |

**Decision**: Jaeger provides the best balance of features, maturity, and flexibility.

### 9.3 Storage: Elasticsearch vs Cassandra

| Factor                     | Elasticsearch  | Cassandra     |
| -------------------------- | -------------- | ------------- |
| **Query flexibility**      | ✅ Rich queries | ⚠️ Limited     |
| **Scalability**            | ✅ Good         | ✅ Excellent   |
| **Operational complexity** | ✅ Moderate     | ⚠️ High        |
| **Cost**                   | ✅ Moderate     | ⚠️ Higher      |
| **Team expertise**         | ✅ Common       | ⚠️ Specialized |

**Decision**: Elasticsearch for better query capabilities and team familiarity.

### 9.4 Deployment: Sidecar vs DaemonSet

| Approach         | Pros                  | Cons                    | Decision     |
| ---------------- | --------------------- | ----------------------- | ------------ |
| **Sidecar**      | Isolated, per-pod     | Resource overhead       | ❌ Not chosen |
| **DaemonSet**    | Efficient, node-level | Shared resource         | ✅ Chosen     |
| **Gateway only** | Simple                | Single point of failure | ❌ Not chosen |

**Decision**: DaemonSet + Gateway for efficiency and fault tolerance.

### 9.5 Sampling Strategy

```
Decision: Hybrid Tail-based Sampling

Rationale:
- Keep 100% of errors for debugging
- Keep 100% of slow requests (>1s) for performance analysis
- Sample 10% of successful fast requests for cost optimization
- Reduces storage by ~80% while maintaining visibility

Implementation:
- Head-based at application: 100% (low overhead)
- Tail-based at collector: Intelligent filtering
```

### 9.6 Context Propagation Standard

**Decision**: W3C Trace Context

**Rationale**:
- Industry standard
- Supported by all OTel SDKs
- Interoperable with non-OTel services
- Future-proof

### 9.7 Kafka vs Direct to Jaeger

| Approach       | Pros                              | Cons             | Decision     |
| -------------- | --------------------------------- | ---------------- | ------------ |
| **With Kafka** | Buffering, replay, fault-tolerant | Added complexity | ✅ Chosen     |
| **Direct**     | Simpler, lower latency            | No buffering     | ❌ Not chosen |

**Decision**: Use Kafka for production resilience, direct for dev/test.

---

## 10. Architecture Diagrams

### 10.1 Component Interaction Diagram

```
┌──────────────────────────────────────────────────────────────────┐
│                        TRACE LIFECYCLE                            │
└──────────────────────────────────────────────────────────────────┘

┌─────────────┐
│   Client    │
└──────┬──────┘
       │ 1. HTTP Request
       ▼
┌─────────────────────────────────────────────────────────────────┐
│  API Gateway (OTel Instrumented)                                 │
│  - Generate trace_id: abc123                                     │
│  - Create root span: span_id=1                                   │
│  - Add traceparent header                                        │
└──────┬──────────────────────────────────────────────────────────┘
       │ 2. Forward request with trace context
       ▼
┌─────────────────────────────────────────────────────────────────┐
│  Service A - Order Service (Java + OTel)                         │
│  - Extract traceparent from header                               │
│  - Create child span: span_id=2, parent=1                        │
│  - Execute: createOrder()                                        │
│    ├─ DB Span: span_id=3, parent=2 (auto-instrumented)          │
│    └─ Async export spans to OTel Collector                       │
└──────┬──────────────────────────────────────────────────────────┘
       │ 3. Call inventory service with trace context
       ▼
┌─────────────────────────────────────────────────────────────────┐
│  Service B - Inventory Service (Python + OTel)                   │
│  - Extract traceparent from header                               │
│  - Create child span: span_id=4, parent=2                        │
│  - Execute: checkInventory()                                     │
│  - Async export spans to OTel Collector                          │
└──────┬──────────────────────────────────────────────────────────┘
       │ 4. All spans exported
       ▼
┌─────────────────────────────────────────────────────────────────┐
│  OTel Collector (Agent - DaemonSet)                              │
│  - Receive spans via OTLP/gRPC (port 4317)                       │
│  - Buffer in memory                                              │
│  - Forward to Gateway Collector                                  │
└──────┬──────────────────────────────────────────────────────────┘
       │ 5. Aggregation
       ▼
┌─────────────────────────────────────────────────────────────────┐
│  OTel Collector (Gateway - Deployment)                           │
│  - Batch spans (1024 spans or 10s timeout)                       │
│  - Apply tail-based sampling rules                               │
│  - Add global attributes                                         │
│  - Export to Jaeger & Kafka                                      │
└──────┬────────────────────────────┬─────────────────────────────┘
       │ 6a. Primary path            │ 6b. Backup buffer
       ▼                             ▼
┌──────────────────┐         ┌──────────────────┐
│ Jaeger Collector │         │  Kafka Topic     │
│  - Validate spans│         │  (jaeger-spans)  │
│  - Write to ES   │         │  - Retention: 1h │
└────────┬─────────┘         └────────┬─────────┘
         │ 7. Persist                 │
         ▼                            │
┌──────────────────┐                  │
│  Elasticsearch   │◄─────────────────┘ (Jaeger Ingester)
│  - Index spans   │
│  - Daily indices │
│  - ILM policies  │
└────────┬─────────┘
         │ 8. Query
         ▼
┌──────────────────┐
│ Jaeger Query     │
│  - Search API    │
│  - Aggregation   │
└────────┬─────────┘
         │ 9. Visualize
         ▼
┌──────────────────┐
│   Jaeger UI      │
│  - Timeline view │
│  - Service graph │
│  - Trace details │
└──────────────────┘
         │ 10. User views trace
         ▼
    ┌─────────┐
    │  User   │
    └─────────┘
```

### 10.2 Multi-Environment Architecture

```
┌────────────────────────────────────────────────────────────────┐
│                       DEVELOPMENT                               │
├────────────────────────────────────────────────────────────────┤
│  Services → OTel Collector → Jaeger All-in-One (memory)       │
│  - 100% sampling                                               │
│  - Single node                                                 │
│  - Minimal resources                                           │
└────────────────────────────────────────────────────────────────┘

┌────────────────────────────────────────────────────────────────┐
│                        STAGING                                  │
├────────────────────────────────────────────────────────────────┤
│  Services → OTel Collectors → Jaeger → Elasticsearch (3 nodes)│
│  - 50% sampling                                                │
│  - 7-day retention                                             │
│  - Production-like setup                                       │
└────────────────────────────────────────────────────────────────┘

┌────────────────────────────────────────────────────────────────┐
│                      PRODUCTION                                 │
├────────────────────────────────────────────────────────────────┤
│  Services → OTel Agents → OTel Gateway → Kafka → Jaeger       │
│           → Elasticsearch Cluster (6 nodes, hot-warm-cold)    │
│  - Tail-based sampling (intelligent)                           │
│  - 30-day retention                                            │
│  - HA, multi-AZ                                                │
│  - Monitoring & alerting                                       │
└────────────────────────────────────────────────────────────────┘
```

---

## 11. Success Criteria

### 11.1 Technical Metrics

| Metric                | Target | Measurement                               |
| --------------------- | ------ | ----------------------------------------- |
| Service coverage      | 100%   | All services instrumented                 |
| Trace completion rate | >95%   | Traces with all expected spans            |
| Query latency (p99)   | <500ms | Jaeger UI query response time             |
| Data freshness        | <30s   | Time from span generation to availability |
| System overhead       | <2%    | CPU/memory impact on applications         |

### 11.2 Business Metrics

| Metric                         | Target | Measurement                 |
| ------------------------------ | ------ | --------------------------- |
| MTTR reduction                 | 40%    | Time to identify root cause |
| Team adoption                  | 80%    | Teams actively using traces |
| Incidents debugged with traces | 70%    | Percentage of incidents     |
| Developer satisfaction         | 4/5    | Survey score                |

---

## 12. Next Steps

1. **Review & Approval**: Architecture review with stakeholders
2. **POC Implementation**: 2-week proof of concept with 2 services
3. **Infrastructure Setup**: Deploy Jaeger, Elasticsearch, OTel Collectors
4. **Pilot Rollout**: Instrument 5 critical services
5. **Full Deployment**: Gradual rollout to all services
6. **Training**: Developer workshops on using traces
7. **Optimization**: Fine-tune sampling, retention, and performance

---

## 13. References

- OpenTelemetry Documentation: https://opentelemetry.io/docs/
- Jaeger Documentation: https://www.jaegertracing.io/docs/
- W3C Trace Context: https://www.w3.org/TR/trace-context/
- OTel Collector Configuration: https://opentelemetry.io/docs/collector/configuration/

---

*Document Version: 1.0*
*Last Updated: October 15, 2025*
*Authors: Architecture Team*
*Status: Draft for Review*
