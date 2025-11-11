# SigNoz Architecture - High-Level Design

## Table of Contents
1. [Introduction](#introduction)
2. [Architecture Overview](#architecture-overview)
3. [Core Components](#core-components)
4. [Data Flow](#data-flow)
5. [Storage Architecture](#storage-architecture)
6. [Deployment Architecture](#deployment-architecture)
7. [Comparison with Jaeger](#comparison-with-jaeger)
8. [Design Decisions](#design-decisions)

---

## 1. Introduction

### 1.1 What is SigNoz?

SigNoz is an **open-source, full-stack observability platform** that provides:
- **Distributed Tracing** (like Jaeger)
- **Metrics Monitoring** (like Prometheus + Grafana)
- **Logs Management** (like ELK/Loki)
- **Unified Dashboards** (all-in-one)
- **Built-in Alerting** (no additional tools needed)

**Built On:**
- OpenTelemetry (vendor-neutral instrumentation)
- ClickHouse (columnar database for storage)
- React (modern UI)

### 1.2 Key Differentiators

| Feature | SigNoz | Jaeger |
|---------|--------|--------|
| **Scope** | Traces + Metrics + Logs | Traces only |
| **Storage** | ClickHouse (columnar) | Elasticsearch/Cassandra |
| **UI** | Unified observability | Trace-focused |
| **Alerting** | Built-in | External (Prometheus) |
| **Dashboards** | Built-in | External (Grafana) |
| **Query Language** | ClickHouse SQL, PromQL | Limited |

### 1.3 Architecture Philosophy

```
┌────────────────────────────────────────────────────┐
│           SigNoz Philosophy                        │
├────────────────────────────────────────────────────┤
│                                                    │
│  "Single pane of glass for all observability"     │
│                                                    │
│  Traces ───┐                                       │
│  Metrics ──┼──> Unified Platform ───> Insights    │
│  Logs ─────┘                                       │
│                                                    │
│  No more juggling multiple tools!                 │
│                                                    │
└────────────────────────────────────────────────────┘
```

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
│   (OTel SDK)    │    │  (OTel SDK)     │    │  (OTel SDK)     │
└────────┬────────┘    └────────┬────────┘    └────────┬────────┘
         │                      │                       │
         └──────────────────────┼───────────────────────┘
                                │
                    OTLP (gRPC/HTTP)
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                    OPENTELEMETRY COLLECTOR                               │
│                  (SigNoz OTel Collector)                                 │
│  - Receive traces, metrics, logs                                        │
│  - Process, batch, enrich                                               │
│  - Export to SigNoz backend                                             │
└────────────────────────────────┬────────────────────────────────────────┘
                                 │
                                 ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                    SIGNOZ BACKEND SERVICES                               │
│  ┌──────────────────────────────────────────────────────────────────┐   │
│  │  Query Service (Go)                                              │   │
│  │  - REST API for UI                                               │   │
│  │  - Query traces, metrics, logs                                   │   │
│  │  - Aggregations & analytics                                      │   │
│  └──────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  ┌──────────────────────────────────────────────────────────────────┐   │
│  │  Alert Manager                                                    │   │
│  │  - Rule evaluation                                                │   │
│  │  - Notification dispatch                                          │   │
│  └──────────────────────────────────────────────────────────────────┘   │
└────────────────────────────────┬────────────────────────────────────────┘
                                 │
                                 ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                    CLICKHOUSE DATABASE                                   │
│  - Columnar storage engine                                              │
│  - High-performance queries                                             │
│  - Compression & indexing                                               │
│  - Tables: traces, metrics, logs                                        │
└────────────────────────────────┬────────────────────────────────────────┘
                                 │
                                 ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                    SIGNOZ FRONTEND (React)                               │
│  - Trace visualization                                                   │
│  - Metrics dashboards                                                    │
│  - Logs explorer                                                         │
│  - Service maps                                                          │
│  - Alert configuration                                                   │
└─────────────────────────────────────────────────────────────────────────┘
                                 │
                                 ▼
                          ┌──────────┐
                          │  Users   │
                          └──────────┘
```

### 2.2 Logical Architecture Layers

| Layer               | Components                    | Responsibility                       |
| ------------------- | ----------------------------- | ------------------------------------ |
| **Instrumentation** | OTel SDKs                     | Generate telemetry data              |
| **Collection**      | OTel Collector                | Receive, process, route data         |
| **Backend**         | Query Service, Alert Manager  | Business logic & API                 |
| **Storage**         | ClickHouse                    | Persist and index all telemetry      |
| **Visualization**   | React Frontend                | Display and analyze data             |

---

## 3. Core Components

### 3.1 OpenTelemetry Collector (SigNoz Distribution)

**Purpose**: Collect and process all telemetry data

**Key Features**:
- Receives OTLP (gRPC and HTTP)
- Supports Jaeger, Zipkin formats (legacy compatibility)
- Processors: batch, memory limiter, resource detection
- Exporters: ClickHouse (native), Kafka (optional)

**Configuration**:
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
    send_batch_size: 10000
  memory_limiter:
    check_interval: 1s
    limit_mib: 4000

exporters:
  clickhousemetricswrite:
    endpoint: tcp://clickhouse:9000
  clickhousetraceswrite:
    endpoint: tcp://clickhouse:9000
  clickhouselogswrite:
    endpoint: tcp://clickhouse:9000

service:
  pipelines:
    traces:
      receivers: [otlp]
      processors: [memory_limiter, batch]
      exporters: [clickhousetraceswrite]
    metrics:
      receivers: [otlp]
      processors: [memory_limiter, batch]
      exporters: [clickhousemetricswrite]
    logs:
      receivers: [otlp]
      processors: [memory_limiter, batch]
      exporters: [clickhouselogswrite]
```

### 3.2 ClickHouse Database

**Purpose**: High-performance storage for all telemetry

**Why ClickHouse?**
- **Columnar Storage**: 10-100x faster queries than row-based
- **Compression**: 10x better compression (saves storage costs)
- **Performance**: Billions of rows, sub-second queries
- **Real-time**: Inserts and queries happen simultaneously
- **Scalability**: Horizontal scaling via sharding

**Schema Design**:

#### Traces Table
```sql
CREATE TABLE signoz_traces.distributed_signoz_index_v2 (
    timestamp DateTime64(9) CODEC(DoubleDelta, LZ4),
    traceID String CODEC(ZSTD(1)),
    spanID String CODEC(ZSTD(1)),
    parentSpanID String CODEC(ZSTD(1)),
    serviceName LowCardinality(String) CODEC(ZSTD(1)),
    name LowCardinality(String) CODEC(ZSTD(1)),
    kind Int8 CODEC(T64, ZSTD(1)),
    durationNano UInt64 CODEC(T64, ZSTD(1)),
    statusCode Int16 CODEC(T64, ZSTD(1)),
    -- Attributes stored as Map
    stringTagMap Map(String, String) CODEC(ZSTD(1)),
    numberTagMap Map(String, Float64) CODEC(ZSTD(1)),
    -- Resource attributes
    resourceTagsMap Map(String, String) CODEC(ZSTD(1))
) ENGINE = Distributed('cluster', 'signoz_traces', 'signoz_index_v2', cityHash64(traceID));
```

#### Metrics Table
```sql
CREATE TABLE signoz_metrics.distributed_samples_v2 (
    metric_name LowCardinality(String) CODEC(ZSTD(1)),
    fingerprint UInt64 CODEC(DoubleDelta, ZSTD(1)),
    timestamp_ms Int64 CODEC(DoubleDelta, ZSTD(1)),
    value Float64 CODEC(Gorilla, ZSTD(1)),
    -- Labels
    labels Map(LowCardinality(String), String) CODEC(ZSTD(1))
) ENGINE = Distributed('cluster', 'signoz_metrics', 'samples_v2', fingerprint);
```

#### Logs Table
```sql
CREATE TABLE signoz_logs.distributed_logs (
    timestamp UInt64 CODEC(DoubleDelta, ZSTD(1)),
    observed_timestamp UInt64 CODEC(DoubleDelta, ZSTD(1)),
    id String CODEC(ZSTD(1)),
    trace_id String CODEC(ZSTD(1)),
    span_id String CODEC(ZSTD(1)),
    trace_flags UInt32,
    severity_text LowCardinality(String) CODEC(ZSTD(1)),
    severity_number UInt8,
    body String CODEC(ZSTD(1)),
    resources_string_key Array(String) CODEC(ZSTD(1)),
    resources_string_value Array(String) CODEC(ZSTD(1)),
    attributes_string_key Array(String) CODEC(ZSTD(1)),
    attributes_string_value Array(String) CODEC(ZSTD(1))
) ENGINE = Distributed('cluster', 'signoz_logs', 'logs', cityHash64(id));
```

**Performance Optimizations**:
- **CODEC**: Compression algorithms (DoubleDelta, ZSTD, Gorilla)
- **LowCardinality**: Optimized for columns with <10k unique values
- **Distributed Engine**: Sharding across multiple nodes
- **Indexes**: Primary key on timestamp for time-series queries

### 3.3 Query Service

**Purpose**: Backend API for frontend and external queries

**Technology**: Go (high performance)

**Responsibilities**:
- Query ClickHouse for traces, metrics, logs
- Aggregate and transform data
- Service dependency graph generation
- Alert rule evaluation
- Authentication & authorization

**API Endpoints**:
```
GET  /api/v1/traces/{traceId}
POST /api/v1/traces/search
GET  /api/v1/services
GET  /api/v1/service/{serviceName}/operations
POST /api/v1/metrics/query_range
POST /api/v1/logs/query
GET  /api/v1/serviceMap
```

### 3.4 Alert Manager

**Purpose**: Evaluate rules and send notifications

**Features**:
- Rule evaluation (PromQL-like queries)
- Multiple notification channels:
  - Slack
  - PagerDuty
  - Email
  - Webhooks
  - Microsoft Teams

**Alert Types**:
- Metrics-based (CPU > 80%, error rate > 5%)
- Trace-based (latency p99 > 1s)
- Log-based (error log count > 100/min)

**Example Alert Rule**:
```yaml
alert: HighErrorRate
condition: error_rate > 0.05
for: 5m
labels:
  severity: critical
annotations:
  summary: "High error rate detected"
  description: "Service {{ $labels.service_name }} has error rate > 5%"
notifications:
  - slack: #alerts
  - pagerduty: on-call
```

### 3.5 Frontend (React)

**Purpose**: User interface for observability

**Pages**:
1. **Services**: Service list, health, key metrics
2. **Traces**: Trace search, timeline visualization
3. **Metrics**: Custom dashboards, PromQL queries
4. **Logs**: Log explorer with filtering
5. **Service Map**: Dependency graph visualization
6. **Alerts**: Rule configuration and history
7. **Dashboard**: Customizable overview

**Key Features**:
- Trace flamegraph visualization
- Span details with attributes
- Service-to-service communication map
- Custom metric dashboards
- Log correlation with traces
- Dark mode 🌙

---

## 4. Data Flow

### 4.1 End-to-End Trace Flow

```
1. Client Request → API Gateway
   ├─ Generate Trace ID (if new)
   └─ Add traceparent header

2. Service A (Java) processes request
   ├─ OTel SDK creates spans
   ├─ Batch spans in memory
   └─ Export via OTLP/gRPC to OTel Collector

3. OTel Collector receives spans
   ├─ Process: batch, enrich
   └─ Export to ClickHouse

4. ClickHouse stores spans
   ├─ Write to signoz_index_v2 table
   ├─ Compress with ZSTD
   └─ Index by timestamp, traceID

5. User queries via SigNoz UI
   ├─ Frontend → Query Service
   ├─ Query Service → ClickHouse
   ├─ ClickHouse returns results (sub-second)
   └─ Frontend visualizes trace timeline
```

### 4.2 Metrics Flow

```
Application → OTel SDK (metrics) → OTel Collector
    → ClickHouse (samples_v2 table) → Query Service → Frontend
```

### 4.3 Logs Flow

```
Application → OTel SDK (logs) → OTel Collector
    → ClickHouse (logs table) → Query Service → Frontend

Correlation: Log trace_id links to traces table
```

### 4.4 Unified Query Example

**Scenario**: Investigate slow API request

```sql
-- 1. Find slow traces
SELECT traceID, durationNano, serviceName
FROM signoz_traces.distributed_signoz_index_v2
WHERE timestamp >= now() - INTERVAL 1 HOUR
  AND durationNano > 1000000000  -- > 1 second
ORDER BY durationNano DESC
LIMIT 10;

-- 2. Get related logs
SELECT timestamp, severity_text, body
FROM signoz_logs.distributed_logs
WHERE trace_id = '<traceID from above>'
ORDER BY timestamp;

-- 3. Get service metrics during that time
SELECT metric_name, avg(value) as avg_value
FROM signoz_metrics.distributed_samples_v2
WHERE metric_name LIKE 'http_server_%'
  AND timestamp_ms BETWEEN <start> AND <end>
  AND labels['service_name'] = 'api-service'
GROUP BY metric_name;
```

**Result**: Complete picture of the issue with traces, logs, and metrics!

---

## 5. Storage Architecture

### 5.1 ClickHouse Deployment Modes

#### Single Node (Development/Small)
```
┌──────────────────────┐
│   ClickHouse Node    │
│  - All shards        │
│  - All replicas      │
│  - Single point      │
└──────────────────────┘

Pros: Simple, low resource
Cons: No HA, limited scale
```

#### Cluster Mode (Production)
```
┌────────────────────────────────────────────────────────┐
│               ClickHouse Cluster                       │
│                                                        │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐   │
│  │  Shard 1    │  │  Shard 2    │  │  Shard 3    │   │
│  │  ┌────┐     │  │  ┌────┐     │  │  ┌────┐     │   │
│  │  │Rep1│     │  │  │Rep1│     │  │  │Rep1│     │   │
│  │  └────┘     │  │  └────┘     │  │  └────┘     │   │
│  │  ┌────┐     │  │  ┌────┐     │  │  ┌────┐     │   │
│  │  │Rep2│     │  │  │Rep2│     │  │  │Rep2│     │   │
│  │  └────┘     │  │  └────┘     │  │  └────┘     │   │
│  └─────────────┘  └─────────────┘  └─────────────┘   │
│                                                        │
│  Data distributed via hash(traceID)                   │
│  Each shard has 2 replicas for HA                     │
└────────────────────────────────────────────────────────┘

Pros: HA, horizontal scale, performance
Cons: Complex, more resources
```

### 5.2 Data Retention

**Configurable via TTL**:
```sql
-- Auto-delete data older than 7 days
ALTER TABLE signoz_traces.signoz_index_v2
MODIFY TTL timestamp + INTERVAL 7 DAY;

-- Different retention for different data
ALTER TABLE signoz_metrics.samples_v2
MODIFY TTL timestamp_ms + INTERVAL 15 DAY;

ALTER TABLE signoz_logs.logs
MODIFY TTL timestamp + INTERVAL 3 DAY;
```

**Cold Storage** (optional):
- Move old data to S3 via ClickHouse's S3 integration
- Keep recent data on hot storage (SSD)
- Archive old data on cold storage (S3)

---

## 6. Deployment Architecture

### 6.1 Kubernetes Deployment

```yaml
┌────────────────────────────────────────────────────────┐
│              Kubernetes Cluster                        │
│                                                        │
│  ┌──────────────────────────────────────────────────┐ │
│  │         Application Namespace                    │ │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────┐       │ │
│  │  │Service A │  │Service B │  │Service C │       │ │
│  │  │(OTel SDK)│  │(OTel SDK)│  │(OTel SDK)│       │ │
│  │  └────┬─────┘  └────┬─────┘  └────┬─────┘       │ │
│  │       │             │             │              │ │
│  │       └─────────────┼─────────────┘              │ │
│  │                     │                            │ │
│  └─────────────────────┼────────────────────────────┘ │
│                        │                              │
│  ┌─────────────────────┼────────────────────────────┐ │
│  │      SigNoz Namespace                            │ │
│  │                     │                            │ │
│  │        ┌────────────▼──────────────┐             │ │
│  │        │  OTel Collector           │             │ │
│  │        │  (DaemonSet or Deployment)│             │ │
│  │        └────────────┬──────────────┘             │ │
│  │                     │                            │ │
│  │        ┌────────────▼──────────────┐             │ │
│  │        │  ClickHouse StatefulSet   │             │ │
│  │        │  (3+ replicas)            │             │ │
│  │        └────────────┬──────────────┘             │ │
│  │                     │                            │ │
│  │        ┌────────────▼──────────────┐             │ │
│  │        │  Query Service Deployment │             │ │
│  │        │  (2+ replicas)            │             │ │
│  │        └────────────┬──────────────┘             │ │
│  │                     │                            │ │
│  │        ┌────────────▼──────────────┐             │ │
│  │        │  Frontend Deployment      │             │ │
│  │        │  (2+ replicas)            │             │ │
│  │        │  + Ingress                │             │ │
│  │        └───────────────────────────┘             │ │
│  └──────────────────────────────────────────────────┘ │
└────────────────────────────────────────────────────────┘
```

### 6.2 Resource Requirements

| Component             | CPU     | Memory | Storage | Replicas |
| --------------------- | ------- | ------ | ------- | -------- |
| OTel Collector        | 1 core  | 2Gi    | -       | 2-3      |
| ClickHouse            | 4 cores | 16Gi   | 500GB   | 3+       |
| Query Service         | 2 cores | 4Gi    | -       | 2        |
| Frontend              | 500m    | 1Gi    | -       | 2        |
| Alert Manager         | 500m    | 1Gi    | -       | 1        |
| **Total (minimum)**   | **20**  | **63** | **1.5TB** | **10**   |

**Scaling Guidelines**:
- **Small** (< 100 services): Single ClickHouse node
- **Medium** (100-500 services): 3-node ClickHouse cluster
- **Large** (500+ services): 6+ node ClickHouse cluster with sharding

---

## 7. Comparison with Jaeger

### 7.1 Architecture Differences

| Aspect                | SigNoz                               | Jaeger + Separate Tools              |
| --------------------- | ------------------------------------ | ------------------------------------ |
| **Components**        | All-in-one (5 components)            | Jaeger + Prometheus + Grafana + ELK  |
| **Complexity**        | Lower (integrated)                   | Higher (multiple tools)              |
| **Storage**           | ClickHouse (unified)                 | Elasticsearch (traces) + Prometheus  |
| **Query Performance** | Excellent (columnar)                 | Good (inverted index)                |
| **Unified Queries**   | Yes (traces + metrics + logs)        | No (separate tools)                  |
| **Alerting**          | Built-in                             | Prometheus Alertmanager              |
| **Dashboards**        | Built-in                             | Grafana                              |

### 7.2 Feature Comparison

```
┌────────────────────────────────────────────────────────────┐
│                  Feature Comparison                         │
├─────────────────────┬──────────────────┬───────────────────┤
│ Feature             │ SigNoz           │ Jaeger Stack      │
├─────────────────────┼──────────────────┼───────────────────┤
│ Distributed Tracing │ ✅ Excellent      │ ✅ Excellent       │
│ Metrics Monitoring  │ ✅ Built-in       │ ⚠️  Prometheus     │
│ Log Management      │ ✅ Built-in       │ ⚠️  ELK/Loki       │
│ Unified Dashboards  │ ✅ Yes            │ ❌ No (Grafana)    │
│ Alerting            │ ✅ Built-in       │ ⚠️  Alertmanager   │
│ Trace Flamegraphs   │ ✅ Yes            │ ✅ Yes             │
│ Service Map         │ ✅ Yes            │ ✅ Yes             │
│ Custom Dashboards   │ ✅ Yes            │ ✅ Grafana         │
│ Logs-Traces Link    │ ✅ Native         │ ⚠️  Manual         │
│ Query Language      │ ClickHouse SQL    │ Limited           │
│ Resource Usage      │ ⚠️  Higher        │ ✅ Lower           │
│ Learning Curve      │ ✅ Lower          │ ⚠️  Higher         │
└─────────────────────┴──────────────────┴───────────────────┘
```

---

## 8. Design Decisions

### 8.1 Why ClickHouse over Elasticsearch?

| Factor                | ClickHouse        | Elasticsearch     |
| --------------------- | ----------------- | ----------------- |
| **Query Speed**       | 10-100x faster    | Fast              |
| **Storage Cost**      | 10x compression   | Moderate          |
| **Write Performance** | Excellent         | Good              |
| **Analytics**         | Native (columnar) | Requires plugins  |
| **Operational Cost**  | Lower             | Higher            |

### 8.2 Why All-in-One vs Separate Tools?

**Advantages**:
- ✅ Single deployment
- ✅ Unified data model
- ✅ Correlated queries (traces ↔ metrics ↔ logs)
- ✅ One UI to learn
- ✅ Lower operational overhead

**Trade-offs**:
- ⚠️ More resource-intensive (includes everything)
- ⚠️ Less flexibility (can't mix-and-match tools)
- ⚠️ Younger project (less mature than Jaeger)

### 8.3 When to Choose SigNoz?

✅ **Choose SigNoz if:**
- You want unified observability (traces + metrics + logs)
- You prefer simpler operations (fewer tools)
- You need built-in dashboards and alerting
- Your team is small/medium (less specialization)
- You value fast query performance

⚠️ **Stick with Jaeger if:**
- You only need distributed tracing
- You already have Prometheus + Grafana setup
- You need maximum flexibility (best-of-breed tools)
- You have specialized teams for each tool
- You need proven, battle-tested stability

---

## 9. Pros & Cons Summary

### ✅ Strengths

1. **Unified Observability**: All telemetry in one place
2. **Performance**: ClickHouse provides fast queries
3. **Cost-Effective**: Better compression = lower storage costs
4. **Modern UI**: Beautiful, intuitive interface
5. **Built-in Alerting**: No need for separate tools
6. **OpenTelemetry Native**: Vendor-neutral instrumentation
7. **Active Development**: Frequent updates, responsive community

### ⚠️ Weaknesses

1. **Resource Intensive**: ClickHouse requires more resources
2. **Erlang Support**: Same as Jaeger (relies on OTel SDK maturity)
3. **Younger Project**: Less mature than Jaeger (founded 2021)
4. **Learning Curve**: ClickHouse SQL knowledge helpful
5. **Community Size**: Smaller than Jaeger/Prometheus
6. **Plugin Ecosystem**: Fewer integrations than established tools

---

## 10. Conclusion

SigNoz is an **excellent alternative to Jaeger** if you:
- Want a unified observability platform
- Need metrics and logs alongside traces
- Prefer simplicity over flexibility
- Value fast query performance
- Are comfortable with a newer project

For **pure distributed tracing**, Jaeger remains battle-tested and mature. But for **full-stack observability**, SigNoz provides a compelling all-in-one solution.

---

*Document Version: 1.0*
*Last Updated: November 7, 2025*
*Status: Ready for Review*

