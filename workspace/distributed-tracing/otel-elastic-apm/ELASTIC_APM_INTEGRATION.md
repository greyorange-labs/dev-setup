# OpenTelemetry + Elastic APM Integration Guide

## Table of Contents
- [Overview](#overview)
- [Architecture](#architecture)
- [Elastic APM vs SigNoz](#elastic-apm-vs-signoz)
- [Setup & Configuration](#setup--configuration)
  - [Elastic APM Server](#elastic-apm-server)
  - [Java Implementation](#java-implementation)
  - [Erlang Implementation](#erlang-implementation)
  - [Python Implementation](#python-implementation)
- [Configuration Reference](#configuration-reference)
- [Observability & Monitoring](#observability--monitoring)
- [Troubleshooting](#troubleshooting)
- [Migration from SigNoz](#migration-from-signoz)

---

## Overview

**Elastic APM** (Application Performance Monitoring) is part of the Elastic Stack (ELK) and provides distributed tracing, metrics, and logs in a unified platform. It supports **OpenTelemetry** as a data ingestion format via the APM Server.

### Key Components

```
┌─────────────────────────────────────────────────────────────────────────┐         ┌───────────────────────────────────────────┐
│ APPLICATION SERVER (Single JVM/BEAM VM Process)                         │         │ External Dependencies (Separate Servers): │
├─────────────────────────────────────────────────────────────────────────┤         ├───────────────────────────────────────────┤
│                                                                         │         │                                           │
│  Application Layer:                                                     │ ----->  │  ├─ PostgreSQL - Database queries         │
│                                                                         │         │  ├─ Kafka - Message streaming             │
│  ├─ Java Apps (API Gateway, Auth Service)                               │         │  └─ Redis - Caching                       │
│  └─ Erlang Apps (GM Core, Other Services)                               │         └───────────────────────────────────────────┘
│                          │                                              │
│                          ▼                                              │
│  OpenTelemetry SDK Layer (co-located in same process):                  │
│  ├─ Tracer Provider (creates tracers, manages context)                  │
│  ├─ Span Processors (BatchSpanProcessor, SimpleSpanProcessor)           │
│  └─ Exporters (OTLP/HTTP or OTLP/gRPC)                                  │
│                          │                                              │
└──────────────────────────┼──────────────────────────────────────────────┘
                           │
                           │ OTLP/HTTP or OTLP/gRPC
                           │ (port 8200 default)
                           ▼
┌─────────────────────────────────────────────────────────────────────────┐
│ ELASTIC APM SERVER (Separate Server)                                    │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  ├─ APM Server (port 8200) - Accepts OTLP data                          │
│  │   • Validates and transforms OTLP traces                             │
│  │   • Enriches with metadata                                           │
│  │   • Rate limiting & sampling                                         │
│  │                                                                      │
│  ├─ Elasticsearch - Document store for traces/metrics/logs              │
│  │   • Index: apm-*                                                     │
│  │   • Full-text search, aggregations                                   │
│  │                                                                      │
│  └─ Kibana (port 5601) - UI for visualization                           │
│      • APM app                                                          │
│      • Service maps, trace explorer                                     │
│      • Dashboards, alerts                                               │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

### Elastic Stack Components

| Component | Purpose | Default Port |
|-----------|---------|--------------|
| **APM Server** | Receives and processes telemetry data (OTLP, Elastic APM agents) | 8200 |
| **Elasticsearch** | Stores traces, metrics, logs | 9200 (HTTP), 9300 (Transport) |
| **Kibana** | Web UI for visualization and analysis | 5601 |

---

## Architecture

### Data Flow

```
Application Code
    │
    ├─ OpenTelemetry SDK (auto or manual instrumentation)
    │
    └─ OTLP Exporter (HTTP or gRPC)
           │
           ▼
      APM Server (port 8200)
           │
           ├─ Transform OTLP → Elastic format
           ├─ Enrich metadata
           └─ Apply sampling/filtering
           │
           ▼
      Elasticsearch (indices: apm-*)
           │
           ├─ apm-traces-*
           ├─ apm-metrics-*
           └─ apm-logs-*
           │
           ▼
      Kibana APM App (port 5601)
           │
           └─ Service Map, Trace Explorer, Dashboards
```

### OpenTelemetry Integration

Elastic APM Server supports **OpenTelemetry Protocol (OTLP)** natively:
- ✅ **OTLP/HTTP** (recommended): `http://apm-server:8200`
- ✅ **OTLP/gRPC**: `grpc://apm-server:8200`

**Advantage:** Use standard OpenTelemetry SDKs (same as SigNoz) - just change the exporter endpoint.

---

## Elastic APM vs SigNoz

| Feature | Elastic APM | SigNoz |
|---------|-------------|--------|
| **Backend Storage** | Elasticsearch (document store) | ClickHouse (columnar DB) |
| **Query Performance** | Good for complex queries, slower for large volumes | Optimized for high-volume time-series data |
| **Retention & Cost** | Higher storage cost, disk-heavy | Lower storage cost, more efficient compression |
| **UI/UX** | Mature Kibana APM app, highly customizable | Modern React UI, focused on observability |
| **Ecosystem** | Full ELK stack (logs, metrics, APM, SIEM) | Focused on APM + metrics |
| **Licensing** | Elastic License 2.0 (not OSI-approved), paid features | MIT License (fully open-source) |
| **OTLP Support** | ✅ Native support via APM Server | ✅ Native support via OTLP Receiver |
| **Setup Complexity** | Higher (ES cluster, Kibana, APM Server) | Lower (single binary or Docker Compose) |
| **Resource Usage** | Higher (ES: 2GB+ RAM per node) | Lower (ClickHouse: 1GB+ RAM) |
| **Alerting** | Built-in Kibana alerting (paid for advanced) | Built-in alerts (free) |
| **Service Maps** | ✅ Yes | ✅ Yes |
| **Sampling** | Head-based (APM Server) + tail-based (paid) | Head-based + tail-based (free) |

**When to Choose Elastic APM:**
- Already using ELK stack for logs/SIEM
- Need unified logs + metrics + traces
- Need advanced full-text search on traces
- Have budget for Elasticsearch infrastructure

**When to Choose SigNoz:**
- Pure observability focus (APM/metrics)
- Need cost-efficient storage at scale
- Want simpler setup and maintenance
- Prefer fully open-source solution

---

## Setup & Configuration

### Elastic APM Server

#### Docker Compose Setup

```yaml
# docker-compose.yml
version: '3.8'

services:
  elasticsearch:
    image: docker.elastic.co/elasticsearch/elasticsearch:8.11.0
    environment:
      - discovery.type=single-node
      - xpack.security.enabled=false
      - ES_JAVA_OPTS=-Xms2g -Xmx2g
    ports:
      - "9200:9200"
    volumes:
      - elasticsearch_data:/usr/share/elasticsearch/data
    networks:
      - elastic

  kibana:
    image: docker.elastic.co/kibana/kibana:8.11.0
    environment:
      - ELASTICSEARCH_HOSTS=http://elasticsearch:9200
    ports:
      - "5601:5601"
    depends_on:
      - elasticsearch
    networks:
      - elastic

  apm-server:
    image: docker.elastic.co/apm/apm-server:8.11.0
    ports:
      - "8200:8200"
    environment:
      - output.elasticsearch.hosts=["http://elasticsearch:9200"]
      - apm-server.rum.enabled=true
      - apm-server.kibana.enabled=true
      - apm-server.kibana.host=kibana:5601
      - apm-server.auth.anonymous.enabled=true
    depends_on:
      - elasticsearch
      - kibana
    networks:
      - elastic

volumes:
  elasticsearch_data:

networks:
  elastic:
    driver: bridge
```

#### APM Server Configuration

```yaml
# apm-server.yml
apm-server:
  host: "0.0.0.0:8200"

  # Enable OTLP endpoints
  otlp:
    grpc:
      enabled: true
    http:
      enabled: true

  # Sampling configuration
  sampling:
    keep_unsampled: false

  # Anonymous access (disable in production)
  auth:
    anonymous:
      enabled: true
      allow_agent: ['rum-js', 'js-base']
      allow_service: ['*']

output.elasticsearch:
  hosts: ["localhost:9200"]

  # Index settings
  indices:
    - index: "apm-%{[observer.version]}-traces"
      when.contains:
        processor.event: "transaction"
    - index: "apm-%{[observer.version]}-metrics"
      when.contains:
        processor.event: "metric"

kibana:
  enabled: true
  host: "localhost:5601"
```

#### Start Elastic Stack

```bash
# Start all services
docker-compose up -d

# Wait for Elasticsearch to be ready (30-60 seconds)
curl -X GET "localhost:9200/_cluster/health?wait_for_status=yellow&timeout=60s"

# Verify APM Server is running
curl http://localhost:8200/

# Access Kibana
open http://localhost:5601
```

---

## Java Implementation

### Option 1: OpenTelemetry Java Agent (Recommended)

Same as SigNoz setup, just change the OTLP endpoint.

#### Download Agent

```bash
cd /opt/otel
wget https://github.com/open-telemetry/opentelemetry-java-instrumentation/releases/latest/download/opentelemetry-javaagent.jar
```

#### Application Startup

```bash
java \
  -javaagent:/opt/otel/opentelemetry-javaagent.jar \
  -Dotel.service.name=api-gateway \
  -Dotel.resource.attributes=service.version=1.0.0,deployment.environment=production \
  -Dotel.traces.exporter=otlp \
  -Dotel.exporter.otlp.protocol=http/protobuf \
  -Dotel.exporter.otlp.endpoint=http://apm-server:8200 \
  -Dotel.exporter.otlp.compression=gzip \
  -Dotel.traces.sampler=parentbased_traceidratio \
  -Dotel.traces.sampler.arg=0.1 \
  -jar your-application.jar
```

#### Key Configuration for Elastic APM

```properties
# OTLP endpoint (Elastic APM Server)
otel.exporter.otlp.endpoint=http://apm-server:8200
otel.exporter.otlp.protocol=http/protobuf

# Service identification
otel.service.name=api-gateway
otel.resource.attributes=service.version=1.0.0,deployment.environment=production

# Sampling (10% of traces)
otel.traces.sampler=parentbased_traceidratio
otel.traces.sampler.arg=0.1

# Compression
otel.exporter.otlp.compression=gzip
```

#### Systemd Service Example

```ini
# /etc/systemd/system/api-gateway.service
[Unit]
Description=API Gateway Service
After=network.target

[Service]
Type=simple
User=appuser
WorkingDirectory=/opt/api-gateway

Environment="JAVA_OPTS=-Xms512m -Xmx2g"
Environment="OTEL_SERVICE_NAME=api-gateway"
Environment="OTEL_RESOURCE_ATTRIBUTES=service.version=1.0.0,deployment.environment=production"
Environment="OTEL_TRACES_EXPORTER=otlp"
Environment="OTEL_EXPORTER_OTLP_ENDPOINT=http://apm-server:8200"
Environment="OTEL_EXPORTER_OTLP_PROTOCOL=http/protobuf"
Environment="OTEL_TRACES_SAMPLER=parentbased_traceidratio"
Environment="OTEL_TRACES_SAMPLER_ARG=0.1"

ExecStart=/usr/bin/java \
  -javaagent:/opt/otel/opentelemetry-javaagent.jar \
  $JAVA_OPTS \
  -jar /opt/api-gateway/api-gateway.jar

Restart=always
RestartSec=10

[Install]
WantedBy=multi-user.target
```

### Option 2: Elastic APM Java Agent (Native)

Alternative to OpenTelemetry if you prefer Elastic's native agent.

```bash
# Download Elastic APM Agent
wget -O /opt/elastic/elastic-apm-agent.jar https://repo1.maven.org/maven2/co/elastic/apm/elastic-apm-agent/1.42.0/elastic-apm-agent-1.42.0.jar

# Start application
java \
  -javaagent:/opt/elastic/elastic-apm-agent.jar \
  -Delastic.apm.service_name=api-gateway \
  -Delastic.apm.server_url=http://apm-server:8200 \
  -Delastic.apm.environment=production \
  -Delastic.apm.sample_rate=0.1 \
  -jar your-application.jar
```

**Note:** Elastic APM agent uses proprietary format, not OpenTelemetry. For portability, prefer OpenTelemetry.

---

## Erlang Implementation

### Setup OpenTelemetry Erlang SDK

Same as SigNoz, change exporter endpoint to Elastic APM Server.

#### Add Dependencies

```erlang
%% rebar.config
{deps, [
    {opentelemetry_api, "~> 1.3"},
    {opentelemetry, "~> 1.4"},
    {opentelemetry_exporter, "~> 1.7"}
]}.
```

#### Configuration

```erlang
%% sys.config or releases/X.Y.Z/sys.config
[
 {opentelemetry,
  [
   {resource, #{
       'service.name' => <<"gm-core">>,
       'service.version' => <<"2.0.0">>,
       'deployment.environment' => <<"production">>
   }},

   {span_processor, batch},

   {traces_exporter, {opentelemetry_exporter, #{
       endpoints => ["http://apm-server:8200"],
       protocol => http_protobuf,
       compression => gzip
   }}},

   {sampler, {parent_based, #{
       root => {trace_id_ratio_based, 0.1}
   }}}
  ]},

 {opentelemetry_exporter,
  [
   {otlp_endpoint, "http://apm-server:8200"},
   {otlp_protocol, http_protobuf},
   {otlp_compression, gzip}
  ]}
].
```

#### Application Startup

```erlang
%% In your application start callback
-module(gm_core_app).
-behaviour(application).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").

start(_StartType, _StartArgs) ->
    %% Initialize OpenTelemetry
    opentelemetry:start(),

    %% Start your supervision tree
    gm_core_sup:start_link().

stop(_State) ->
    ok.
```

#### Manual Instrumentation

```erlang
%% Example: Trace HTTP request handling
-module(gm_core_handler).
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

handle_request(Req) ->
    %% Extract trace context from HTTP headers
    Ctx = otel_propagator_text_map:extract(maps:to_list(cowboy_req:headers(Req))),
    otel_ctx:attach(Ctx),

    %% Create span
    ?with_span(<<"handle_request">>, #{attributes => #{
        'http.method' => cowboy_req:method(Req),
        'http.target' => cowboy_req:path(Req)
    }}, fun(_SpanCtx) ->
        %% Business logic
        Result = process_request(Req),

        %% Add span attributes
        otel_span:set_attribute('http.status_code', 200),

        %% Return response with propagated context
        Headers = otel_propagator_text_map:inject([]),
        {ok, Result, Headers}
    end).

process_request(Req) ->
    %% Nested span for database query
    ?with_span(<<"db_query">>, #{attributes => #{
        'db.system' => 'postgresql',
        'db.operation' => 'SELECT'
    }}, fun(_SpanCtx) ->
        %% Database call
        {ok, Result} = db:query("SELECT * FROM users WHERE id = $1", [123]),
        Result
    end).
```

#### Context Propagation (HTTP Client)

```erlang
%% When making outbound HTTP requests
make_http_request(Url) ->
    %% Inject current trace context into headers
    Headers = otel_propagator_text_map:inject([
        {"content-type", "application/json"}
    ]),

    httpc:request(get, {Url, Headers}, [], []).
```

---

## Python Implementation

### Setup OpenTelemetry Python SDK

#### Install Dependencies

```bash
pip install \
    opentelemetry-api \
    opentelemetry-sdk \
    opentelemetry-exporter-otlp-proto-http \
    opentelemetry-instrumentation-requests
```

#### Configuration

```python
# app.py
import os
from opentelemetry import trace
from opentelemetry.sdk.trace import TracerProvider
from opentelemetry.sdk.trace.export import BatchSpanProcessor
from opentelemetry.exporter.otlp.proto.http.trace_exporter import OTLPSpanExporter
from opentelemetry.sdk.resources import Resource
from opentelemetry.instrumentation.requests import RequestsInstrumentor

# Configure resource attributes
resource = Resource.create({
    "service.name": "python-web-client",
    "service.version": "1.0.0",
    "deployment.environment": os.getenv("ENVIRONMENT", "production")
})

# Create tracer provider
tracer_provider = TracerProvider(resource=resource)

# Configure OTLP exporter (Elastic APM Server)
otlp_exporter = OTLPSpanExporter(
    endpoint="http://apm-server:8200/v1/traces",  # Note: /v1/traces path
    compression="gzip"
)

# Add batch span processor
span_processor = BatchSpanProcessor(
    otlp_exporter,
    max_queue_size=2048,
    schedule_delay_millis=5000,
    max_export_batch_size=512
)
tracer_provider.add_span_processor(span_processor)

# Set global tracer provider
trace.set_tracer_provider(tracer_provider)

# Auto-instrument requests library
RequestsInstrumentor().instrument()

# Get tracer for manual instrumentation
tracer = trace.get_tracer(__name__)
```

#### Manual Instrumentation

```python
import requests
from opentelemetry import trace
from opentelemetry.trace import Status, StatusCode

tracer = trace.get_tracer(__name__)

def fetch_user_data(user_id: int):
    # Create manual span
    with tracer.start_as_current_span(
        "fetch_user_data",
        attributes={
            "user.id": user_id,
            "operation": "fetch"
        }
    ) as span:
        try:
            # HTTP request (auto-instrumented)
            response = requests.get(f"http://api-gateway/users/{user_id}")
            response.raise_for_status()

            # Add attributes
            span.set_attribute("http.status_code", response.status_code)
            span.set_attribute("user.found", True)

            return response.json()

        except requests.RequestException as e:
            # Record exception
            span.record_exception(e)
            span.set_status(Status(StatusCode.ERROR, str(e)))
            raise
```

#### Environment Variables

```bash
# .env or systemd environment
export OTEL_SERVICE_NAME=python-web-client
export OTEL_RESOURCE_ATTRIBUTES=service.version=1.0.0,deployment.environment=production
export OTEL_EXPORTER_OTLP_ENDPOINT=http://apm-server:8200
export OTEL_EXPORTER_OTLP_PROTOCOL=http/protobuf
export OTEL_TRACES_SAMPLER=parentbased_traceidratio
export OTEL_TRACES_SAMPLER_ARG=0.1
```

---

## Configuration Reference

### OTLP Endpoints for Elastic APM

| Protocol | Endpoint | Path |
|----------|----------|------|
| **OTLP/HTTP** (traces) | `http://apm-server:8200` | `/v1/traces` (auto-appended by SDK) |
| **OTLP/gRPC** (traces) | `grpc://apm-server:8200` | N/A |

**Important:**
- Java/Erlang: Use base URL `http://apm-server:8200` (SDK adds `/v1/traces`)
- Python: Some exporters need full path `http://apm-server:8200/v1/traces`

### Sampling Strategies

| Sampler | Configuration | Use Case |
|---------|---------------|----------|
| **Always On** | `always_on` | Development, debugging |
| **Always Off** | `always_off` | Disable tracing |
| **Trace ID Ratio** | `traceidratio` (e.g., `0.1` = 10%) | Production (fixed sampling) |
| **Parent Based** | `parentbased_traceidratio` | Production (respect upstream decisions) |

**Java:**
```bash
-Dotel.traces.sampler=parentbased_traceidratio
-Dotel.traces.sampler.arg=0.1
```

**Erlang:**
```erlang
{sampler, {parent_based, #{root => {trace_id_ratio_based, 0.1}}}}
```

**Python:**
```python
from opentelemetry.sdk.trace.sampling import ParentBasedTraceIdRatioBased
tracer_provider = TracerProvider(
    resource=resource,
    sampler=ParentBasedTraceIdRatioBased(0.1)
)
```

### Resource Attributes (Metadata)

Standard attributes to add to all traces:

```bash
# Service identification
service.name=api-gateway
service.version=1.0.0
service.instance.id=api-gateway-pod-1

# Deployment context
deployment.environment=production
deployment.cluster=us-west-2

# Infrastructure
host.name=app-server-01
k8s.pod.name=api-gateway-7d8c9-abc123
k8s.namespace.name=default
```

---

## Observability & Monitoring

### Access Kibana APM

1. Open Kibana: `http://localhost:5601`
2. Navigate to **Observability** → **APM**
3. Select service from dropdown

### Key Features

#### Service Map

Visual representation of service dependencies:
- **Nodes:** Services
- **Edges:** Request flows
- **Colors:** Health status (green = healthy, red = errors)

#### Trace Explorer

Search and filter traces:
```
# Filter by service
service.name: "api-gateway"

# Filter by status
http.status_code: 500

# Filter by duration
transaction.duration.us > 1000000

# Combine filters
service.name: "gm-core" AND trace.id: "abc123..."
```

#### Service Metrics

- **Throughput:** Requests per minute
- **Latency:** p50, p95, p99 percentiles
- **Error Rate:** % of failed requests
- **Transaction Types:** HTTP, gRPC, background jobs

### Kibana Query Language (KQL)

```bash
# Find slow transactions
transaction.duration.us > 5000000

# Find errors in specific service
service.name: "api-gateway" AND error.message: *

# Find traces with specific attribute
transaction.custom.user_id: 12345

# Date range
@timestamp >= "2024-01-01" AND @timestamp < "2024-01-02"
```

---

## Troubleshooting

### Common Issues

#### 1. Traces Not Appearing in Kibana

**Check APM Server logs:**
```bash
docker logs apm-server

# Look for errors like:
# "failed to index document"
# "connection refused"
```

**Verify APM Server is receiving data:**
```bash
curl http://localhost:8200/

# Expected response:
# {
#   "build_date": "2023-11-01",
#   "build_sha": "abc123",
#   "version": "8.11.0"
# }
```

**Check Elasticsearch indices:**
```bash
curl http://localhost:9200/_cat/indices/apm-*

# Expected output:
# green open apm-8.11.0-traces-2024.01.01
# green open apm-8.11.0-metrics-2024.01.01
```

**Test OTLP endpoint:**
```bash
# From application server
curl -v -X POST http://apm-server:8200/v1/traces \
  -H "Content-Type: application/x-protobuf" \
  --data-binary @test-span.pb
```

#### 2. High Elasticsearch Disk Usage

**Check index sizes:**
```bash
curl http://localhost:9200/_cat/indices/apm-*?v&s=store.size:desc
```

**Solutions:**
- Reduce retention period
- Increase sampling rate (fewer traces)
- Enable ILM (Index Lifecycle Management)

**Configure ILM:**
```bash
# Delete traces older than 7 days
PUT _ilm/policy/apm-traces-policy
{
  "policy": {
    "phases": {
      "hot": {
        "actions": {
          "rollover": {
            "max_size": "50GB",
            "max_age": "1d"
          }
        }
      },
      "delete": {
        "min_age": "7d",
        "actions": {
          "delete": {}
        }
      }
    }
  }
}
```

#### 3. Context Not Propagating

**Verify headers in HTTP requests:**

Java (OTel agent automatically injects):
```
traceparent: 00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01
```

Erlang (manual injection):
```erlang
Headers = otel_propagator_text_map:inject([]),
%% Verify "traceparent" header is present
io:format("Headers: ~p~n", [Headers]).
```

Python (auto with RequestsInstrumentor):
```python
import requests
from opentelemetry.instrumentation.requests import RequestsInstrumentor

RequestsInstrumentor().instrument()  # Must be called once
response = requests.get("http://api-gateway/users/123")
```

#### 4. High CPU/Memory in Application

**Check span export rate:**

Java:
```bash
# Add JMX metrics
-Dcom.sun.management.jmxremote
-Dcom.sun.management.jmxremote.port=9010
-Dcom.sun.management.jmxremote.authenticate=false
-Dcom.sun.management.jmxremote.ssl=false

# Check with jconsole or JMX tool
# Look for: otel.spans.exported.count
```

Erlang:
```erlang
%% Check batch processor queue size
{ok, QueueSize} = opentelemetry:get_tracer_provider_info(queue_size),
io:format("Queue size: ~p~n", [QueueSize]).
```

**Solutions:**
- Increase sampling rate (e.g., 0.01 = 1%)
- Increase batch export interval
- Use SimpleSpanProcessor only in dev

---

## Migration from SigNoz

### Configuration Changes

| Setting | SigNoz | Elastic APM |
|---------|--------|-------------|
| **Endpoint** | `http://signoz:4318` | `http://apm-server:8200` |
| **Protocol** | OTLP/HTTP | OTLP/HTTP |
| **UI URL** | `http://signoz:3301` | `http://kibana:5601` |
| **Backend** | ClickHouse | Elasticsearch |

### Java Migration

**Before (SigNoz):**
```bash
-Dotel.exporter.otlp.endpoint=http://signoz:4318
```

**After (Elastic APM):**
```bash
-Dotel.exporter.otlp.endpoint=http://apm-server:8200
```

### Erlang Migration

**Before (SigNoz):**
```erlang
{traces_exporter, {opentelemetry_exporter, #{
    endpoints => ["http://signoz:4318"]
}}}
```

**After (Elastic APM):**
```erlang
{traces_exporter, {opentelemetry_exporter, #{
    endpoints => ["http://apm-server:8200"]
}}}
```

### Python Migration

**Before (SigNoz):**
```python
otlp_exporter = OTLPSpanExporter(
    endpoint="http://signoz:4318/v1/traces"
)
```

**After (Elastic APM):**
```python
otlp_exporter = OTLPSpanExporter(
    endpoint="http://apm-server:8200/v1/traces"
)
```

### Data Migration

**Note:** Elasticsearch and ClickHouse use different storage formats. Direct migration is not possible.

**Options:**
1. **Parallel Run:** Run both SigNoz and Elastic APM for overlap period
2. **Export/Import:** Export SigNoz traces via API, transform, import to Elasticsearch (complex, not recommended)
3. **Clean Cut:** Accept historical data loss, start fresh

### Feature Parity

| Feature | SigNoz | Elastic APM | Notes |
|---------|--------|-------------|-------|
| **Service Maps** | ✅ | ✅ | Similar functionality |
| **Trace Search** | ✅ | ✅ | Elasticsearch more powerful text search |
| **Dashboards** | ✅ | ✅ | Kibana more mature |
| **Alerts** | ✅ (free) | ✅ (basic free, advanced paid) | Elastic requires license for advanced |
| **Tail-based Sampling** | ✅ (free) | ⚠️ (paid only) | SigNoz advantage |
| **Logs Integration** | ✅ | ✅ | Elastic stronger (full ELK) |
| **Metrics** | ✅ | ✅ | Both support metrics |
| **Cost** | Lower | Higher | Elasticsearch more resource-intensive |

---

## Performance & Resource Requirements

### Elastic APM Server

| Environment | CPU | RAM | Disk I/O |
|-------------|-----|-----|----------|
| **Dev** | 0.5 cores | 512 MB | Low |
| **Staging** | 1 core | 1 GB | Medium |
| **Production** | 2-4 cores | 2-4 GB | High |

### Elasticsearch

| Environment | Nodes | CPU per Node | RAM per Node | Disk per Node |
|-------------|-------|--------------|--------------|---------------|
| **Dev** | 1 | 1 core | 2 GB | 20 GB |
| **Staging** | 1-3 | 2 cores | 4 GB | 100 GB |
| **Production** | 3+ | 4+ cores | 8+ GB | 500+ GB SSD |

**Note:** Elasticsearch is significantly more resource-intensive than ClickHouse (used by SigNoz).

### Application Overhead

Same as SigNoz (negligible with proper sampling):
- **Java:** <1% CPU, <50 MB RAM
- **Erlang:** <1% CPU, <20 MB RAM
- **Python:** <2% CPU, <30 MB RAM

---

## Summary

### Quick Start Checklist

- [ ] Deploy Elastic Stack (Elasticsearch, Kibana, APM Server)
- [ ] Configure APM Server to accept OTLP data
- [ ] Update application OTLP endpoint to `http://apm-server:8200`
- [ ] Set sampling rate (e.g., 10% = `0.1`)
- [ ] Verify traces appear in Kibana APM app
- [ ] Configure alerts and dashboards
- [ ] Set up ILM for trace retention

### Key Differences from SigNoz

- **Storage:** Elasticsearch (document store) vs ClickHouse (columnar)
- **UI:** Kibana (port 5601) vs SigNoz UI (port 3301)
- **Endpoint:** APM Server (port 8200) vs SigNoz Collector (port 4318)
- **Resources:** Higher (ES cluster) vs Lower (ClickHouse)
- **Ecosystem:** Full ELK stack vs focused APM

### When to Use Elastic APM

✅ **Choose Elastic APM if:**
- Already invested in ELK stack
- Need unified logs + metrics + traces
- Need advanced full-text search
- Have budget for infrastructure

❌ **Avoid Elastic APM if:**
- Limited infrastructure budget
- Want simpler setup
- Only need APM (no logs/SIEM)
- Prefer fully open-source (MIT)

---

## Additional Resources

- [Elastic APM Documentation](https://www.elastic.co/guide/en/apm/guide/current/index.html)
- [OpenTelemetry with Elastic](https://www.elastic.co/guide/en/apm/guide/current/open-telemetry.html)
- [Kibana APM UI Guide](https://www.elastic.co/guide/en/kibana/current/apm.html)
- [Elasticsearch Sizing Guide](https://www.elastic.co/guide/en/elasticsearch/reference/current/size-your-shards.html)

---

**Last Updated:** January 2024

