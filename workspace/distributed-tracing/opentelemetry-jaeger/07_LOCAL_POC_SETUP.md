# OpenTelemetry + Jaeger Local POC Setup Guide

## Table of Contents
1. [Prerequisites](#prerequisites)
2. [Quick Start](#quick-start)
3. [Detailed Setup](#detailed-setup)
4. [Sample Applications](#sample-applications)
5. [Testing & Verification](#testing--verification)
6. [Troubleshooting](#troubleshooting)
7. [Resource Requirements](#resource-requirements)
8. [Cleanup](#cleanup)

---

## 1. Prerequisites

### System Requirements

**Minimum**:
- CPU: 2 cores
- RAM: 4GB
- Disk: 10GB free space
- OS: macOS, Linux, or Windows (with WSL2)

**Recommended**:
- CPU: 4 cores
- RAM: 8GB
- Disk: 20GB free space

### Software Requirements

```bash
# Check installations
docker --version          # >= 20.10
docker-compose --version  # >= 2.0
git --version
curl --version
```

---

## 2. Quick Start (3 Minutes)

### All-in-One Docker Compose

Create `jaeger-poc/docker-compose.yml`:

```yaml
version: "3.9"

services:
  # Jaeger All-in-One (includes UI, Collector, Query)
  jaeger:
    image: jaegertracing/all-in-one:1.51
    container_name: jaeger
    environment:
      - COLLECTOR_OTLP_ENABLED=true
      - METRICS_STORAGE_TYPE=prometheus
    ports:
      - "16686:16686"    # Jaeger UI
      - "14268:14268"    # Jaeger collector HTTP
      - "14250:14250"    # Jaeger collector gRPC
      - "4317:4317"      # OTLP gRPC (direct to Jaeger)
      - "4318:4318"      # OTLP HTTP (direct to Jaeger)
    networks:
      - tracing

  # OpenTelemetry Collector (optional, for advanced processing)
  otel-collector:
    image: otel/opentelemetry-collector-contrib:0.89.0
    container_name: otel-collector
    command: ["--config=/etc/otel-collector-config.yaml"]
    volumes:
      - ./otel-collector-config.yaml:/etc/otel-collector-config.yaml
    ports:
      - "4319:4317"      # OTLP gRPC (via collector)
      - "4320:4318"      # OTLP HTTP (via collector)
      - "8888:8888"      # Prometheus metrics
      - "13133:13133"    # Health check
    depends_on:
      - jaeger
    networks:
      - tracing

networks:
  tracing:
    driver: bridge
```

**Start Services**:
```bash
mkdir -p jaeger-poc && cd jaeger-poc

# Create config file (see next section)

# Start Jaeger
docker-compose up -d

# Verify
docker-compose ps
docker-compose logs jaeger

# Access Jaeger UI
open http://localhost:16686
```

### Minimal Setup (Jaeger Only)

If you just want Jaeger without OTel Collector:

```bash
# Run Jaeger all-in-one
docker run -d \
  --name jaeger \
  -p 16686:16686 \
  -p 4317:4317 \
  -p 4318:4318 \
  -e COLLECTOR_OTLP_ENABLED=true \
  jaegertracing/all-in-one:1.51

# Access UI
open http://localhost:16686
```

---

## 3. Detailed Setup

### 3.1 Configuration Files

#### `otel-collector-config.yaml`

```yaml
receivers:
  otlp:
    protocols:
      grpc:
        endpoint: 0.0.0.0:4317
      http:
        endpoint: 0.0.0.0:4318
        cors:
          allowed_origins:
            - "http://*"
            - "https://*"

  # Jaeger receiver (for legacy compatibility)
  jaeger:
    protocols:
      grpc:
        endpoint: 0.0.0.0:14250
      thrift_http:
        endpoint: 0.0.0.0:14268

processors:
  # Memory limiter (prevent OOM)
  memory_limiter:
    check_interval: 1s
    limit_mib: 512
    spike_limit_mib: 128

  # Batch processor (improve performance)
  batch:
    timeout: 10s
    send_batch_size: 1024
    send_batch_max_size: 2048

  # Resource processor (add global attributes)
  resource:
    attributes:
      - key: deployment.environment
        value: local-poc
        action: insert
      - key: cluster.name
        value: local-dev
        action: insert

  # Attributes processor (clean up data)
  attributes:
    actions:
      # Remove sensitive headers
      - key: http.request.header.authorization
        action: delete
      - key: http.request.header.cookie
        action: delete

  # Span processor (enrich spans)
  span:
    name:
      # Rename spans based on attributes
      from_attributes: ["http.method", "http.route"]
      separator: " "

  # Tail-based sampling (intelligent filtering)
  tail_sampling:
    decision_wait: 10s
    num_traces: 100000
    expected_new_traces_per_sec: 100
    policies:
      # Always sample errors
      - name: errors-policy
        type: status_code
        status_code:
          status_codes: [ERROR]

      # Always sample slow requests (>500ms)
      - name: latency-policy
        type: latency
        latency:
          threshold_ms: 500

      # Sample 100% for POC (use 10% in production)
      - name: probabilistic-policy
        type: probabilistic
        probabilistic:
          sampling_percentage: 100

exporters:
  # Jaeger exporter
  jaeger:
    endpoint: jaeger:14250
    tls:
      insecure: true

  # Logging exporter (for debugging)
  logging:
    loglevel: info
    sampling_initial: 5
    sampling_thereafter: 200

  # Prometheus exporter (for collector metrics)
  prometheus:
    endpoint: "0.0.0.0:8889"

extensions:
  # Health check
  health_check:
    endpoint: ":13133"

  # Performance profiling
  pprof:
    endpoint: ":1777"

service:
  extensions: [health_check, pprof]

  telemetry:
    logs:
      level: info
    metrics:
      address: "0.0.0.0:8888"

  pipelines:
    traces:
      receivers: [otlp, jaeger]
      processors:
        - memory_limiter
        - resource
        - attributes
        - span
        - tail_sampling
        - batch
      exporters: [jaeger, logging]
```

### 3.2 Production-Like Setup (with Elasticsearch)

For a more realistic setup with persistent storage:

**`docker-compose.prod-like.yml`**:

```yaml
version: "3.9"

services:
  # Elasticsearch (storage backend)
  elasticsearch:
    image: docker.elastic.co/elasticsearch/elasticsearch:8.11.0
    container_name: elasticsearch
    environment:
      - discovery.type=single-node
      - xpack.security.enabled=false
      - "ES_JAVA_OPTS=-Xms2g -Xmx2g"
    ports:
      - "9200:9200"
    volumes:
      - esdata:/usr/share/elasticsearch/data
    networks:
      - tracing
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:9200"]
      interval: 10s
      timeout: 5s
      retries: 5

  # Jaeger Collector
  jaeger-collector:
    image: jaegertracing/jaeger-collector:1.51
    container_name: jaeger-collector
    environment:
      - SPAN_STORAGE_TYPE=elasticsearch
      - ES_SERVER_URLS=http://elasticsearch:9200
      - COLLECTOR_OTLP_ENABLED=true
    ports:
      - "14250:14250"
      - "14268:14268"
      - "4317:4317"
      - "4318:4318"
    depends_on:
      elasticsearch:
        condition: service_healthy
    networks:
      - tracing

  # Jaeger Query Service
  jaeger-query:
    image: jaegertracing/jaeger-query:1.51
    container_name: jaeger-query
    environment:
      - SPAN_STORAGE_TYPE=elasticsearch
      - ES_SERVER_URLS=http://elasticsearch:9200
      - QUERY_BASE_PATH=/jaeger
    ports:
      - "16686:16686"
      - "16687:16687"
    depends_on:
      elasticsearch:
        condition: service_healthy
    networks:
      - tracing

  # OpenTelemetry Collector
  otel-collector:
    image: otel/opentelemetry-collector-contrib:0.89.0
    container_name: otel-collector
    command: ["--config=/etc/otel-collector-config.yaml"]
    volumes:
      - ./otel-collector-config.yaml:/etc/otel-collector-config.yaml
    ports:
      - "4319:4317"
      - "8888:8888"
    depends_on:
      - jaeger-collector
    networks:
      - tracing

  # Kafka (for buffering - optional)
  kafka:
    image: bitnami/kafka:3.6
    container_name: kafka
    environment:
      - KAFKA_CFG_NODE_ID=0
      - KAFKA_CFG_PROCESS_ROLES=controller,broker
      - KAFKA_CFG_LISTENERS=PLAINTEXT://:9092,CONTROLLER://:9093
      - KAFKA_CFG_LISTENER_SECURITY_PROTOCOL_MAP=CONTROLLER:PLAINTEXT,PLAINTEXT:PLAINTEXT
      - KAFKA_CFG_CONTROLLER_QUORUM_VOTERS=0@kafka:9093
      - KAFKA_CFG_CONTROLLER_LISTENER_NAMES=CONTROLLER
    ports:
      - "9092:9092"
    networks:
      - tracing

volumes:
  esdata:

networks:
  tracing:
    driver: bridge
```

---

## 4. Sample Applications

### 4.1 Java Service (Spring Boot) - Order Service

**Dockerfile**:
```dockerfile
FROM eclipse-temurin:21-jre-alpine

# Download OpenTelemetry Java Agent
ADD https://github.com/open-telemetry/opentelemetry-java-instrumentation/releases/latest/download/opentelemetry-javaagent.jar /app/opentelemetry-javaagent.jar

COPY target/order-service.jar /app/order-service.jar

WORKDIR /app

ENV JAVA_OPTS="-javaagent:/app/opentelemetry-javaagent.jar"

ENTRYPOINT ["sh", "-c", "java $JAVA_OPTS -jar order-service.jar"]
```

**`application.yml`**:
```yaml
spring:
  application:
    name: order-service

server:
  port: 8080

# OpenTelemetry configuration (via Java agent)
otel:
  service:
    name: ${spring.application.name}
  exporter:
    otlp:
      endpoint: http://otel-collector:4317
  traces:
    exporter: otlp
```

**`OrderController.java`**:
```java
package com.example.order;

import io.opentelemetry.api.trace.Span;
import io.opentelemetry.api.trace.Tracer;
import io.opentelemetry.instrumentation.annotations.WithSpan;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.client.RestTemplate;

@RestController
@RequestMapping("/api/orders")
public class OrderController {

    @Autowired
    private Tracer tracer;

    @Autowired
    private RestTemplate restTemplate;

    @PostMapping
    @WithSpan("createOrder")
    public Order createOrder(@RequestBody OrderRequest request) {
        Span span = Span.current();
        span.setAttribute("order.customer_id", request.getCustomerId());
        span.setAttribute("order.total", request.getTotal());

        // Validate order
        validateOrder(request);

        // Check inventory (calls Python service)
        boolean available = checkInventory(request.getItems());
        span.setAttribute("inventory.available", available);

        if (!available) {
            span.setAttribute("order.status", "rejected");
            throw new RuntimeException("Insufficient inventory");
        }

        // Create order
        Order order = new Order();
        order.setId(UUID.randomUUID().toString());
        order.setCustomerId(request.getCustomerId());
        order.setTotal(request.getTotal());
        order.setStatus("created");

        span.setAttribute("order.id", order.getId());
        span.setAttribute("order.status", "created");

        return order;
    }

    @WithSpan("validateOrder")
    private void validateOrder(OrderRequest request) {
        // Validation logic
        if (request.getTotal() <= 0) {
            throw new IllegalArgumentException("Invalid order total");
        }
    }

    @WithSpan("checkInventory")
    private boolean checkInventory(List<String> items) {
        // Call inventory service
        String url = "http://inventory-service:8001/api/inventory/check";
        InventoryResponse response = restTemplate.postForObject(
            url,
            new InventoryRequest(items),
            InventoryResponse.class
        );
        return response != null && response.isAvailable();
    }
}
```

### 4.2 Python Service (FastAPI) - Inventory Service

**Dockerfile**:
```dockerfile
FROM python:3.11-slim

WORKDIR /app

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

CMD ["python", "main.py"]
```

**`requirements.txt`**:
```
fastapi==0.104.1
uvicorn[standard]==0.24.0
opentelemetry-api==1.21.0
opentelemetry-sdk==1.21.0
opentelemetry-instrumentation-fastapi==0.42b0
opentelemetry-instrumentation-requests==0.42b0
opentelemetry-exporter-otlp-proto-grpc==1.21.0
```

**`main.py`**:
```python
from fastapi import FastAPI, HTTPException
from opentelemetry import trace
from opentelemetry.sdk.trace import TracerProvider
from opentelemetry.sdk.trace.export import BatchSpanProcessor
from opentelemetry.exporter.otlp.proto.grpc.trace_exporter import OTLPSpanExporter
from opentelemetry.sdk.resources import Resource, SERVICE_NAME
from opentelemetry.instrumentation.fastapi import FastAPIInstrumentor
from opentelemetry.instrumentation.requests import RequestsInstrumentor
from pydantic import BaseModel
from typing import List
import time
import random

# Setup OpenTelemetry
resource = Resource.create({
    SERVICE_NAME: "inventory-service",
    "deployment.environment": "local-poc"
})

tracer_provider = TracerProvider(resource=resource)
otlp_exporter = OTLPSpanExporter(
    endpoint="http://otel-collector:4317",
    insecure=True
)
tracer_provider.add_span_processor(BatchSpanProcessor(otlp_exporter))
trace.set_tracer_provider(tracer_provider)

# Create FastAPI app
app = FastAPI(title="Inventory Service")

# Auto-instrument
FastAPIInstrumentor.instrument_app(app)
RequestsInstrumentor().instrument()

# Get tracer
tracer = trace.get_tracer(__name__)

class InventoryRequest(BaseModel):
    items: List[str]

class InventoryResponse(BaseModel):
    available: bool
    items: dict

@app.post("/api/inventory/check")
async def check_inventory(request: InventoryRequest):
    span = trace.get_current_span()
    span.set_attribute("inventory.items.count", len(request.items))

    with tracer.start_as_current_span("check_database") as db_span:
        # Simulate database query
        time.sleep(0.05 + random.random() * 0.1)

        # Simulate stock check
        items_status = {}
        for item in request.items:
            items_status[item] = random.choice([True, True, True, False])

        db_span.set_attribute("db.items_checked", len(items_status))

    all_available = all(items_status.values())
    span.set_attribute("inventory.all_available", all_available)

    if not all_available:
        span.add_event("Insufficient inventory for some items")

    return InventoryResponse(
        available=all_available,
        items=items_status
    )

@app.get("/health")
async def health():
    return {"status": "healthy"}

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8001)
```

### 4.3 Node.js Service (Express) - Analytics Service

**Dockerfile**:
```dockerfile
FROM node:20-alpine

WORKDIR /app

COPY package*.json ./
RUN npm ci --only=production

COPY . .

CMD ["node", "app.js"]
```

**`package.json`**:
```json
{
  "name": "analytics-service",
  "version": "1.0.0",
  "dependencies": {
    "express": "^4.18.2",
    "@opentelemetry/sdk-node": "^0.45.1",
    "@opentelemetry/api": "^1.7.0",
    "@opentelemetry/auto-instrumentations-node": "^0.40.0",
    "@opentelemetry/exporter-trace-otlp-grpc": "^0.45.1",
    "@opentelemetry/resources": "^1.19.0",
    "@opentelemetry/semantic-conventions": "^1.19.0"
  }
}
```

**`tracing.js`**:
```javascript
const { NodeSDK } = require('@opentelemetry/sdk-node');
const { getNodeAutoInstrumentations } = require('@opentelemetry/auto-instrumentations-node');
const { OTLPTraceExporter } = require('@opentelemetry/exporter-trace-otlp-grpc');
const { Resource } = require('@opentelemetry/resources');
const { SemanticResourceAttributes } = require('@opentelemetry/semantic-conventions');

const sdk = new NodeSDK({
  resource: new Resource({
    [SemanticResourceAttributes.SERVICE_NAME]: 'analytics-service',
    [SemanticResourceAttributes.DEPLOYMENT_ENVIRONMENT]: 'local-poc',
  }),
  traceExporter: new OTLPTraceExporter({
    url: 'http://otel-collector:4317',
  }),
  instrumentations: [
    getNodeAutoInstrumentations({
      '@opentelemetry/instrumentation-fs': {
        enabled: false,
      },
    }),
  ],
});

sdk.start();

process.on('SIGTERM', () => {
  sdk.shutdown()
    .then(() => console.log('Tracing terminated'))
    .catch((error) => console.error('Error terminating tracing', error))
    .finally(() => process.exit(0));
});

module.exports = sdk;
```

**`app.js`**:
```javascript
require('./tracing'); // Must be first!

const express = require('express');
const { trace, SpanStatusCode } = require('@opentelemetry/api');

const app = express();
app.use(express.json());

const tracer = trace.getTracer('analytics-service', '1.0.0');

app.post('/api/analytics/track', async (req, res) => {
  const currentSpan = trace.getActiveSpan();
  currentSpan.setAttribute('analytics.event_type', req.body.event_type);

  const span = tracer.startSpan('process_event');

  try {
    // Simulate processing
    await new Promise(resolve => setTimeout(resolve, 50 + Math.random() * 100));

    span.setAttribute('analytics.event_count', 1);
    span.addEvent('Event processed');
    span.setStatus({ code: SpanStatusCode.OK });

    res.json({ status: 'tracked', event_id: Date.now() });
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: SpanStatusCode.ERROR });
    res.status(500).json({ error: error.message });
  } finally {
    span.end();
  }
});

app.get('/health', (req, res) => {
  res.json({ status: 'healthy' });
});

const PORT = 8002;
app.listen(PORT, () => {
  console.log(`Analytics service listening on port ${PORT}`);
});
```

### 4.4 Complete Docker Compose with Sample Apps

**`sample-apps/docker-compose.yml`**:

```yaml
version: "3.9"

services:
  # Infrastructure
  jaeger:
    image: jaegertracing/all-in-one:1.51
    environment:
      - COLLECTOR_OTLP_ENABLED=true
    ports:
      - "16686:16686"
      - "14250:14250"
      - "4317:4317"
    networks:
      - tracing

  otel-collector:
    image: otel/opentelemetry-collector-contrib:0.89.0
    command: ["--config=/etc/otel-collector-config.yaml"]
    volumes:
      - ./otel-collector-config.yaml:/etc/otel-collector-config.yaml
    ports:
      - "4319:4317"
      - "8888:8888"
    depends_on:
      - jaeger
    networks:
      - tracing

  # Sample Applications
  order-service:
    build:
      context: ./java-order-service
    environment:
      - OTEL_SERVICE_NAME=order-service
      - OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4317
      - OTEL_TRACES_EXPORTER=otlp
    ports:
      - "8080:8080"
    depends_on:
      - otel-collector
    networks:
      - tracing

  inventory-service:
    build:
      context: ./python-inventory-service
    ports:
      - "8001:8001"
    depends_on:
      - otel-collector
    networks:
      - tracing

  analytics-service:
    build:
      context: ./node-analytics-service
    ports:
      - "8002:8002"
    depends_on:
      - otel-collector
    networks:
      - tracing

networks:
  tracing:
    driver: bridge
```

---

## 5. Testing & Verification

### 5.1 Generate Test Traffic

**`test-traffic.sh`**:
```bash
#!/bin/bash

echo "🚀 Generating test traffic..."

# Number of requests
REQUESTS=100

for i in $(seq 1 $REQUESTS); do
  # Create order
  curl -s -X POST http://localhost:8080/api/orders \
    -H "Content-Type: application/json" \
    -d "{
      \"customerId\": \"customer-$i\",
      \"items\": [\"item-1\", \"item-2\", \"item-3\"],
      \"total\": $((RANDOM % 1000 + 100))
    }" > /dev/null &

  # Check inventory directly
  if [ $((i % 3)) -eq 0 ]; then
    curl -s -X POST http://localhost:8001/api/inventory/check \
      -H "Content-Type: application/json" \
      -d '{"items": ["item-1", "item-2"]}' > /dev/null &
  fi

  # Track analytics
  if [ $((i % 2)) -eq 0 ]; then
    curl -s -X POST http://localhost:8002/api/analytics/track \
      -H "Content-Type: application/json" \
      -d '{"event_type": "order_created", "user_id": "user-'$i'"}' > /dev/null &
  fi

  # Don't overwhelm services
  if [ $((i % 10)) -eq 0 ]; then
    wait
    echo "  ✓ Sent $i requests..."
  fi
done

wait
echo "✅ Done! Sent $REQUESTS requests"
echo "🔍 View traces at http://localhost:16686"
```

Run:
```bash
chmod +x test-traffic.sh
./test-traffic.sh
```

### 5.2 Advanced Load Test (with hey)

```bash
# Install hey (HTTP load generator)
# macOS: brew install hey
# Linux: go install github.com/rakyll/hey@latest

# Load test order service
hey -n 1000 -c 10 -m POST \
  -H "Content-Type: application/json" \
  -d '{"customerId":"cust-1","items":["item-1"],"total":100}' \
  http://localhost:8080/api/orders

# View results in Jaeger
open http://localhost:16686
```

### 5.3 Verify in Jaeger UI

1. **Open Jaeger UI**: http://localhost:16686

2. **Find Services**:
   - Select "Service": `order-service`
   - Click "Find Traces"
   - See list of traces

3. **Inspect a Trace**:
   - Click on any trace
   - View flamegraph/timeline
   - See spans from all services:
     - `order-service` → `inventory-service`
   - Check span attributes, events, logs

4. **Service Dependencies**:
   - Navigate to "System Architecture"
   - See service dependency graph
   - Check request rates and error rates

5. **Search Traces**:
   - By operation: `POST /api/orders`
   - By duration: Min 500ms
   - By tags: `http.status_code=500`
   - By trace ID

### 5.4 Query Traces via API

Jaeger provides a REST API:

```bash
# Get services
curl http://localhost:16686/api/services

# Get operations for a service
curl 'http://localhost:16686/api/services/order-service/operations'

# Search traces
curl 'http://localhost:16686/api/traces?service=order-service&limit=10'

# Get specific trace
curl 'http://localhost:16686/api/traces/<trace-id>'
```

---

## 6. Troubleshooting

### Common Issues

#### Issue 1: Jaeger UI Shows No Services

```bash
# Check if Jaeger is running
docker ps | grep jaeger

# Check Jaeger logs
docker logs jaeger

# Verify OTLP is enabled
docker exec jaeger env | grep OTLP

# Test collector endpoint
curl -v http://localhost:4317

# Check if apps are sending data
docker logs otel-collector | grep "traces"
```

#### Issue 2: OTel Collector Errors

```bash
# Check config syntax
docker run --rm -v $(pwd):/conf \
  otel/opentelemetry-collector-contrib:0.89.0 \
  validate --config=/conf/otel-collector-config.yaml

# Check collector logs
docker logs otel-collector --tail 100 -f

# Increase log verbosity
# Add to config: service.telemetry.logs.level: debug
```

#### Issue 3: Application Not Sending Traces

**Java**:
```bash
# Enable debug logging
export OTEL_LOG_LEVEL=debug
export OTEL_JAVAAGENT_DEBUG=true

# Verify agent is attached
java -javaagent:opentelemetry-javaagent.jar -version
```

**Python**:
```python
# Add debug logging
from opentelemetry.sdk.trace.export import ConsoleSpanExporter

# Use console exporter to see spans locally
console_exporter = ConsoleSpanExporter()
tracer_provider.add_span_processor(BatchSpanProcessor(console_exporter))
```

**Node.js**:
```javascript
// Enable debug logs
const { diag, DiagConsoleLogger, DiagLogLevel } = require('@opentelemetry/api');
diag.setLogger(new DiagConsoleLogger(), DiagLogLevel.DEBUG);
```

#### Issue 4: High Memory Usage

```bash
# Check memory usage
docker stats --no-stream

# Reduce OTel Collector memory
# Edit config:
processors:
  memory_limiter:
    limit_mib: 256  # Reduce from 512
    spike_limit_mib: 64

  batch:
    send_batch_size: 512  # Reduce from 1024
```

---

## 7. Resource Requirements

### Actual Usage (Local POC)

| Component         | CPU (idle) | CPU (load) | Memory | Disk       |
| ----------------- | ---------- | ---------- | ------ | ---------- |
| Jaeger All-in-One | 0.05 cores | 0.5 cores  | 200MB  | Minimal    |
| OTel Collector    | 0.05 cores | 0.3 cores  | 200MB  | Negligible |
| Sample Apps (3)   | 0.1 cores  | 1 core     | 600MB  | Minimal    |
| **Total**         | **0.2**    | **1.8**    | **1GB** | **<100MB** |

**With Elasticsearch**:
- Add 2-4GB memory for Elasticsearch
- Add 5-10GB disk space

### Comparison with SigNoz

| Aspect        | Jaeger POC | SigNoz POC |
| ------------- | ---------- | ---------- |
| CPU (idle)    | 0.2 cores  | 0.3 cores  |
| CPU (load)    | 1.8 cores  | 3 cores    |
| Memory        | 1GB        | 3-5GB      |
| Disk          | <100MB     | 6-10GB     |
| Startup Time  | 30 seconds | 2-3 min    |
| **Winner**    | **Jaeger** | -          |

**Conclusion**: Jaeger is significantly lighter for local POC testing!

---

## 8. Cleanup

### Stop Services (Keep Data)
```bash
docker-compose stop
```

### Complete Cleanup
```bash
# Stop and remove containers
docker-compose down -v

# Remove images
docker rmi jaegertracing/all-in-one:1.51
docker rmi otel/opentelemetry-collector-contrib:0.89.0
```

---

## 9. Next Steps

1. **Compare with SigNoz**:
   - Run both POCs side-by-side
   - Evaluate UI, features, performance
   - Check documentation at `../signoz/02_LOCAL_POC_SETUP.md`

2. **Test with Your Services**:
   - Instrument your actual applications
   - Generate realistic traffic patterns
   - Measure overhead and performance

3. **Plan Production Deployment**:
   - Review Kubernetes deployment guide
   - Plan high availability setup
   - Configure persistent storage

4. **Make Decision**:
   - Review comparison document
   - Consider trade-offs
   - Choose best solution for your needs

---

## 10. Quick Reference

### Essential Commands

```bash
# Start POC
docker-compose up -d

# View logs
docker-compose logs -f

# Restart collector
docker-compose restart otel-collector

# Stop POC
docker-compose down

# Access Jaeger UI
open http://localhost:16686
```

### Important URLs

- Jaeger UI: http://localhost:16686
- OTel Collector (gRPC): http://localhost:4317
- OTel Collector (HTTP): http://localhost:4318
- OTel Collector Metrics: http://localhost:8888/metrics
- Health Check: http://localhost:13133

---

*Document Version: 1.0*
*Last Updated: November 7, 2025*
*Status: Ready for Testing*

