# SigNoz Local POC Setup Guide

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

**Minimum (Basic POC)**:
- CPU: 4 cores
- RAM: 8GB
- Disk: 20GB free space
- OS: macOS, Linux, or Windows (with WSL2)

**Recommended (Full Testing)**:
- CPU: 8 cores
- RAM: 16GB
- Disk: 50GB free space

### Software Requirements

```bash
# Check installations
docker --version          # >= 20.10
docker-compose --version  # >= 2.0
git --version
```

**Install if missing**:
- Docker Desktop (includes Docker + Docker Compose)
- Git

---

## 2. Quick Start (5 Minutes)

### Option A: Official SigNoz Install Script

```bash
# Clone SigNoz repository
git clone https://github.com/SigNoz/signoz.git
cd signoz/deploy

# Install (this will pull all containers)
./install.sh

# Wait for all services to start (2-3 minutes)
# Watch logs
docker-compose logs -f
```

**Access SigNoz UI**:
- URL: http://localhost:8080
- First time: Create admin account

### Option B: Custom Docker Compose (Minimal)

Create `signoz-poc/docker-compose.yml`:

```yaml
version: "3.9"

x-clickhouse-defaults: &clickhouse-defaults
  restart: on-failure
  image: clickhouse/clickhouse-server:23.11-alpine
  tty: true
  depends_on:
    - zookeeper-1
  logging:
    options:
      max-size: 50m
      max-file: "3"
  healthcheck:
    test: ["CMD", "wget", "--spider", "-q", "localhost:8123/ping"]
    interval: 30s
    timeout: 5s
    retries: 3

x-db-depend: &db-depend
  depends_on:
    clickhouse:
      condition: service_healthy

services:
  # ClickHouse (Storage)
  zookeeper-1:
    image: bitnami/zookeeper:3.9.1
    container_name: signoz-zookeeper-1
    hostname: zookeeper-1
    user: root
    volumes:
      - ./data/zookeeper:/bitnami/zookeeper
    environment:
      - ZOO_SERVER_ID=1
      - ALLOW_ANONYMOUS_LOGIN=yes
      - ZOO_AUTOPURGE_INTERVAL=1

  clickhouse:
    <<: *clickhouse-defaults
    container_name: signoz-clickhouse
    hostname: clickhouse
    volumes:
      - ./data/clickhouse:/var/lib/clickhouse
      - ./clickhouse-config.xml:/etc/clickhouse-server/config.d/config.xml
      - ./clickhouse-users.xml:/etc/clickhouse-server/users.d/users.xml
    ports:
      - "9000:9000"      # Native client
      - "8123:8123"      # HTTP
      - "9181:9181"      # Inter-server

  # OpenTelemetry Collector
  otel-collector:
    image: signoz/signoz-otel-collector:0.88.11
    container_name: signoz-otel-collector
    command: ["--config=/etc/otel-collector-config.yaml"]
    user: root
    volumes:
      - ./otel-collector-config.yaml:/etc/otel-collector-config.yaml
    environment:
      - OTEL_RESOURCE_ATTRIBUTES=host.name=signoz-host,os.type=linux
    ports:
      - "4317:4317"      # OTLP gRPC receiver
      - "4318:4318"      # OTLP HTTP receiver
    <<: *db-depend

  # Query Service (Backend API)
  query-service:
    image: signoz/query-service:0.39.0
    container_name: signoz-query-service
    command: ["-config=/root/config/prometheus.yml"]
    volumes:
      - ./prometheus.yml:/root/config/prometheus.yml
      - ./dashboards:/root/config/dashboards
    environment:
      - ClickHouseUrl=tcp://clickhouse:9000
      - STORAGE=clickhouse
      - GODEBUG=netdns=go
      - TELEMETRY_ENABLED=true
      - DEPLOYMENT_TYPE=docker-standalone-amd
    ports:
      - "8080:8080"      # Query Service API
    <<: *db-depend

  # Frontend
  frontend:
    image: signoz/frontend:0.39.0
    container_name: signoz-frontend
    depends_on:
      - query-service
    ports:
      - "8080:8080"      # SigNoz UI
    environment:
      - FRONTEND_API_ENDPOINT=http://query-service:8080

  # Alert Manager
  alertmanager:
    image: signoz/alertmanager:0.23.5
    container_name: signoz-alertmanager
    volumes:
      - ./alertmanager-config.yaml:/etc/alertmanager/config.yml
    command:
      - --config.file=/etc/alertmanager/config.yml
    ports:
      - "9093:9093"

volumes:
  zookeeper-data:
  clickhouse-data:
```

**Start Services**:
```bash
# Create directory structure
mkdir -p signoz-poc/data/{clickhouse,zookeeper}
cd signoz-poc

# Download config files (see next section)
curl -O https://raw.githubusercontent.com/SigNoz/signoz/main/deploy/docker/clickhouse-setup/clickhouse-config.xml
curl -O https://raw.githubusercontent.com/SigNoz/signoz/main/deploy/docker/clickhouse-setup/clickhouse-users.xml
curl -O https://raw.githubusercontent.com/SigNoz/signoz/main/deploy/docker/clickhouse-setup/otel-collector-config.yaml

# Start all services
docker-compose up -d

# Check status
docker-compose ps
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

  # For Jaeger compatibility
  jaeger:
    protocols:
      grpc:
        endpoint: 0.0.0.0:14250
      thrift_http:
        endpoint: 0.0.0.0:14268

processors:
  batch:
    send_batch_size: 10000
    send_batch_max_size: 11000
    timeout: 10s

  memory_limiter:
    check_interval: 1s
    limit_mib: 4000
    spike_limit_mib: 800

  # Resource detection
  resourcedetection:
    detectors: [env, system]
    timeout: 5s

  # Add host metrics
  resource:
    attributes:
      - key: deployment.environment
        value: local-poc
        action: insert

exporters:
  # ClickHouse exporters
  clickhousemetricswrite:
    endpoint: tcp://clickhouse:9000/?database=signoz_metrics
    resource_to_telemetry_conversion:
      enabled: true

  clickhousetraceswrite:
    datasource: tcp://clickhouse:9000/?database=signoz_traces
    low_cardinal_exception_grouping: true

  clickhouselogswrite:
    endpoint: tcp://clickhouse:9000/?database=signoz_logs

  # Debug logging
  logging:
    loglevel: debug
    sampling_initial: 5
    sampling_thereafter: 200

service:
  telemetry:
    logs:
      level: info
    metrics:
      address: 0.0.0.0:8888

  pipelines:
    traces:
      receivers: [otlp, jaeger]
      processors: [memory_limiter, batch, resourcedetection, resource]
      exporters: [clickhousetraceswrite]

    metrics:
      receivers: [otlp]
      processors: [memory_limiter, batch, resourcedetection, resource]
      exporters: [clickhousemetricswrite]

    logs:
      receivers: [otlp]
      processors: [memory_limiter, batch, resourcedetection, resource]
      exporters: [clickhouselogswrite]
```

#### `prometheus.yml` (Query Service Config)

```yaml
global:
  scrape_interval: 60s
  evaluation_interval: 60s
  external_labels:
    cluster: "local-poc"

rule_files:
  - "/root/config/alerting_rules.yml"

alerting:
  alertmanagers:
    - static_configs:
        - targets: ["alertmanager:9093"]

scrape_configs:
  - job_name: "otel-collector"
    static_configs:
      - targets: ["otel-collector:8888"]
```

#### `alertmanager-config.yaml`

```yaml
route:
  receiver: "default"
  group_by: ["alertname"]
  group_wait: 10s
  group_interval: 10s
  repeat_interval: 12h

receivers:
  - name: "default"
    # Webhook for notifications
    webhook_configs:
      - url: "http://query-service:8080/api/v1/alerts"
        send_resolved: true
```

### 3.2 ClickHouse Initialization

The official install script handles this, but if you need manual setup:

**Create `init-clickhouse.sh`**:
```bash
#!/bin/bash

# Wait for ClickHouse to be ready
echo "Waiting for ClickHouse..."
until docker exec signoz-clickhouse clickhouse-client --query "SELECT 1" > /dev/null 2>&1; do
  sleep 2
done

# Create databases
docker exec signoz-clickhouse clickhouse-client --query "CREATE DATABASE IF NOT EXISTS signoz_traces"
docker exec signoz-clickhouse clickhouse-client --query "CREATE DATABASE IF NOT EXISTS signoz_metrics"
docker exec signoz-clickhouse clickhouse-client --query "CREATE DATABASE IF NOT EXISTS signoz_logs"

echo "ClickHouse initialized!"
```

Run: `chmod +x init-clickhouse.sh && ./init-clickhouse.sh`

---

## 4. Sample Applications

### 4.1 Java Service (Spring Boot)

**`pom.xml`**:
```xml
<dependencies>
    <dependency>
        <groupId>io.opentelemetry</groupId>
        <artifactId>opentelemetry-api</artifactId>
        <version>1.32.0</version>
    </dependency>
</dependencies>
```

**`OrderService.java`**:
```java
package com.example.demo;

import io.opentelemetry.api.OpenTelemetry;
import io.opentelemetry.api.trace.Span;
import io.opentelemetry.api.trace.Tracer;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/orders")
public class OrderController {

    private final Tracer tracer;

    public OrderController(OpenTelemetry openTelemetry) {
        this.tracer = openTelemetry.getTracer("order-service", "1.0.0");
    }

    @PostMapping
    public String createOrder(@RequestBody Order order) {
        Span span = tracer.spanBuilder("createOrder").startSpan();

        try {
            span.setAttribute("order.id", order.getId());
            span.setAttribute("order.amount", order.getAmount());

            // Business logic
            Thread.sleep(100); // Simulate processing

            return "Order created: " + order.getId();
        } catch (Exception e) {
            span.recordException(e);
            throw e;
        } finally {
            span.end();
        }
    }
}
```

**Run with Java Agent**:
```bash
# Download OpenTelemetry Java Agent
wget https://github.com/open-telemetry/opentelemetry-java-instrumentation/releases/latest/download/opentelemetry-javaagent.jar

# Run application
java -javaagent:opentelemetry-javaagent.jar \
  -Dotel.service.name=order-service \
  -Dotel.traces.exporter=otlp \
  -Dotel.exporter.otlp.endpoint=http://localhost:4317 \
  -Dotel.metrics.exporter=otlp \
  -Dotel.logs.exporter=otlp \
  -jar target/order-service.jar
```

### 4.2 Python Service (FastAPI)

**`requirements.txt`**:
```
fastapi==0.104.1
uvicorn==0.24.0
opentelemetry-api==1.21.0
opentelemetry-sdk==1.21.0
opentelemetry-instrumentation-fastapi==0.42b0
opentelemetry-exporter-otlp-proto-grpc==1.21.0
```

**`main.py`**:
```python
from fastapi import FastAPI
from opentelemetry import trace
from opentelemetry.sdk.trace import TracerProvider
from opentelemetry.sdk.trace.export import BatchSpanProcessor
from opentelemetry.exporter.otlp.proto.grpc.trace_exporter import OTLPSpanExporter
from opentelemetry.sdk.resources import Resource, SERVICE_NAME
from opentelemetry.instrumentation.fastapi import FastAPIInstrumentor
import time

# Setup OpenTelemetry
resource = Resource.create({SERVICE_NAME: "inventory-service"})
tracer_provider = TracerProvider(resource=resource)
otlp_exporter = OTLPSpanExporter(endpoint="http://localhost:4317", insecure=True)
tracer_provider.add_span_processor(BatchSpanProcessor(otlp_exporter))
trace.set_tracer_provider(tracer_provider)

app = FastAPI()
FastAPIInstrumentor.instrument_app(app)
tracer = trace.get_tracer(__name__)

@app.get("/api/inventory/check")
async def check_inventory(item_id: str):
    with tracer.start_as_current_span("check_inventory") as span:
        span.set_attribute("item.id", item_id)

        # Simulate database check
        time.sleep(0.05)

        return {"item_id": item_id, "in_stock": True, "quantity": 42}

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8001)
```

**Run**:
```bash
pip install -r requirements.txt
python main.py
```

### 4.3 Node.js Service (Express)

**`package.json`**:
```json
{
  "name": "analytics-service",
  "version": "1.0.0",
  "dependencies": {
    "express": "^4.18.2",
    "@opentelemetry/sdk-node": "^0.45.1",
    "@opentelemetry/auto-instrumentations-node": "^0.40.0",
    "@opentelemetry/exporter-trace-otlp-grpc": "^0.45.1"
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
  }),
  traceExporter: new OTLPTraceExporter({
    url: 'http://localhost:4317',
  }),
  instrumentations: [getNodeAutoInstrumentations()],
});

sdk.start();
```

**`app.js`**:
```javascript
require('./tracing'); // Must be first!

const express = require('express');
const app = express();

app.get('/api/analytics/events', (req, res) => {
  // Automatically traced by OpenTelemetry
  res.json({ events: ['event1', 'event2'], count: 2 });
});

app.listen(8002, () => {
  console.log('Analytics service listening on port 8002');
});
```

**Run**:
```bash
npm install
node app.js
```

### 4.4 Docker Compose for Sample Apps

Create `sample-apps/docker-compose.yml`:

```yaml
version: "3.9"

services:
  order-service:
    build: ./java-order-service
    ports:
      - "8080:8080"
    environment:
      - OTEL_SERVICE_NAME=order-service
      - OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4317
      - OTEL_TRACES_EXPORTER=otlp
      - OTEL_METRICS_EXPORTER=otlp
    networks:
      - signoz_default

  inventory-service:
    build: ./python-inventory-service
    ports:
      - "8001:8001"
    environment:
      - OTEL_SERVICE_NAME=inventory-service
      - OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4317
    networks:
      - signoz_default

  analytics-service:
    build: ./node-analytics-service
    ports:
      - "8002:8002"
    environment:
      - OTEL_SERVICE_NAME=analytics-service
      - OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4317
    networks:
      - signoz_default

networks:
  signoz_default:
    external: true
```

---

## 5. Testing & Verification

### 5.1 Generate Test Traffic

**Simple Load Test Script** (`test-traffic.sh`):
```bash
#!/bin/bash

echo "Generating test traffic..."

for i in {1..50}; do
  # Order service
  curl -X POST http://localhost:8080/api/orders \
    -H "Content-Type: application/json" \
    -d '{"id": "order-'$i'", "amount": 100.50}' &

  # Inventory service
  curl http://localhost:8001/api/inventory/check?item_id=item-$i &

  # Analytics service
  curl http://localhost:8002/api/analytics/events &

  sleep 0.1
done

wait
echo "Traffic generation complete!"
```

Run: `chmod +x test-traffic.sh && ./test-traffic.sh`

### 5.2 Verify in SigNoz UI

1. **Open SigNoz**: http://localhost:8080

2. **Check Services**:
   - Navigate to "Services" tab
   - You should see: `order-service`, `inventory-service`, `analytics-service`
   - Check metrics: Request rate, error rate, latency (p50, p99)

3. **View Traces**:
   - Navigate to "Traces" tab
   - Filter by service name
   - Click on any trace to see flamegraph
   - Verify span details, attributes, events

4. **Check Service Map**:
   - Navigate to "Service Map"
   - See dependencies between services
   - Verify request flow

5. **Explore Logs** (if configured):
   - Navigate to "Logs" tab
   - Filter by trace_id to correlate with traces

### 5.3 Query ClickHouse Directly

```bash
# Connect to ClickHouse
docker exec -it signoz-clickhouse clickhouse-client

# Query traces
SELECT
    serviceName,
    name,
    count() as span_count,
    avg(durationNano / 1000000) as avg_duration_ms
FROM signoz_traces.signoz_index_v2
WHERE timestamp >= now() - INTERVAL 1 HOUR
GROUP BY serviceName, name
ORDER BY span_count DESC
LIMIT 10;

# Query metrics
SELECT
    metric_name,
    count() as sample_count
FROM signoz_metrics.samples_v2
WHERE timestamp_ms >= toUnixTimestamp(now() - INTERVAL 1 HOUR) * 1000
GROUP BY metric_name
ORDER BY sample_count DESC
LIMIT 10;
```

---

## 6. Troubleshooting

### Common Issues

#### Issue 1: Containers Won't Start

```bash
# Check Docker resources
docker stats

# Check container logs
docker-compose logs clickhouse
docker-compose logs otel-collector
docker-compose logs query-service

# Increase Docker memory
# Docker Desktop → Settings → Resources → Memory → 8GB+
```

#### Issue 2: No Data in SigNoz

```bash
# Verify OTel Collector is receiving data
docker logs signoz-otel-collector --tail 100

# Check if apps are sending data
# Look for OTLP endpoint connections

# Test OTLP endpoint
curl -v http://localhost:4317

# Check ClickHouse has data
docker exec signoz-clickhouse clickhouse-client --query \
  "SELECT count() FROM signoz_traces.signoz_index_v2"
```

#### Issue 3: ClickHouse Errors

```bash
# Check ClickHouse logs
docker logs signoz-clickhouse

# Verify ZooKeeper is running
docker ps | grep zookeeper

# Reset ClickHouse data (WARNING: deletes all data)
docker-compose down
rm -rf data/clickhouse/*
docker-compose up -d
```

#### Issue 4: High Resource Usage

```bash
# Check resource usage
docker stats --no-stream

# Reduce ClickHouse memory
# Edit clickhouse-config.xml:
# <max_memory_usage>4000000000</max_memory_usage>

# Reduce OTel Collector batch size
# Edit otel-collector-config.yaml:
# batch:
#   send_batch_size: 1000
```

### Debug Checklist

```
□ Docker daemon running?
□ Sufficient disk space? (df -h)
□ Ports not in use? (lsof -i :8080, :4317, :9000)
□ All containers healthy? (docker-compose ps)
□ ClickHouse initialized? (check logs)
□ OTel Collector config valid? (check logs)
□ Apps configured with correct endpoint?
□ Firewall not blocking ports?
```

---

## 7. Resource Requirements

### Actual Usage (Local POC)

| Component      | CPU (idle) | CPU (load) | Memory    | Disk       |
| -------------- | ---------- | ---------- | --------- | ---------- |
| ClickHouse     | 0.1 cores  | 2 cores    | 2-4GB     | 5GB+       |
| OTel Collector | 0.05 cores | 0.5 cores  | 200MB     | Negligible |
| Query Service  | 0.05 cores | 0.3 cores  | 300MB     | Negligible |
| Frontend       | 0.01 cores | 0.1 cores  | 100MB     | Negligible |
| ZooKeeper      | 0.05 cores | 0.1 cores  | 100MB     | 1GB        |
| **Total**      | **0.3**    | **3**      | **3-5GB** | **6-10GB** |

### Optimization Tips

**For constrained environments (4GB RAM)**:
```yaml
# docker-compose.override.yml
version: "3.9"

services:
  clickhouse:
    deploy:
      resources:
        limits:
          memory: 2G
        reservations:
          memory: 1G

  otel-collector:
    deploy:
      resources:
        limits:
          memory: 500M

  query-service:
    deploy:
      resources:
        limits:
          memory: 500M
```

**Reduce data retention**:
```bash
# In ClickHouse
docker exec signoz-clickhouse clickhouse-client --query \
  "ALTER TABLE signoz_traces.signoz_index_v2 MODIFY TTL timestamp + INTERVAL 1 DAY"
```

---

## 8. Cleanup

### Stop Services (Keep Data)
```bash
docker-compose stop
```

### Stop and Remove Containers (Keep Data)
```bash
docker-compose down
```

### Complete Cleanup (Delete Everything)
```bash
# Stop and remove containers
docker-compose down -v

# Remove data directories
rm -rf data/

# Remove downloaded images (optional)
docker rmi $(docker images 'signoz/*' -q)
docker rmi clickhouse/clickhouse-server:23.11-alpine
```

### Selective Cleanup (Reset Data, Keep Containers)
```bash
# Stop containers
docker-compose stop

# Remove only data
rm -rf data/clickhouse/* data/zookeeper/*

# Restart
docker-compose up -d
```

---

## 9. Next Steps

After successful POC:

1. **Evaluate Performance**:
   - Query speed
   - Resource usage
   - Data retention needs

2. **Test with Real Services**:
   - Instrument your actual applications
   - Generate production-like load
   - Measure overhead

3. **Plan Production Deployment**:
   - Kubernetes setup (see `04_DEPLOYMENT_GUIDE.md`)
   - High availability configuration
   - Backup and disaster recovery

4. **Compare with Jaeger**:
   - Run both POCs side-by-side
   - Compare features, performance, usability
   - Make informed decision

---

## 10. Quick Reference

### Essential Commands

```bash
# Start SigNoz
docker-compose up -d

# View logs
docker-compose logs -f

# Check status
docker-compose ps

# Restart service
docker-compose restart otel-collector

# Stop SigNoz
docker-compose down

# Access SigNoz UI
open http://localhost:8080

# Access ClickHouse CLI
docker exec -it signoz-clickhouse clickhouse-client

# Check disk usage
du -sh data/
```

### Important URLs

- SigNoz UI: http://localhost:8080
- OTel Collector (gRPC): http://localhost:4317
- OTel Collector (HTTP): http://localhost:4318
- Query Service API: http://localhost:8080
- ClickHouse HTTP: http://localhost:8123

---

## 11. Additional Resources

- [SigNoz Documentation](https://signoz.io/docs/)
- [OpenTelemetry Documentation](https://opentelemetry.io/docs/)
- [ClickHouse Documentation](https://clickhouse.com/docs/)
- [SigNoz GitHub](https://github.com/SigNoz/signoz)
- [SigNoz Community Slack](https://signoz.io/slack)

---

*Document Version: 1.0*
*Last Updated: November 7, 2025*
*Status: Ready for Testing*

