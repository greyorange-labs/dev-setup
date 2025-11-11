# OpenTelemetry + SigNoz Distributed Tracing
## Rollout Plan & Deployment Strategy

**Document Version:** 1.0
**Last Updated:** November 2025
**Target Audience:** Technical Leads, DevOps, SRE Teams

---

## Table of Contents

1. [Rollout Overview](#rollout-overview)
2. [Phase 1: Foundation](#phase-1-foundation-week-1-2)
3. [Phase 2: Development Environment](#phase-2-development-environment-week-3-4)
4. [Phase 3: Staging Environment](#phase-3-staging-environment-week-5-7)
5. [Phase 4: Production Pilot](#phase-4-production-pilot-week-8-9)
6. [Phase 5: Production Rollout](#phase-5-production-rollout-week-10-12)
7. [Environment-Specific Configurations](#environment-specific-configurations)
8. [Emergency Procedures](#emergency-procedures)
9. [Rollback Strategy](#rollback-strategy)

---

## Rollout Overview

### Timeline

```
12-Week Phased Rollout Plan

Week 1-2:   Foundation (SigNoz deployment, training)
Week 3-4:   Development (Java & Erlang instrumentation)
Week 5-7:   Staging (Load testing, validation)
Week 8-9:   Production Pilot (1-2 non-critical services)
Week 10-12: Production Rollout (All services, gradual)
```

### Objectives

| Phase       | Primary Goal        | Success Criteria                          |
| ----------- | ------------------- | ----------------------------------------- |
| **Phase 1** | SigNoz operational  | UI accessible, OTLP receiving traces      |
| **Phase 2** | Dev instrumentation | All services instrumented, traces visible |
| **Phase 3** | Staging validation  | < 2% performance impact, stable traces    |
| **Phase 4** | Pilot validation    | Production-ready, team feedback positive  |
| **Phase 5** | Full deployment     | All services traced, metrics tracking     |

### Risk Mitigation

**Strategy:** Gradual rollout with multiple validation gates

- ✅ Each phase has explicit go/no-go criteria
- ✅ Rollback procedures defined for each phase
- ✅ Performance monitoring at every stage
- ✅ Emergency off switch always available

---

## Phase 1: Foundation (Week 1-2)

### Objectives

1. Deploy SigNoz infrastructure
2. Train technical team
3. Establish monitoring and alerting
4. Prepare deployment scripts

### Tasks

#### Week 1: SigNoz Deployment

**Day 1-2: Infrastructure Setup**

```bash
# 1. Provision servers (or containers)
# Development SigNoz:
# - 1 server: 4 vCPU, 8 GB RAM, 100 GB disk

# 2. Install Docker and Docker Compose
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh
sudo curl -L "https://github.com/docker/compose/releases/latest/download/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose

# 3. Deploy SigNoz
git clone https://github.com/SigNoz/signoz.git
cd signoz/deploy
./install.sh

# 4. Verify deployment
docker ps
# Expected: signoz-clickhouse, signoz-query-service, signoz-frontend, signoz-otel-collector

# 5. Access UI
open http://localhost:8080
# Complete initial setup wizard
```

**Day 3: Configure SigNoz**

```yaml
# docker-compose.yaml adjustments
version: "3.8"

services:
  clickhouse:
    image: clickhouse/clickhouse-server:23.11
    volumes:
      - ./data/clickhouse:/var/lib/clickhouse
    environment:
      - CLICKHOUSE_DB=signoz
    ports:
      - "9000:9000"
    # Add resource limits
    deploy:
      resources:
        limits:
          memory: 4G

  otel-collector:
    image: signoz/signoz-otel-collector:0.88.0
    ports:
      - "4318:4318"  # OTLP HTTP
      - "4317:4317"  # OTLP gRPC
    # Add resource limits
    deploy:
      resources:
        limits:
          memory: 2G

  query-service:
    image: signoz/query-service:0.38.0
    ports:
      - "8080:8080"
    deploy:
      resources:
        limits:
          memory: 2G

  frontend:
    image: signoz/frontend:0.38.0
    ports:
      - "8080:8080"
    deploy:
      resources:
        limits:
          memory: 512M
```

**Day 4-5: Monitoring & Alerting**

```yaml
# monitoring/prometheus-alerts.yaml
groups:
  - name: signoz_alerts
    rules:
      - alert: SigNozOTLPReceiverDown
        expr: up{job="signoz-otel-collector"} == 0
        for: 5m
        labels:
          severity: critical
        annotations:
          summary: "SigNoz OTLP receiver is down"
          description: "Cannot receive traces from applications"

      - alert: SigNozHighMemoryUsage
        expr: (container_memory_usage_bytes{name=~"signoz.*"} / container_spec_memory_limit_bytes{name=~"signoz.*"}) > 0.9
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "SigNoz component using > 90% memory"

      - alert: SigNozClickHouseDiskSpace
        expr: (node_filesystem_avail_bytes{mountpoint="/var/lib/clickhouse"} / node_filesystem_size_bytes{mountpoint="/var/lib/clickhouse"}) < 0.2
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "ClickHouse disk space < 20%"
```

```bash
# Deploy monitoring
docker-compose -f monitoring/docker-compose.yaml up -d
```

#### Week 2: Team Training & Preparation

**Day 6-7: Training Sessions**

**Session 1: OpenTelemetry Concepts (2 hours)**
- Distributed tracing fundamentals
- Traces, spans, context propagation
- W3C Trace Context standard
- Hands-on demo with SigNoz UI

**Session 2: Implementation Workshop (2 hours)**
- Java: Java agent setup and configuration
- Erlang: Manual instrumentation with macros
- Live coding examples
- Q&A

**Day 8-9: Documentation & Scripts**

```bash
# Create deployment scripts
mkdir -p scripts/deployment

# scripts/deployment/deploy-otel-java.sh
cat > scripts/deployment/deploy-otel-java.sh << 'EOF'
#!/bin/bash
set -e

SERVICE_NAME=$1
ENVIRONMENT=$2
AGENT_VERSION="2.10.0"

if [ -z "$SERVICE_NAME" ] || [ -z "$ENVIRONMENT" ]; then
    echo "Usage: $0 <service-name> <environment>"
    exit 1
fi

echo "Deploying OpenTelemetry to $SERVICE_NAME ($ENVIRONMENT)..."

# Download agent if not exists
if [ ! -f "/opt/observability/opentelemetry-javaagent.jar" ]; then
    echo "Downloading OpenTelemetry Java agent..."
    curl -L -o /opt/observability/opentelemetry-javaagent.jar \
        https://github.com/open-telemetry/opentelemetry-java-instrumentation/releases/download/v${AGENT_VERSION}/opentelemetry-javaagent.jar
fi

# Create environment file
cat > /etc/default/${SERVICE_NAME}-otel << ENVEOF
export JAVA_TOOL_OPTIONS="-javaagent:/opt/observability/opentelemetry-javaagent.jar"
export OTEL_SERVICE_NAME="${SERVICE_NAME}"
export OTEL_TRACES_EXPORTER="otlp"
export OTEL_EXPORTER_OTLP_ENDPOINT="http://signoz-${ENVIRONMENT}:4318"
export OTEL_EXPORTER_OTLP_PROTOCOL="http/protobuf"
export OTEL_TRACES_SAMPLER="always_on"
export OTEL_PROPAGATORS="tracecontext,baggage"
export OTEL_RESOURCE_ATTRIBUTES="environment=${ENVIRONMENT}"
ENVEOF

echo "Configuration written to /etc/default/${SERVICE_NAME}-otel"
echo "Restart service to apply changes"
EOF

chmod +x scripts/deployment/deploy-otel-java.sh
```

**Day 10: Pre-Production Checklist**

- [ ] SigNoz UI accessible and responsive
- [ ] OTLP endpoints (4318, 4317) accepting connections
- [ ] ClickHouse database operational
- [ ] Disk space monitoring configured
- [ ] Backup strategy defined
- [ ] Team trained on basic concepts
- [ ] Deployment scripts tested
- [ ] Documentation reviewed

### Phase 1 Success Criteria

- ✅ SigNoz deployed and operational
- ✅ Test traces successfully exported and visible
- ✅ Monitoring and alerting configured
- ✅ Team trained (minimum 80% attendance)
- ✅ Deployment scripts validated
- ✅ Go/No-Go decision for Phase 2

**Go/No-Go Decision Point:** End of Week 2

---

## Phase 2: Development Environment (Week 3-4)

### Objectives

1. Instrument all Java services (dev environment)
2. Instrument all Erlang services (dev environment)
3. Validate end-to-end tracing
4. Fix any integration issues

### Tasks

#### Week 3: Java Services

**Day 1-2: API Gateway**

```bash
# 1. Deploy OpenTelemetry agent
./scripts/deployment/deploy-otel-java.sh api-gateway dev

# 2. Update systemd service
sudo systemctl edit api-gateway

# Add:
[Service]
EnvironmentFile=/etc/default/api-gateway-otel

# 3. Restart service
sudo systemctl restart api-gateway

# 4. Verify startup
sudo journalctl -u api-gateway -f | grep -i opentelemetry
# Expected: "OpenTelemetry Javaagent 2.10.0 started"

# 5. Test tracing
curl http://localhost:8080/api-gateway/health
# Check SigNoz: Service "api-gateway" should appear
```

**Day 3-4: Auth Service**

```bash
# Repeat same process for auth service
./scripts/deployment/deploy-otel-java.sh gor-platform-auth dev

# Update config
sudo systemctl edit gor-platform-auth

# Restart and verify
sudo systemctl restart gor-platform-auth
sudo journalctl -u gor-platform-auth -f | grep -i opentelemetry
```

**Day 5: Validation**

```bash
# End-to-end test
curl -v http://localhost:8080/api-gateway/gm_core/api/butler_shared/v2/diagnose/infra

# Extract trace ID from response
TRACE_ID=$(curl -s -v http://localhost:8080/api-gateway/health 2>&1 | grep traceparent | awk -F'-' '{print $2}')
echo "Trace ID: $TRACE_ID"

# Search in SigNoz UI
# Navigate to Traces → Search by trace_id: $TRACE_ID
# Expected: Complete trace showing Gateway → Auth flow
```

#### Week 4: Erlang Services

**Day 6-8: GM Core**

```bash
# 1. Update dependencies
cd /path/to/butler_server
vi rebar.config
# Add OpenTelemetry dependencies (see implementation guide)

# 2. Fetch dependencies
rebar3 get-deps

# 3. Update sys.config
cp config/sys.config.example config/dev.config
vi config/dev.config
# Add OpenTelemetry configuration (see implementation guide)

# 4. Add tracing to critical paths
# Edit source files to add ?with_span macros
# Priority:
#   - HTTP handlers (entry points)
#   - Kafka consumers
#   - Major business logic functions

# 5. Compile and build release
rebar3 compile
rebar3 as dev release

# 6. Start service
_build/dev/rel/butler_server/bin/butler_server start

# 7. Verify OTel apps running
_build/dev/rel/butler_server/bin/butler_server remote_console
> application:which_applications().
% Expected: opentelemetry, opentelemetry_api, opentelemetry_exporter

# 8. Test tracing
curl -X POST http://localhost:8181/api/test
# Check SigNoz: Service "butler_server" should appear
```

**Day 9: End-to-End Validation**

```bash
# Complete flow test: Gateway → Auth → Butler
./scripts/testing/test-e2e-tracing.sh
```

```bash
# scripts/testing/test-e2e-tracing.sh
#!/bin/bash
set -e

echo "===== End-to-End Tracing Test ====="

# 1. Generate test trace ID
TRACE_ID=$(openssl rand -hex 16)
SPAN_ID=$(openssl rand -hex 8)
TRACEPARENT="00-${TRACE_ID}-${SPAN_ID}-01"

echo "Test Trace ID: ${TRACE_ID}"

# 2. Make request with trace context
echo "Making request to API Gateway..."
curl -X GET "http://localhost:8080/api-gateway/gm_core/api/butler_shared/v2/diagnose/infra" \
  -H "Content-Type: application/json" \
  -H "traceparent: ${TRACEPARENT}" \
  -o /dev/null -s -w "HTTP Status: %{http_code}\n"

# 3. Wait for export (batch delay)
echo "Waiting for spans to export..."
sleep 10

# 4. Query SigNoz API for trace
echo "Querying SigNoz for trace..."
SIGNOZ_API="http://localhost:8080/api/v1/traces/${TRACE_ID}"
TRACE_DATA=$(curl -s "${SIGNOZ_API}")

# 5. Validate spans
SPAN_COUNT=$(echo "${TRACE_DATA}" | jq '.spans | length')
echo "Spans found: ${SPAN_COUNT}"

# Expected: at least 3 spans (Gateway, Auth, Butler)
if [ "$SPAN_COUNT" -ge 3 ]; then
    echo "✅ SUCCESS: Complete trace found with ${SPAN_COUNT} spans"
    echo "Services in trace:"
    echo "${TRACE_DATA}" | jq -r '.spans[].serviceName' | sort -u
    exit 0
else
    echo "❌ FAILURE: Incomplete trace (expected >= 3 spans, got ${SPAN_COUNT})"
    exit 1
fi
```

**Day 10: Issue Resolution**

- Review and fix any integration issues
- Adjust span attributes
- Optimize sampling if needed
- Update documentation based on findings

### Phase 2 Success Criteria

- ✅ All Java services instrumented and exporting traces
- ✅ All Erlang services instrumented and exporting traces
- ✅ End-to-end traces visible in SigNoz
- ✅ Trace context propagates correctly
- ✅ No critical bugs or errors
- ✅ Go/No-Go decision for Phase 3

**Go/No-Go Decision Point:** End of Week 4

---

## Phase 3: Staging Environment (Week 5-7)

### Objectives

1. Deploy to staging environment
2. Conduct load testing with tracing enabled
3. Measure performance impact
4. Fine-tune sampling rates
5. Validate under production-like load

### Tasks

#### Week 5: Staging Deployment

**Day 1-2: SigNoz Staging**

```bash
# 1. Provision staging SigNoz infrastructure
# Staging specs (higher than dev):
# - ClickHouse: 8 vCPU, 16 GB RAM, 500 GB SSD
# - Query Service: 4 vCPU, 8 GB RAM
# - OTLP Collector: 4 vCPU, 8 GB RAM

# 2. Deploy SigNoz (same as dev, different hosts)
ssh staging-signoz
cd /opt/signoz
docker-compose up -d

# 3. Configure data retention
# Edit clickhouse-config.xml
<ttl>
    <traces>INTERVAL 90 DAY</traces>
</ttl>

# 4. Verify deployment
curl http://staging-signoz:4318
# Expected: 405 Method Not Allowed
```

**Day 3-5: Application Deployment**

```bash
# Deploy services to staging with OTel enabled

# API Gateway
./scripts/deployment/deploy-otel-java.sh api-gateway staging
systemctl restart api-gateway@staging

# Auth Service
./scripts/deployment/deploy-otel-java.sh gor-platform-auth staging
systemctl restart gor-platform-auth@staging

# GM Core
# Update sys.config to point to staging SigNoz
cd /opt/butler_server
vi config/staging.config
# Change otlp_endpoint to staging-signoz:4318
_build/staging/rel/butler_server/bin/butler_server restart
```

#### Week 6: Load Testing

**Day 6-7: Baseline Without Tracing**

```bash
# 1. Temporarily disable tracing
export OTEL_TRACES_SAMPLER=always_off

# 2. Run load tests
./scripts/testing/load-test.sh baseline
```

```bash
# scripts/testing/load-test.sh
#!/bin/bash

TEST_NAME=$1
DURATION=600  # 10 minutes
RATE=100      # requests per second

echo "Running load test: ${TEST_NAME}"

# Use Apache Bench, wrk, or Gatling
wrk -t12 -c400 -d${DURATION}s --latency \
    http://staging-gateway:8080/api-gateway/health \
    > results/${TEST_NAME}.txt

# Extract metrics
cat results/${TEST_NAME}.txt | grep -E "Latency|Requests/sec"
```

**Day 8-9: Load Test WITH Tracing (100% Sampling)**

```bash
# 1. Enable 100% sampling (worst case)
export OTEL_TRACES_SAMPLER=always_on

# 2. Restart services
systemctl restart api-gateway@staging
systemctl restart gor-platform-auth@staging

# 3. Run same load test
./scripts/testing/load-test.sh with-tracing-100

# 4. Compare results
./scripts/testing/compare-performance.sh baseline with-tracing-100
```

**Day 10: Load Test WITH Tracing (10% Sampling)**

```bash
# 1. Configure 10% sampling
export OTEL_TRACES_SAMPLER=traceidratio
export OTEL_TRACES_SAMPLER_ARG=0.1

# 2. Restart services
systemctl restart api-gateway@staging

# 3. Run load test
./scripts/testing/load-test.sh with-tracing-10

# 4. Final comparison
./scripts/testing/compare-performance.sh baseline with-tracing-10
```

#### Week 7: Analysis & Tuning

**Day 11-12: Performance Analysis**

```bash
# Generate performance report
./scripts/testing/generate-report.sh
```

```bash
# scripts/testing/generate-report.sh
#!/bin/bash

echo "===== OpenTelemetry Performance Impact Report ====="
echo ""
echo "Baseline (No Tracing):"
cat results/baseline.txt | grep -E "Latency|Requests/sec|Transfer/sec"
echo ""
echo "With Tracing (100% Sampling):"
cat results/with-tracing-100.txt | grep -E "Latency|Requests/sec|Transfer/sec"
echo ""
echo "With Tracing (10% Sampling):"
cat results/with-tracing-10.txt | grep -E "Latency|Requests/sec|Transfer/sec"
echo ""

# Calculate impact percentages
# ... (parsing and math logic)

echo "===== Summary ====="
echo "Impact with 100% sampling: X%"
echo "Impact with 10% sampling: Y%"
echo "Recommendation: Use 10% sampling in production"
```

**Day 13-14: Configuration Tuning**

Based on performance analysis:

1. **Adjust batch processor settings:**

```erlang
% Erlang: Increase batch size if export is slow
{otel_batch_processor, #{
    scheduled_delay_ms => 10000,  % Increase from 5s to 10s
    max_queue_size => 4096         % Increase from 2048 to 4096
}}.
```

```bash
# Java: Tune via environment variables
export OTEL_BSP_SCHEDULE_DELAY=10000
export OTEL_BSP_MAX_QUEUE_SIZE=4096
```

2. **Disable noisy instrumentations:**

```bash
# If database operations create too much noise
export OTEL_INSTRUMENTATION_JDBC_ENABLED=false
export OTEL_INSTRUMENTATION_SPRING_DATA_ENABLED=false
```

3. **Optimize sampling:**

```bash
# Start with 10% for all services
export OTEL_TRACES_SAMPLER=traceidratio
export OTEL_TRACES_SAMPLER_ARG=0.1

# High-traffic services: 5%
export OTEL_TRACES_SAMPLER_ARG=0.05

# Low-traffic services: 50%
export OTEL_TRACES_SAMPLER_ARG=0.5
```

### Phase 3 Success Criteria

- ✅ Staging environment fully instrumented
- ✅ Load tests completed successfully
- ✅ Performance impact < 2% with 10% sampling
- ✅ No memory leaks or stability issues
- ✅ SigNoz handles expected load
- ✅ Team confident in production readiness
- ✅ Go/No-Go decision for Phase 4

**Go/No-Go Decision Point:** End of Week 7

---

## Phase 4: Production Pilot (Week 8-9)

### Objectives

1. Deploy to 1-2 non-critical production services
2. Monitor for 2 weeks with 100% sampling
3. Gather team feedback
4. Validate production operations

### Tasks

#### Week 8: Pilot Service Selection & Deployment

**Day 1: Select Pilot Services**

**Criteria for pilot services:**
- ✅ Non-critical to business operations
- ✅ Moderate traffic (not highest or lowest)
- ✅ Representative of typical service patterns
- ✅ Good monitoring already in place

**Example pilot services:**
- Service 1: Internal admin API (low traffic, non-critical)
- Service 2: Background job processor (moderate traffic)

**Day 2-3: Production Deployment (Pilot Services)**

```bash
# 1. Create production-specific configuration
cat > /etc/default/admin-api-otel << EOF
export JAVA_TOOL_OPTIONS="-javaagent:/opt/observability/opentelemetry-javaagent.jar"
export OTEL_SERVICE_NAME="admin-api"
export OTEL_TRACES_EXPORTER="otlp"
export OTEL_EXPORTER_OTLP_ENDPOINT="http://signoz-prod:4318"
export OTEL_EXPORTER_OTLP_PROTOCOL="http/protobuf"
export OTEL_TRACES_SAMPLER="always_on"  # 100% for pilot
export OTEL_PROPAGATORS="tracecontext,baggage"
export OTEL_RESOURCE_ATTRIBUTES="environment=production,version=1.0.0,team=platform"
EOF

# 2. Deploy during maintenance window
# Schedule: Off-peak hours (e.g., 2 AM Sunday)
sudo systemctl edit admin-api
# Add: EnvironmentFile=/etc/default/admin-api-otel

# 3. Rolling restart (if load balanced)
# Instance 1:
sudo systemctl restart admin-api@instance1
sleep 60  # Wait for health check
# Instance 2:
sudo systemctl restart admin-api@instance2

# 4. Verify deployment
sudo journalctl -u admin-api -n 100 | grep -i opentelemetry
curl http://localhost:8080/health
# Check response headers for traceparent
```

**Day 4-5: Monitoring Setup**

```yaml
# monitoring/pilot-dashboards.yaml
dashboards:
  - name: "OTel Pilot Services"
    panels:
      - title: "Trace Volume"
        query: "rate(traces_total{service=~'admin-api|background-processor'}[5m])"

      - title: "Span Export Success Rate"
        query: "otel_exporter_success_total / otel_exporter_total"

      - title: "Service Latency (p95)"
        query: "histogram_quantile(0.95, http_request_duration_seconds{service=~'admin-api|background-processor'})"

      - title: "Memory Usage"
        query: "container_memory_usage_bytes{name=~'admin-api|background-processor'}"

      - title: "CPU Usage"
        query: "rate(container_cpu_usage_seconds_total{name=~'admin-api|background-processor'}[5m])"
```

```bash
# Create alerts
cat > monitoring/pilot-alerts.yaml << EOF
groups:
  - name: otel_pilot_alerts
    rules:
      - alert: PilotServiceHighLatency
        expr: histogram_quantile(0.95, http_request_duration_seconds{service=~"admin-api|background-processor"}) > 1
        for: 15m
        labels:
          severity: warning
        annotations:
          summary: "Pilot service p95 latency > 1s"

      - alert: PilotServiceHighMemory
        expr: (container_memory_usage_bytes{name=~"admin-api|background-processor"} / container_spec_memory_limit_bytes) > 0.9
        for: 10m
        labels:
          severity: critical
        annotations:
          summary: "Pilot service memory > 90%"
EOF
```

#### Week 9: Monitoring & Feedback

**Day 6-10: Daily Health Checks**

```bash
# Daily monitoring script
# scripts/monitoring/pilot-health-check.sh
#!/bin/bash

echo "===== Pilot Services Health Check ====="
echo "Date: $(date)"
echo ""

# Check services are up
echo "Service Status:"
systemctl status admin-api --no-pager | grep "Active:"
systemctl status background-processor --no-pager | grep "Active:"
echo ""

# Check trace volume
echo "Trace Statistics (last 24h):"
curl -s "http://signoz-prod:8080/api/v1/metrics?service=admin-api&duration=24h" | jq '.traces.count'
curl -s "http://signoz-prod:8080/api/v1/metrics?service=background-processor&duration=24h" | jq '.traces.count'
echo ""

# Check for errors
echo "Recent Errors (last 1h):"
curl -s "http://signoz-prod:8080/api/v1/traces?service=admin-api&status=error&duration=1h" | jq '.count'
echo ""

# Check memory usage
echo "Memory Usage:"
docker stats --no-stream admin-api | tail -n 1
docker stats --no-stream background-processor | tail -n 1
echo ""

echo "===== Report End ====="
```

```bash
# Run daily
crontab -e
# Add:
0 9 * * * /opt/scripts/monitoring/pilot-health-check.sh | mail -s "OTel Pilot Health Check" team@example.com
```

**Day 11-14: Team Feedback Collection**

```markdown
# Pilot Feedback Survey

## Service: _______________________
## Engineer: _____________________
## Date: __________________________

### Trace Visibility
1. Are traces helpful for debugging production issues?
   [ ] Very helpful  [ ] Somewhat helpful  [ ] Not helpful

2. How often do you use SigNoz to investigate issues?
   [ ] Daily  [ ] Weekly  [ ] Rarely  [ ] Never

3. Have you successfully traced a request end-to-end?
   [ ] Yes  [ ] No  [ ] Partially

### Performance
4. Have you noticed any performance degradation?
   [ ] Yes  [ ] No  [ ] Unsure

5. If yes, describe:
   _________________________________________________

### Usability
6. Is the SigNoz UI intuitive?
   [ ] Very intuitive  [ ] Somewhat  [ ] Not intuitive

7. What features do you use most?
   - [ ] Trace search
   - [ ] Service map
   - [ ] Latency metrics
   - [ ] Error tracking
   - [ ] Custom dashboards

### Issues
8. Have you encountered any bugs or issues?
   [ ] Yes  [ ] No

9. If yes, describe:
   _________________________________________________

### Overall Satisfaction
10. Would you recommend continuing the rollout?
    [ ] Yes  [ ] Yes with changes  [ ] No

11. Additional comments:
    _________________________________________________
```

### Phase 4 Success Criteria

- ✅ Pilot services stable for 2 weeks
- ✅ No critical production incidents caused by tracing
- ✅ Performance metrics within acceptable range (< 2% impact)
- ✅ Team feedback positive (> 80% satisfaction)
- ✅ At least 3 successful issue resolutions using traces
- ✅ Go/No-Go decision for Phase 5

**Go/No-Go Decision Point:** End of Week 9

---

## Phase 5: Production Rollout (Week 10-12)

### Objectives

1. Gradual rollout to all production services
2. Start with 10% sampling
3. Monitor system health continuously
4. Achieve 100% service coverage

### Tasks

#### Week 10: Critical Services

**Day 1-3: High-Priority Services**

Services: API Gateway, Auth Service

```bash
# Deployment during off-peak hours

# 1. Update configuration (10% sampling)
cat > /etc/default/api-gateway-otel << EOF
export JAVA_TOOL_OPTIONS="-javaagent:/opt/observability/opentelemetry-javaagent.jar"
export OTEL_SERVICE_NAME="api-gateway"
export OTEL_TRACES_EXPORTER="otlp"
export OTEL_EXPORTER_OTLP_ENDPOINT="http://signoz-prod:4318"
export OTEL_EXPORTER_OTLP_PROTOCOL="http/protobuf"
export OTEL_TRACES_SAMPLER="traceidratio"
export OTEL_TRACES_SAMPLER_ARG="0.1"  # 10% sampling
export OTEL_PROPAGATORS="tracecontext,baggage"
export OTEL_RESOURCE_ATTRIBUTES="environment=production"
EOF

# 2. Rolling deployment
# Deploy to 1 instance, monitor for 1 hour, then proceed
for instance in $(seq 1 4); do
    echo "Deploying to instance ${instance}..."
    ssh api-gateway-${instance} "systemctl restart api-gateway"
    echo "Waiting 1 hour for validation..."
    sleep 3600

    # Check for issues
    ERROR_COUNT=$(ssh api-gateway-${instance} "journalctl -u api-gateway --since '1 hour ago' | grep -i error | wc -l")
    if [ "$ERROR_COUNT" -gt 100 ]; then
        echo "ERROR: High error count detected. Halting rollout."
        exit 1
    fi
done
```

**Day 4-5: Monitor Critical Services**

```bash
# Intensive monitoring for first 48 hours
watch -n 60 './scripts/monitoring/production-health-check.sh'
```

#### Week 11: Remaining Services

**Day 6-8: Batch Rollout**

Group services by criticality and deploy in batches:

**Batch 1:** Core services (GM Core, Order Service)
**Batch 2:** Supporting services (Notification, Email)
**Batch 3:** Background jobs and workers

```bash
# scripts/deployment/batch-rollout.sh
#!/bin/bash

BATCH=$1
SERVICES_FILE="config/services-batch-${BATCH}.txt"

echo "Deploying batch ${BATCH}..."

while read service; do
    echo "Deploying ${service}..."
    ./scripts/deployment/deploy-otel-java.sh ${service} production

    # Restart service
    systemctl restart ${service}

    # Wait and validate
    sleep 300  # 5 minutes

    # Check health
    if ! systemctl is-active --quiet ${service}; then
        echo "ERROR: ${service} failed to start"
        exit 1
    fi

    echo "${service} deployed successfully"
done < "${SERVICES_FILE}"

echo "Batch ${BATCH} deployment complete"
```

```bash
# config/services-batch-1.txt
butler-server
order-service
payment-service

# config/services-batch-2.txt
notification-service
email-service
sms-service

# config/services-batch-3.txt
cleanup-job
report-generator
data-sync-worker
```

**Day 9-10: Validation & Issue Resolution**

- Monitor all services
- Address any issues
- Fine-tune sampling rates if needed

#### Week 12: Optimization & Documentation

**Day 11-12: Performance Optimization**

Based on 1-2 weeks of data:

```bash
# Analyze trace volume per service
./scripts/analysis/trace-volume-analysis.sh

# Adjust sampling for high-traffic services
# Example: If a service has > 10K req/s, reduce to 1%
export OTEL_TRACES_SAMPLER_ARG="0.01"
```

**Day 13-14: Final Documentation**

- Update runbooks with lessons learned
- Document any service-specific configurations
- Create troubleshooting guides
- Conduct post-mortem meeting

### Phase 5 Success Criteria

- ✅ All production services instrumented
- ✅ Trace volume within SigNoz capacity
- ✅ No degradation in service performance
- ✅ Team actively using traces for debugging
- ✅ Documentation complete and reviewed
- ✅ Monitoring and alerting operational

**Final Sign-Off:** End of Week 12

---

## Environment-Specific Configurations

### Development Environment

```bash
# Java services
export OTEL_SERVICE_NAME="${SERVICE_NAME}-dev"
export OTEL_EXPORTER_OTLP_ENDPOINT="http://localhost:4318"
export OTEL_TRACES_SAMPLER="always_on"  # 100% sampling
export OTEL_INSTRUMENTATION_JDBC_ENABLED="true"  # Keep DB tracing for debugging
export OTEL_RESOURCE_ATTRIBUTES="environment=dev"
```

```erlang
% Erlang services (config/dev.config)
{opentelemetry, [
    {service_name, <<"butler_server_dev">>},
    {sampler, {parent_based, #{root => {always_on}}}},
    {processors, [
        {otel_simple_processor, #{}}  % Immediate export for debugging
    ]}
]}.

{opentelemetry_exporter, [
    {otlp_endpoint, "http://localhost:4318"}
]}.
```

### Staging Environment

```bash
# Java services
export OTEL_SERVICE_NAME="${SERVICE_NAME}"
export OTEL_EXPORTER_OTLP_ENDPOINT="http://staging-signoz:4318"
export OTEL_TRACES_SAMPLER="always_on"  # 100% for testing
export OTEL_INSTRUMENTATION_JDBC_ENABLED="false"  # Disable DB noise
export OTEL_RESOURCE_ATTRIBUTES="environment=staging,version=${BUILD_VERSION}"
```

```erlang
% Erlang services (config/staging.config)
{opentelemetry, [
    {service_name, <<"butler_server_staging">>},
    {sampler, {parent_based, #{root => {always_on}}}},
    {processors, [
        {otel_batch_processor, #{
            scheduled_delay_ms => 5000,
            max_queue_size => 2048
        }}
    ]}
]}.

{opentelemetry_exporter, [
    {otlp_endpoint, "http://staging-signoz:4318"}
]}.
```

### Production Environment

```bash
# Java services
export OTEL_SERVICE_NAME="${SERVICE_NAME}"
export OTEL_EXPORTER_OTLP_ENDPOINT="http://signoz-prod:4318"
export OTEL_TRACES_SAMPLER="traceidratio"
export OTEL_TRACES_SAMPLER_ARG="0.1"  # 10% default
export OTEL_INSTRUMENTATION_JDBC_ENABLED="false"
export OTEL_INSTRUMENTATION_SPRING_DATA_ENABLED="false"
export OTEL_INSTRUMENTATION_HTTP_SERVER_IGNORE_PATHS="/actuator/*,/health,/metrics"
export OTEL_RESOURCE_ATTRIBUTES="environment=production,version=${BUILD_VERSION},datacenter=${DC}"
```

```erlang
% Erlang services (config/prod.config)
{opentelemetry, [
    {service_name, <<"butler_server">>},
    {sampler, {parent_based, #{
        root => {trace_id_ratio_based, 0.1}  % 10% sampling
    }}},
    {processors, [
        {otel_batch_processor, #{
            scheduled_delay_ms => 10000,  % Longer batch (10s)
            max_queue_size => 4096         % Larger queue
        }}
    ]}
]}.

{opentelemetry_exporter, [
    {otlp_endpoint, "http://signoz-prod:4318"},
    {otlp_compression, gzip}  % Enable compression for production
]}.
```

---

## Emergency Procedures

### Emergency Off Switch

**Scenario:** Tracing is causing production issues (high latency, memory leaks, etc.)

#### Option 1: Disable Sampling (Fastest)

```bash
# Java services
export OTEL_TRACES_SAMPLER="always_off"

# Restart service (or use dynamic config if available)
systemctl restart ${SERVICE_NAME}
```

```erlang
% Erlang services
% Update sys.config and reload
{opentelemetry, [
    {sampler, {parent_based, #{root => {always_off}}}}
]}.

% Reload config (requires app restart in most cases)
_build/prod/rel/butler_server/bin/butler_server restart
```

**Impact:**
- ✅ Traces stop immediately
- ✅ Performance returns to baseline
- ⚠️ Lose observability until re-enabled

#### Option 2: Reduce Sampling (Gradual)

```bash
# Reduce from 10% to 1%
export OTEL_TRACES_SAMPLER_ARG="0.01"
systemctl restart ${SERVICE_NAME}
```

**Impact:**
- ✅ Reduces load on system
- ✅ Maintains some visibility
- ⚠️ Less trace data available

#### Option 3: Remove Java Agent (Nuclear Option)

```bash
# Remove agent flag
unset JAVA_TOOL_OPTIONS

# Restart service
systemctl restart ${SERVICE_NAME}
```

**Impact:**
- ✅ Complete removal of OpenTelemetry
- ✅ Zero overhead
- ❌ Requires service restart
- ❌ Loses all tracing

### Emergency Contact List

| Issue Type                  | Contact          | Response Time |
| --------------------------- | ---------------- | ------------- |
| **SigNoz Down**             | DevOps Team      | 15 minutes    |
| **High Memory Usage**       | SRE Team         | 30 minutes    |
| **Tracing Bugs**            | Platform Team    | 1 hour        |
| **Critical Service Impact** | On-Call Engineer | Immediate     |

---

## Rollback Strategy

### Scenario 1: Development Rollback

```bash
# Simply disable tracing
export OTEL_TRACES_SAMPLER="always_off"
systemctl restart ${SERVICE_NAME}

# Or remove configuration
rm /etc/default/${SERVICE_NAME}-otel
systemctl restart ${SERVICE_NAME}
```

**Impact:** None (development environment)

### Scenario 2: Staging Rollback

```bash
# Disable tracing for all services
for service in $(cat config/staging-services.txt); do
    ssh staging-${service} "export OTEL_TRACES_SAMPLER=always_off && systemctl restart ${service}"
done

# Verify services are healthy
./scripts/monitoring/health-check-all.sh
```

**Impact:** Lose staging trace data (acceptable)

### Scenario 3: Production Pilot Rollback

```bash
# Emergency rollback script
# scripts/emergency/rollback-pilot.sh
#!/bin/bash
set -e

echo "===== EMERGENCY ROLLBACK: Pilot Services ====="

PILOT_SERVICES=("admin-api" "background-processor")

for service in "${PILOT_SERVICES[@]}"; do
    echo "Rolling back ${service}..."

    # Disable tracing
    ssh prod-${service} "export OTEL_TRACES_SAMPLER=always_off && systemctl restart ${service}"

    # Wait for health check
    sleep 60

    # Verify service is up
    if systemctl is-active --quiet ${service}; then
        echo "✅ ${service} rollback successful"
    else
        echo "❌ ${service} rollback FAILED - requires manual intervention"
    fi
done

echo "===== Rollback Complete ====="
```

**Decision criteria:**
- Critical production incident caused by tracing
- Performance degradation > 5%
- Memory leaks or crashes

### Scenario 4: Production Full Rollback

**ONLY if tracing causes widespread production issues**

```bash
# scripts/emergency/rollback-all-production.sh
#!/bin/bash
set -e

echo "===== EMERGENCY ROLLBACK: ALL PRODUCTION SERVICES ====="
echo "WARNING: This will disable tracing across all production services"
read -p "Are you sure? (yes/no): " confirm

if [ "$confirm" != "yes" ]; then
    echo "Rollback cancelled"
    exit 1
fi

# Disable sampling for all services
for host in $(cat config/production-hosts.txt); do
    echo "Disabling tracing on ${host}..."
    ssh ${host} "export OTEL_TRACES_SAMPLER=always_off && systemctl restart 'api-*'"
done

# Notify team
./scripts/notification/send-alert.sh "Production tracing disabled across all services"

echo "===== Rollback Complete ====="
```

**Post-rollback:**
1. Conduct incident post-mortem
2. Identify root cause
3. Fix issues in development
4. Re-validate in staging
5. Plan re-deployment

---

## Summary

### Rollout Timeline Recap

```
Week 1-2:   SigNoz deployed, team trained
Week 3-4:   Development environment instrumented
Week 5-7:   Staging validation and load testing
Week 8-9:   Production pilot (1-2 services)
Week 10-12: Full production rollout

Total: 12 weeks from start to completion
```

### Key Success Factors

- ✅ Phased approach with validation gates
- ✅ Comprehensive monitoring at each phase
- ✅ Emergency procedures defined
- ✅ Team training and buy-in
- ✅ Performance validation before production
- ✅ Gradual production rollout

### Post-Rollout

**Ongoing activities:**
- Weekly metrics review
- Monthly optimization sessions
- Quarterly strategic review
- Continuous team training

---

**Document Status:** ✅ Ready for Execution
**Last Updated:** November 2025
**Owner:** Platform Engineering Team

