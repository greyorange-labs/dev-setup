# OpenTelemetry + SigNoz Distributed Tracing
## Operations Runbook

**Document Version:** 1.0
**Last Updated:** November 2025
**Target Audience:** DevOps, SRE, On-Call Engineers

---

## Table of Contents

1. [Daily Operations](#daily-operations)
2. [Monitoring & Alerting](#monitoring--alerting)
3. [Performance Tuning](#performance-tuning)
4. [Emergency Procedures](#emergency-procedures)
5. [Troubleshooting Guide](#troubleshooting-guide)
6. [SigNoz Operations](#signoz-operations)
7. [Maintenance Tasks](#maintenance-tasks)
8. [Disaster Recovery](#disaster-recovery)

---

## Daily Operations

### Morning Health Check

**Execute daily at 9 AM:**

```bash
#!/bin/bash
# scripts/ops/morning-health-check.sh

echo "===== OpenTelemetry Daily Health Check ====="
echo "Date: $(date)"
echo ""

# 1. Check SigNoz Services
echo "=== SigNoz Status ==="
docker ps --filter "name=signoz" --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"
echo ""

# 2. Check Disk Usage
echo "=== Disk Usage (ClickHouse) ==="
df -h /var/lib/clickhouse | tail -1
echo ""

# 3. Trace Volume (Last 24h)
echo "=== Trace Volume (Last 24h) ==="
curl -s "http://localhost:8080/api/v1/metrics?duration=24h" | jq '{
  total_traces: .traces.count,
  total_spans: .spans.count,
  services: .services | length
}'
echo ""

# 4. Top Services by Trace Count
echo "=== Top 5 Services (by trace count) ==="
curl -s "http://localhost:8080/api/v1/services?duration=24h" | jq -r '.data[] | "\(.serviceName): \(.traceCount)"' | head -5
echo ""

# 5. Error Rate
echo "=== Error Rate (Last 1h) ==="
curl -s "http://localhost:8080/api/v1/metrics?duration=1h&status=error" | jq '{
  error_traces: .traces.count,
  error_rate: .error_rate
}'
echo ""

# 6. Export Success Rate
echo "=== OTLP Export Success Rate ==="
curl -s "http://localhost:9090/api/v1/query?query=rate(otel_exporter_success_total[5m])" | jq '.data.result[0].value[1]'
echo ""

echo "===== Health Check Complete ====="
```

**Expected Results:**
- ✅ All SigNoz containers running
- ✅ Disk usage < 80%
- ✅ Trace volume consistent with traffic
- ✅ Error rate < 5%
- ✅ Export success rate > 99%

**Actions if unhealthy:**
- Disk usage > 80% → See [Disk Space Management](#disk-space-management)
- High error rate → See [High Error Rate](#high-error-rate)
- Export failures → See [Export Failures](#export-failures)

### Weekly Tasks

**Every Monday:**

```bash
# 1. Review storage growth
./scripts/ops/storage-growth-report.sh

# 2. Check for application updates
docker exec signoz-clickhouse clickhouse-client --query "SELECT version()"
docker exec signoz-otel-collector /otelcol --version

# 3. Review alert history
./scripts/ops/alert-summary.sh --days 7

# 4. Validate backups
./scripts/ops/validate-backups.sh --latest

# 5. Performance report
./scripts/ops/weekly-performance-report.sh
```

### Monthly Tasks

**First Monday of each month:**

```bash
# 1. Update OpenTelemetry agents (if needed)
./scripts/ops/check-otel-updates.sh

# 2. ClickHouse optimization
docker exec signoz-clickhouse clickhouse-client --query "OPTIMIZE TABLE signoz_traces.signoz_index_v2 FINAL"

# 3. Review and adjust sampling rates
./scripts/ops/sampling-rate-analysis.sh

# 4. Capacity planning review
./scripts/ops/capacity-planning-report.sh

# 5. Security audit
./scripts/ops/security-audit.sh
```

---

## Monitoring & Alerting

### Critical Alerts

#### Alert 1: SigNoz OTLP Receiver Down

```yaml
alert: SigNozOTLPReceiverDown
expr: up{job="signoz-otel-collector"} == 0
for: 5m
severity: critical
```

**Impact:** Applications cannot export traces

**Response:**

```bash
# 1. Check container status
docker ps -a | grep signoz-otel-collector

# 2. Check logs
docker logs signoz-otel-collector --tail 100

# 3. Restart if needed
docker restart signoz-otel-collector

# 4. Verify endpoint
curl http://localhost:4318/v1/traces
# Expected: 405 Method Not Allowed (means it's up)

# 5. If still down, check ClickHouse connection
docker exec signoz-otel-collector cat /etc/otel-collector/config.yaml
# Verify ClickHouse endpoint is correct
```

#### Alert 2: High Memory Usage

```yaml
alert: SigNozHighMemoryUsage
expr: (container_memory_usage_bytes{name=~"signoz.*"} / container_spec_memory_limit_bytes{name=~"signoz.*"}) > 0.9
for: 10m
severity: warning
```

**Response:**

```bash
# 1. Identify which component
docker stats --no-stream | grep signoz

# 2. Check for memory leaks
docker exec signoz-clickhouse clickhouse-client --query "
  SELECT
    formatReadableSize(sum(bytes)) AS size,
    database
  FROM system.parts
  GROUP BY database
  ORDER BY size DESC
"

# 3. If ClickHouse, optimize tables
docker exec signoz-clickhouse clickhouse-client --query "
  OPTIMIZE TABLE signoz_traces.signoz_index_v2 FINAL
"

# 4. If OTel Collector, reduce batch size
# Edit otel-collector-config.yaml:
processors:
  batch:
    send_batch_size: 512      # Reduce from 1024
    timeout: 10s

# 5. Restart affected container
docker restart <container-name>
```

#### Alert 3: Disk Space Low

```yaml
alert: SigNozDiskSpaceLow
expr: (node_filesystem_avail_bytes{mountpoint="/var/lib/clickhouse"} / node_filesystem_size_bytes{mountpoint="/var/lib/clickhouse"}) < 0.2
for: 5m
severity: warning
```

**Response:**

```bash
# 1. Check current usage
df -h /var/lib/clickhouse

# 2. Check table sizes
docker exec signoz-clickhouse clickhouse-client --query "
  SELECT
    table,
    formatReadableSize(sum(bytes)) AS size,
    sum(rows) AS rows,
    max(modification_time) AS latest_data
  FROM system.parts
  WHERE database = 'signoz_traces'
  GROUP BY table
  ORDER BY size DESC
"

# 3. Delete old data (if TTL not working)
# Delete traces older than 90 days
docker exec signoz-clickhouse clickhouse-client --query "
  ALTER TABLE signoz_traces.signoz_index_v2
  DELETE WHERE timestamp < now() - INTERVAL 90 DAY
"

# 4. Optimize tables
docker exec signoz-clickhouse clickhouse-client --query "
  OPTIMIZE TABLE signoz_traces.signoz_index_v2 FINAL
"

# 5. Verify space freed
df -h /var/lib/clickhouse
```

#### Alert 4: High Error Rate

```yaml
alert: HighApplicationErrorRate
expr: (sum(rate(traces_total{status="error"}[5m])) / sum(rate(traces_total[5m]))) > 0.1
for: 15m
severity: warning
```

**Response:**

```bash
# 1. Identify services with errors
curl -s "http://localhost:8080/api/v1/services?duration=1h&status=error" | jq -r '.data[] | "\(.serviceName): \(.errorRate)"'

# 2. View recent error traces
# Open SigNoz UI → Traces → Filter by Status: Error

# 3. Check for patterns
# Look for:
# - Common error messages
# - Specific services
# - Time-based patterns

# 4. Notify application teams
./scripts/ops/notify-team.sh --service <service-name> --alert high-error-rate
```

### Monitoring Dashboards

#### Dashboard 1: SigNoz Health

**Metrics to monitor:**

| Metric                   | Threshold       | Action                        |
| ------------------------ | --------------- | ----------------------------- |
| **OTLP Receiver Uptime** | < 99.9%         | Investigate outages           |
| **Memory Usage**         | > 90%           | Scale up or optimize          |
| **Disk Usage**           | > 80%           | Clean old data or add storage |
| **CPU Usage**            | > 80% sustained | Scale up                      |
| **Query Latency (p95)**  | > 5s            | Optimize queries or scale     |

#### Dashboard 2: Application Tracing Health

**Metrics to monitor:**

| Metric                  | Threshold         | Action                 |
| ----------------------- | ----------------- | ---------------------- |
| **Trace Volume**        | Unexpected drop   | Check applications     |
| **Export Success Rate** | < 99%             | Check network/config   |
| **Avg Spans per Trace** | Unexpected change | Verify instrumentation |
| **Sampling Rate**       | Not as configured | Check config           |

---

## Performance Tuning

### Tuning SigNoz

#### ClickHouse Optimization

```sql
-- 1. Optimize tables (run weekly)
OPTIMIZE TABLE signoz_traces.signoz_index_v2 FINAL;
OPTIMIZE TABLE signoz_traces.signoz_span_attributes FINAL;

-- 2. Check query performance
SELECT
    query,
    elapsed,
    read_rows,
    formatReadableSize(read_bytes) AS read_bytes,
    formatReadableSize(memory_usage) AS memory_usage
FROM system.query_log
WHERE type = 'QueryFinish'
  AND event_time > now() - INTERVAL 1 HOUR
ORDER BY elapsed DESC
LIMIT 10;

-- 3. Analyze slow queries
SELECT
    query,
    query_duration_ms,
    query_start_time
FROM system.query_log
WHERE query_duration_ms > 5000  -- Queries > 5 seconds
  AND event_time > now() - INTERVAL 1 DAY
ORDER BY query_duration_ms DESC
LIMIT 20;
```

#### OTLP Collector Tuning

```yaml
# /etc/otel-collector/config.yaml

receivers:
  otlp:
    protocols:
      http:
        endpoint: "0.0.0.0:4318"
        max_request_body_size: 4194304  # 4 MB

processors:
  batch:
    send_batch_size: 1024      # Increase for high-volume
    timeout: 10s
    send_batch_max_size: 2048

  memory_limiter:
    check_interval: 1s
    limit_mib: 1536            # 1.5 GB
    spike_limit_mib: 512

exporters:
  clickhouse:
    endpoint: tcp://clickhouse:9000
    database: signoz_traces
    ttl_days: 90
    timeout: 10s
    sending_queue:
      enabled: true
      num_consumers: 10        # Increase for throughput
      queue_size: 5000
    retry_on_failure:
      enabled: true
      initial_interval: 5s
      max_interval: 30s
      max_elapsed_time: 300s

service:
  pipelines:
    traces:
      receivers: [otlp]
      processors: [memory_limiter, batch]
      exporters: [clickhouse]
```

### Tuning Application Services

#### Java Services

```bash
# High-traffic service (>1000 req/s)
export OTEL_TRACES_SAMPLER=traceidratio
export OTEL_TRACES_SAMPLER_ARG=0.01  # 1% sampling

# Increase batch size
export OTEL_BSP_MAX_QUEUE_SIZE=4096
export OTEL_BSP_MAX_EXPORT_BATCH_SIZE=1024
export OTEL_BSP_SCHEDULE_DELAY=10000  # 10 seconds

# Disable noisy instrumentations
export OTEL_INSTRUMENTATION_JDBC_ENABLED=false
export OTEL_INSTRUMENTATION_SPRING_DATA_ENABLED=false
```

#### Erlang Services

```erlang
% High-traffic service configuration
{opentelemetry, [
    {sampler, {parent_based, #{
        root => {trace_id_ratio_based, 0.01}  % 1% sampling
    }}},
    {processors, [
        {otel_batch_processor, #{
            scheduled_delay_ms => 15000,     % 15 seconds
            max_queue_size => 8192,          % Large queue
            exporting_timeout_ms => 60000    % 60 second timeout
        }}
    ]}
]}.
```

### Sampling Rate Optimization

**Decision matrix:**

| Service Type          | Request Rate   | Recommended Sampling |
| --------------------- | -------------- | -------------------- |
| **Low-traffic**       | < 10 req/s     | 50-100%              |
| **Medium-traffic**    | 10-100 req/s   | 10-20%               |
| **High-traffic**      | 100-1000 req/s | 5-10%                |
| **Very high-traffic** | > 1000 req/s   | 1-5%                 |

```bash
# Script to analyze and recommend sampling rates
# scripts/ops/recommend-sampling-rates.sh
#!/bin/bash

echo "===== Sampling Rate Recommendations ====="

# Get request rates per service (last 24h)
curl -s "http://localhost:8080/api/v1/services?duration=24h" | jq -r '.data[] |
  "\(.serviceName),\(.requestRate),\(.currentSamplingRate)"' | while IFS=',' read -r service rate current; do

    if (( $(echo "$rate < 10" | bc -l) )); then
        recommended="0.5"  # 50%
    elif (( $(echo "$rate < 100" | bc -l) )); then
        recommended="0.1"  # 10%
    elif (( $(echo "$rate < 1000" | bc -l) )); then
        recommended="0.05" # 5%
    else
        recommended="0.01" # 1%
    fi

    if [ "$current" != "$recommended" ]; then
        echo "⚠️  $service: Current=$current, Recommended=$recommended, Rate=${rate} req/s"
    else
        echo "✅ $service: Optimal sampling rate ($current)"
    fi
done
```

---

## Emergency Procedures

### Emergency: Disable Tracing Completely

**Scenario:** Tracing is causing critical production issues

#### Option 1: Disable at Application Level (Fastest)

```bash
#!/bin/bash
# scripts/emergency/disable-tracing-all.sh

echo "===== EMERGENCY: Disabling Tracing on All Services ====="

# Java services
for host in $(cat /etc/production-hosts-java.txt); do
    echo "Disabling tracing on $host..."
    ssh $host "export OTEL_TRACES_SAMPLER=always_off && systemctl restart 'java-*'"
done

# Erlang services
for host in $(cat /etc/production-hosts-erlang.txt); do
    echo "Disabling tracing on $host..."
    ssh $host "_build/prod/rel/*/bin/* eval 'application:set_env(opentelemetry, sampler, {parent_based, #{root => {always_off}}})'"
done

echo "===== Tracing Disabled Across All Services ====="
echo "Send notification to team..."
./scripts/ops/send-alert.sh "EMERGENCY: Tracing disabled production-wide"
```

**Verification:**

```bash
# Check that no traces are being exported
watch -n 5 'curl -s "http://localhost:8080/api/v1/metrics?duration=5m" | jq .traces.count'
# Should show 0 new traces
```

#### Option 2: Stop SigNoz OTLP Receiver

**Use only if application-level disable doesn't work:**

```bash
# Stop OTLP receiver (applications will buffer or drop spans)
docker stop signoz-otel-collector

# Applications will continue running normally
# Traces will be dropped (buffered temporarily with backpressure)

# Verify applications are healthy
./scripts/ops/check-app-health.sh
```

### Emergency: High Memory Usage

**Scenario:** SigNoz components consuming excessive memory

```bash
#!/bin/bash
# scripts/emergency/reduce-memory-usage.sh

echo "===== EMERGENCY: Reducing Memory Usage ====="

# 1. Reduce OTLP Collector memory
docker exec signoz-otel-collector pkill -SIGUSR1 otelcol
# Triggers garbage collection

# 2. Reduce batch sizes
cat > /tmp/otel-config-emergency.yaml << 'EOF'
processors:
  batch:
    send_batch_size: 256       # Reduced from 1024
    timeout: 5s                # Reduced from 10s
EOF

docker cp /tmp/otel-config-emergency.yaml signoz-otel-collector:/etc/otel-collector/config.yaml
docker restart signoz-otel-collector

# 3. Optimize ClickHouse
docker exec signoz-clickhouse clickhouse-client --query "
  SYSTEM DROP MARK CACHE;
  SYSTEM DROP UNCOMPRESSED CACHE;
"

# 4. Monitor recovery
watch -n 10 'docker stats --no-stream | grep signoz'
```

### Emergency: Disk Space Critical

**Scenario:** Disk usage > 95%

```bash
#!/bin/bash
# scripts/emergency/free-disk-space.sh

echo "===== EMERGENCY: Freeing Disk Space ====="

# 1. Check current usage
df -h /var/lib/clickhouse

# 2. Drop old data immediately (last resort)
docker exec signoz-clickhouse clickhouse-client --query "
  ALTER TABLE signoz_traces.signoz_index_v2
  DELETE WHERE timestamp < now() - INTERVAL 30 DAY
"

# 3. Optimize tables
docker exec signoz-clickhouse clickhouse-client --query "
  OPTIMIZE TABLE signoz_traces.signoz_index_v2 FINAL
"

# 4. Check freed space
df -h /var/lib/clickhouse

# 5. If still critical, reduce sampling
export OTEL_TRACES_SAMPLER_ARG=0.01  # Reduce to 1%
# Apply to all services
./scripts/ops/update-sampling-all-services.sh 0.01
```

### Emergency Contact List

| Escalation Level            | Contact                   | Response Time |
| --------------------------- | ------------------------- | ------------- |
| **L1: On-Call SRE**         | sre-oncall@company.com    | 15 minutes    |
| **L2: Platform Team Lead**  | platform-lead@company.com | 30 minutes    |
| **L3: Engineering Manager** | eng-manager@company.com   | 1 hour        |
| **L4: VP Engineering**      | vp-eng@company.com        | 2 hours       |

---

## Troubleshooting Guide

### Issue: Traces Not Appearing

**Symptoms:**
- Service is running
- No traces in SigNoz UI

**Troubleshooting steps:**

```bash
# 1. Check service configuration
# Java:
env | grep OTEL

# Erlang:
_build/prod/rel/*/bin/* eval 'application:get_all_env(opentelemetry)'

# 2. Check SigNoz OTLP endpoint
curl http://signoz-host:4318/v1/traces
# Expected: 405 Method Not Allowed

# 3. Test trace export manually
# Java:
-Dotel.javaagent.debug=true

# Erlang: Check logs
tail -f _build/prod/rel/*/log/erlang.log.* | grep otel

# 4. Check network connectivity
telnet signoz-host 4318

# 5. Verify firewall rules
iptables -L -n | grep 4318
```

**Common causes:**
- ❌ Wrong OTLP endpoint configured
- ❌ Firewall blocking port 4318
- ❌ SigNoz OTLP receiver down
- ❌ Sampling set to `always_off`
- ❌ Application not instrumented correctly

### Issue: Broken Trace Context

**Symptoms:**
- Multiple disconnected traces instead of one
- Missing parent-child relationships

**Troubleshooting steps:**

```bash
# 1. Verify propagators are configured
# Java:
echo $OTEL_PROPAGATORS
# Expected: tracecontext,baggage

# Erlang:
grep text_map_propagators config/sys.config
# Expected: [trace_context, baggage]

# 2. Check HTTP headers
curl -v http://service:8080/api/test 2>&1 | grep traceparent
# Should see traceparent header in request AND response

# 3. Test with explicit trace context
TRACE_ID=$(openssl rand -hex 16)
SPAN_ID=$(openssl rand -hex 8)
curl -H "traceparent: 00-${TRACE_ID}-${SPAN_ID}-01" http://service:8080/api/test

# Check SigNoz for trace_id: $TRACE_ID
# All services should use same trace_id

# 4. Erlang-specific: Check context attachment
# Ensure otel_ctx:attach() is called after extraction
grep -r "otel_ctx:attach" src/
```

**Common causes:**
- ❌ Propagators not configured
- ❌ Context not extracted from headers (Erlang)
- ❌ Context not attached to current process (Erlang)
- ❌ Intermediate proxy stripping headers

### Issue: High Latency

**Symptoms:**
- Application latency increased after enabling tracing

**Troubleshooting steps:**

```bash
# 1. Check sampling rate
# If 100%, reduce to 10%
export OTEL_TRACES_SAMPLER=traceidratio
export OTEL_TRACES_SAMPLER_ARG=0.1

# 2. Check batch processor settings
# Increase batch delay to reduce frequency
export OTEL_BSP_SCHEDULE_DELAY=15000  # 15 seconds

# 3. Disable noisy instrumentations
export OTEL_INSTRUMENTATION_JDBC_ENABLED=false

# 4. Profile application
# Java: Enable GC logging
-Xlog:gc*:file=/var/log/gc.log

# 5. Check OTLP export latency
curl -s "http://localhost:9090/api/v1/query?query=otel_exporter_export_duration_milliseconds" | jq .
```

**Solutions:**
- ✅ Reduce sampling rate
- ✅ Increase batch delay
- ✅ Disable specific instrumentations
- ✅ Optimize span attributes (fewer attributes)

### Issue: Memory Leaks

**Symptoms:**
- Memory usage grows continuously
- OOM errors

**Troubleshooting steps:**

```bash
# 1. Monitor memory over time
# Java:
jstat -gc <pid> 1000 10
# Watch for heap growth

# Erlang:
_build/prod/rel/*/bin/* eval 'erlang:memory().'
# Watch for process memory growth

# 2. Check span queue size
# If queue is full, spans aren't being exported

# 3. Heap dump (Java)
jmap -dump:live,format=b,file=/tmp/heapdump.hprof <pid>
# Analyze with jhat or VisualVM

# 4. Erlang process inspection
_build/prod/rel/*/bin/* eval 'erlang:processes().' | wc -l
# High process count indicates leak

# 5. Check for stuck exporters
docker logs signoz-otel-collector | grep -i error
```

**Solutions:**
- ✅ Reduce max_queue_size
- ✅ Increase export frequency
- ✅ Check network connectivity to SigNoz
- ✅ Add memory_limiter processor

---

## SigNoz Operations

### Backup Procedures

```bash
#!/bin/bash
# scripts/ops/backup-signoz.sh

BACKUP_DIR="/backups/signoz/$(date +%Y%m%d)"
mkdir -p "$BACKUP_DIR"

echo "===== Backing up SigNoz ====="

# 1. Backup ClickHouse data
echo "Backing up ClickHouse..."
docker exec signoz-clickhouse clickhouse-client --query "
  BACKUP DATABASE signoz_traces TO Disk('backups', '${BACKUP_DIR}/clickhouse')
"

# 2. Backup configurations
echo "Backing up configurations..."
cp -r /opt/signoz/config "$BACKUP_DIR/"
cp -r /opt/signoz/docker-compose.yaml "$BACKUP_DIR/"

# 3. Backup dashboards
echo "Backing up dashboards..."
docker exec signoz-frontend tar czf /tmp/dashboards.tar.gz /var/lib/signoz/dashboards
docker cp signoz-frontend:/tmp/dashboards.tar.gz "$BACKUP_DIR/"

# 4. Verify backup
echo "Verifying backup..."
ls -lh "$BACKUP_DIR"

# 5. Upload to S3 (optional)
aws s3 cp "$BACKUP_DIR" "s3://company-backups/signoz/$(date +%Y%m%d)/" --recursive

echo "===== Backup Complete ====="
```

**Backup schedule:**
- Daily: Last 7 days
- Weekly: Last 4 weeks
- Monthly: Last 12 months

### Restore Procedures

```bash
#!/bin/bash
# scripts/ops/restore-signoz.sh

BACKUP_DATE=$1

if [ -z "$BACKUP_DATE" ]; then
    echo "Usage: $0 <backup-date (YYYYMMDD)>"
    exit 1
fi

BACKUP_DIR="/backups/signoz/$BACKUP_DATE"

echo "===== Restoring SigNoz from $BACKUP_DATE ====="

# 1. Stop SigNoz
docker-compose -f /opt/signoz/docker-compose.yaml down

# 2. Restore ClickHouse data
docker run --rm \
    -v "$BACKUP_DIR/clickhouse:/backup" \
    -v clickhouse-data:/var/lib/clickhouse \
    clickhouse/clickhouse-server:23.11 \
    bash -c "cp -r /backup/* /var/lib/clickhouse/"

# 3. Restore configurations
cp -r "$BACKUP_DIR/config/" /opt/signoz/
cp "$BACKUP_DIR/docker-compose.yaml" /opt/signoz/

# 4. Start SigNoz
docker-compose -f /opt/signoz/docker-compose.yaml up -d

# 5. Verify
sleep 30
curl http://localhost:8080/api/health
curl http://localhost:4318

echo "===== Restore Complete ====="
```

### Upgrade Procedures

```bash
#!/bin/bash
# scripts/ops/upgrade-signoz.sh

NEW_VERSION=$1

if [ -z "$NEW_VERSION" ]; then
    echo "Usage: $0 <new-version>"
    exit 1
fi

echo "===== Upgrading SigNoz to $NEW_VERSION ====="

# 1. Backup current installation
./scripts/ops/backup-signoz.sh

# 2. Download new version
cd /opt/signoz
git fetch --tags
git checkout $NEW_VERSION

# 3. Update docker-compose.yaml
sed -i "s/:.*/:${NEW_VERSION}/g" docker-compose.yaml

# 4. Pull new images
docker-compose pull

# 5. Stop old version
docker-compose down

# 6. Start new version
docker-compose up -d

# 7. Verify upgrade
sleep 60
curl http://localhost:8080/api/health

# 8. Check logs for errors
docker-compose logs --tail 100

echo "===== Upgrade Complete ====="
```

### Scaling SigNoz

#### Horizontal Scaling: ClickHouse Cluster

```yaml
# docker-compose-cluster.yaml
version: "3.8"

services:
  clickhouse-01:
    image: clickhouse/clickhouse-server:23.11
    hostname: clickhouse-01
    volumes:
      - clickhouse-01-data:/var/lib/clickhouse
    environment:
      - CLICKHOUSE_CLUSTER_NAME=signoz_cluster
    ports:
      - "9001:9000"

  clickhouse-02:
    image: clickhouse/clickhouse-server:23.11
    hostname: clickhouse-02
    volumes:
      - clickhouse-02-data:/var/lib/clickhouse
    environment:
      - CLICKHOUSE_CLUSTER_NAME=signoz_cluster
    ports:
      - "9002:9000"

  clickhouse-03:
    image: clickhouse/clickhouse-server:23.11
    hostname: clickhouse-03
    volumes:
      - clickhouse-03-data:/var/lib/clickhouse
    environment:
      - CLICKHOUSE_CLUSTER_NAME=signoz_cluster
    ports:
      - "9003:9000"

volumes:
  clickhouse-01-data:
  clickhouse-02-data:
  clickhouse-03-data:
```

#### Vertical Scaling: Increase Resources

```yaml
# docker-compose.yaml
services:
  clickhouse:
    deploy:
      resources:
        limits:
          cpus: '8'
          memory: 32G
        reservations:
          cpus: '4'
          memory: 16G

  otel-collector:
    deploy:
      resources:
        limits:
          cpus: '4'
          memory: 8G
        reservations:
          cpus: '2'
          memory: 4G
```

---

## Maintenance Tasks

### Disk Space Management

```bash
#!/bin/bash
# scripts/ops/manage-disk-space.sh

echo "===== Disk Space Management ====="

# 1. Check current usage
df -h /var/lib/clickhouse

# 2. Check table sizes
docker exec signoz-clickhouse clickhouse-client --query "
  SELECT
    database,
    table,
    formatReadableSize(sum(bytes)) AS size,
    sum(rows) AS rows
  FROM system.parts
  WHERE active
  GROUP BY database, table
  ORDER BY sum(bytes) DESC
"

# 3. Set/Verify TTL
docker exec signoz-clickhouse clickhouse-client --query "
  ALTER TABLE signoz_traces.signoz_index_v2
  MODIFY TTL timestamp + INTERVAL 90 DAY
"

# 4. Delete old data manually (if needed)
docker exec signoz-clickhouse clickhouse-client --query "
  ALTER TABLE signoz_traces.signoz_index_v2
  DELETE WHERE timestamp < now() - INTERVAL 120 DAY
"

# 5. Optimize tables
docker exec signoz-clickhouse clickhouse-client --query "
  OPTIMIZE TABLE signoz_traces.signoz_index_v2 FINAL
"
```

### Log Rotation

```bash
# /etc/logrotate.d/signoz
/var/lib/docker/containers/*/*.log {
    rotate 7
    daily
    compress
    size=100M
    missingok
    delaycompress
    copytruncate
}
```

### Security Updates

```bash
#!/bin/bash
# scripts/ops/security-updates.sh

echo "===== Checking for Security Updates ====="

# 1. Check Docker image vulnerabilities
docker scan signoz/clickhouse:latest
docker scan signoz/otel-collector:latest
docker scan signoz/query-service:latest

# 2. Update base images
docker pull clickhouse/clickhouse-server:23.11
docker pull signoz/signoz-otel-collector:latest

# 3. Recreate containers
docker-compose up -d --force-recreate

# 4. Verify services
./scripts/ops/morning-health-check.sh
```

---

## Disaster Recovery

### Scenarios & Procedures

#### Scenario 1: Complete SigNoz Loss

**Recovery steps:**

```bash
# 1. Provision new infrastructure
# 2. Restore from latest backup
./scripts/ops/restore-signoz.sh $(ls -t /backups/signoz/ | head -1)

# 3. Update application endpoints
# Update all services to point to new SigNoz host
./scripts/ops/update-otel-endpoints.sh new-signoz-host

# 4. Verify traces flowing
sleep 60
./scripts/ops/morning-health-check.sh

# 5. Notify team
./scripts/ops/send-alert.sh "SigNoz restored from backup"
```

**Expected RTO (Recovery Time Objective):** 1-2 hours
**Expected RPO (Recovery Point Objective):** 24 hours (last backup)

#### Scenario 2: Corrupted ClickHouse Data

**Recovery steps:**

```bash
# 1. Identify corrupted table
docker exec signoz-clickhouse clickhouse-client --query "
  CHECK TABLE signoz_traces.signoz_index_v2
"

# 2. Try to repair
docker exec signoz-clickhouse clickhouse-client --query "
  SYSTEM RESTORE REPLICA signoz_traces.signoz_index_v2
"

# 3. If repair fails, restore from backup
./scripts/ops/restore-clickhouse-table.sh signoz_index_v2

# 4. Verify data integrity
docker exec signoz-clickhouse clickhouse-client --query "
  SELECT count() FROM signoz_traces.signoz_index_v2
"
```

#### Scenario 3: Network Partition

**Impact:** Applications cannot reach SigNoz

**Response:**

```bash
# 1. Applications buffer spans (temporary)
# Check application memory usage
./scripts/ops/check-app-memory.sh

# 2. If memory issues, temporarily disable tracing
export OTEL_TRACES_SAMPLER=always_off
./scripts/ops/restart-all-services.sh

# 3. Fix network issue

# 4. Re-enable tracing
export OTEL_TRACES_SAMPLER=traceidratio
export OTEL_TRACES_SAMPLER_ARG=0.1
./scripts/ops/restart-all-services.sh
```

---

## Summary

### Daily Checklist

- [ ] Run morning health check
- [ ] Check disk usage
- [ ] Review error rate
- [ ] Verify export success rate
- [ ] Check for alerts

### Weekly Checklist

- [ ] Storage growth report
- [ ] Review alert history
- [ ] Validate backups
- [ ] Performance report
- [ ] Optimize ClickHouse tables

### Monthly Checklist

- [ ] Check for updates
- [ ] Security audit
- [ ] Capacity planning review
- [ ] Sampling rate optimization
- [ ] Team feedback session

### Emergency Contacts

- **On-Call SRE:** sre-oncall@company.com (15 min)
- **Platform Lead:** platform-lead@company.com (30 min)
- **Eng Manager:** eng-manager@company.com (1 hour)

---

**Document Status:** ✅ Ready for Operations
**Last Updated:** November 2025
**Maintainer:** SRE Team

