# Operational Guide: Distributed Tracing System

## Table of Contents
1. [Daily Operations](#daily-operations)
2. [Monitoring the Monitoring](#monitoring-the-monitoring)
3. [Incident Response](#incident-response)
4. [Maintenance Procedures](#maintenance-procedures)
5. [Troubleshooting](#troubleshooting)
6. [Performance Tuning](#performance-tuning)
7. [Cost Management](#cost-management)
8. [Best Practices](#best-practices)

---

## 1. Daily Operations

### 1.1 Morning Health Check

**Time**: 9:00 AM daily
**Owner**: SRE on-call
**Duration**: 10 minutes

```bash
#!/bin/bash
# daily-health-check.sh

echo "🔍 Distributed Tracing Health Check - $(date)"
echo "================================================"

# 1. Check all pods are running
echo -e "\n📦 Pod Status:"
kubectl get pods -n observability | grep -v Running | grep -v Completed || echo "✅ All pods healthy"

# 2. Check Elasticsearch cluster health
echo -e "\n🔍 Elasticsearch Health:"
ES_HEALTH=$(kubectl exec -n observability elasticsearch-master-0 -- curl -s http://localhost:9200/_cluster/health | jq -r '.status')
if [ "$ES_HEALTH" == "green" ]; then
    echo "✅ Elasticsearch: $ES_HEALTH"
else
    echo "⚠️  Elasticsearch: $ES_HEALTH - ACTION REQUIRED"
fi

# 3. Check Elasticsearch disk usage
echo -e "\n💾 Disk Usage:"
kubectl exec -n observability elasticsearch-master-0 -- df -h | grep -E '/usr/share/elasticsearch/data|Filesystem'

# 4. Check Jaeger collector ingestion rate
echo -e "\n📊 Jaeger Ingestion (last 5 min):"
kubectl exec -n observability jaeger-collector-0 -- curl -s http://localhost:14269/metrics | grep jaeger_collector_spans_received_total | tail -1

# 5. Check OTel Collector CPU/Memory
echo -e "\n🖥️  OTel Collector Resources:"
kubectl top pods -n observability -l app=otel-collector-gateway | head -3

# 6. Check Kafka lag (if using Kafka)
echo -e "\n📨 Kafka Consumer Lag:"
kubectl exec -n observability kafka-0 -- kafka-consumer-groups.sh \
  --describe --group jaeger-ingester --bootstrap-server localhost:9092 2>/dev/null | grep jaeger-spans | awk '{print $6}'

# 7. Test trace query
echo -e "\n🔎 Query Test:"
RESPONSE_TIME=$(curl -s -o /dev/null -w "%{time_total}" "http://jaeger-query.observability:16686/api/services")
echo "Query response time: ${RESPONSE_TIME}s"
if (( $(echo "$RESPONSE_TIME < 0.5" | bc -l) )); then
    echo "✅ Query performance OK"
else
    echo "⚠️  Query performance degraded - ACTION REQUIRED"
fi

echo -e "\n================================================"
echo "Health check complete!"
```

**Action Items**:
- ✅ Green: No action required
- ⚠️  Yellow: Monitor closely, investigate during business hours
- 🔴 Red: Immediate action required

### 1.2 Weekly Maintenance

**Time**: Saturday 2:00 AM (off-peak)
**Owner**: Platform team
**Duration**: 30 minutes

```bash
#!/bin/bash
# weekly-maintenance.sh

echo "🔧 Weekly Maintenance - $(date)"

# 1. Force merge old Elasticsearch indices (improve query performance)
echo "📊 Force merging old indices..."
curl -X POST "elasticsearch-master.observability:9200/jaeger-span-$(date -d '7 days ago' +%Y-%m-%d)/_forcemerge?max_num_segments=1"

# 2. Clean up old Kafka topics (if not using ILM)
echo "🗑️  Cleaning up old data..."
# (Usually handled by ILM, manual cleanup if needed)

# 3. Restart OTel Collectors (clear any memory leaks)
echo "🔄 Rolling restart of collectors..."
kubectl rollout restart deployment/otel-collector-gateway -n observability
sleep 60
kubectl rollout status deployment/otel-collector-gateway -n observability

# 4. Verify backup jobs ran
echo "💾 Checking backup status..."
kubectl get cronjobs -n observability
kubectl get jobs -n observability | grep backup | head -5

# 5. Update Grafana dashboards (if modified)
echo "📈 Updating dashboards..."
# Import latest dashboard JSONs

echo "Maintenance complete!"
```

### 1.3 Monthly Review

**Time**: First Monday of month
**Attendees**: SRE team, Platform lead
**Duration**: 1 hour

**Agenda**:
1. Review metrics:
   - Service coverage
   - Trace volume trends
   - Query performance
   - System uptime
   - Cost analysis

2. Review incidents:
   - Trace-related incidents
   - False positives
   - Gaps in observability

3. Plan improvements:
   - Sampling adjustments
   - Storage optimization
   - Feature requests
   - Training needs

**Deliverables**:
- Monthly report (template below)
- Action items for next month
- Budget forecast

---

## 2. Monitoring the Monitoring

### 2.1 Key Metrics Dashboard

**Grafana Dashboard: "Distributed Tracing Health"**

#### Panel 1: Ingestion Rate
```promql
# Traces ingested per second
rate(jaeger_collector_traces_received_total[5m])

# Alert: >10k traces/sec sustained for 10 min
rate(jaeger_collector_traces_received_total[10m]) > 10000
```

#### Panel 2: Query Performance
```promql
# Query latency p99
histogram_quantile(0.99,
  rate(jaeger_query_request_duration_seconds_bucket[5m])
)

# Alert: p99 > 500ms for 5 min
histogram_quantile(0.99,
  rate(jaeger_query_request_duration_seconds_bucket[5m])
) > 0.5
```

#### Panel 3: Storage Health
```promql
# Elasticsearch disk usage
100 - (
  elasticsearch_filesystem_data_available_bytes /
  elasticsearch_filesystem_data_size_bytes * 100
)

# Alert: > 85% disk usage
100 - (
  elasticsearch_filesystem_data_available_bytes /
  elasticsearch_filesystem_data_size_bytes * 100
) > 85
```

#### Panel 4: Collector Health
```promql
# OTel Collector CPU usage
rate(container_cpu_usage_seconds_total{
  namespace="observability",
  pod=~"otel-collector-gateway.*"
}[5m]) * 100

# Alert: CPU > 80% for 10 min
rate(container_cpu_usage_seconds_total{
  namespace="observability",
  pod=~"otel-collector-gateway.*"
}[10m]) * 100 > 80
```

#### Panel 5: Kafka Lag (if applicable)
```promql
# Kafka consumer lag
kafka_consumergroup_lag{
  consumergroup="jaeger-ingester",
  topic="jaeger-spans"
}

# Alert: Lag > 100k messages for 5 min
kafka_consumergroup_lag{
  consumergroup="jaeger-ingester"
} > 100000
```

### 2.2 Alerts Configuration

```yaml
# prometheus-alerts.yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: prometheus-tracing-alerts
  namespace: observability
data:
  tracing.rules: |
    groups:
    - name: distributed-tracing
      interval: 30s
      rules:

      # Critical: Jaeger collector down
      - alert: JaegerCollectorDown
        expr: up{job="jaeger-collector"} == 0
        for: 2m
        labels:
          severity: critical
          component: jaeger
        annotations:
          summary: "Jaeger collector is down"
          description: "Jaeger collector {{ $labels.pod }} has been down for 2 minutes"
          runbook: "https://wiki.company.com/runbooks/jaeger-collector-down"

      # Critical: Elasticsearch cluster red
      - alert: ElasticsearchClusterRed
        expr: elasticsearch_cluster_health_status{color="red"} == 1
        for: 5m
        labels:
          severity: critical
          component: elasticsearch
        annotations:
          summary: "Elasticsearch cluster is RED"
          description: "Elasticsearch cluster health is RED. Data loss may occur."
          runbook: "https://wiki.company.com/runbooks/elasticsearch-red"

      # Warning: High query latency
      - alert: HighTraceQueryLatency
        expr: histogram_quantile(0.99, rate(jaeger_query_request_duration_seconds_bucket[5m])) > 0.5
        for: 5m
        labels:
          severity: warning
          component: jaeger
        annotations:
          summary: "High trace query latency"
          description: "P99 query latency is {{ $value }}s (threshold: 0.5s)"

      # Warning: Disk usage high
      - alert: ElasticsearchDiskUsageHigh
        expr: 100 - (elasticsearch_filesystem_data_available_bytes / elasticsearch_filesystem_data_size_bytes * 100) > 85
        for: 10m
        labels:
          severity: warning
          component: elasticsearch
        annotations:
          summary: "Elasticsearch disk usage high"
          description: "Disk usage is {{ $value }}% on {{ $labels.pod }}"
          runbook: "https://wiki.company.com/runbooks/elasticsearch-disk-full"

      # Warning: OTel Collector high CPU
      - alert: OTelCollectorHighCPU
        expr: rate(container_cpu_usage_seconds_total{namespace="observability", pod=~"otel-collector-gateway.*"}[5m]) * 100 > 80
        for: 10m
        labels:
          severity: warning
          component: otel-collector
        annotations:
          summary: "OTel Collector high CPU usage"
          description: "CPU usage is {{ $value }}% on {{ $labels.pod }}"

      # Warning: Kafka lag high
      - alert: JaegerKafkaLagHigh
        expr: kafka_consumergroup_lag{consumergroup="jaeger-ingester"} > 100000
        for: 5m
        labels:
          severity: warning
          component: kafka
        annotations:
          summary: "Jaeger Kafka consumer lag high"
          description: "Consumer lag is {{ $value }} messages for topic {{ $labels.topic }}"

      # Info: Low trace volume (possible issue)
      - alert: LowTraceVolume
        expr: rate(jaeger_collector_traces_received_total[10m]) < 10
        for: 10m
        labels:
          severity: info
          component: jaeger
        annotations:
          summary: "Unusually low trace volume"
          description: "Receiving only {{ $value }} traces/sec (may indicate instrumentation issues)"
```

### 2.3 Alerting Escalation

| Severity | Response Time | Escalation Path | Notification |
|----------|---------------|-----------------|--------------|
| **Critical** | Immediate | 1. On-call SRE → 2. SRE Manager → 3. CTO | PagerDuty + Slack |
| **Warning** | 30 minutes | 1. On-call SRE → 2. SRE Manager | Slack |
| **Info** | Best effort | SRE Team | Slack (no page) |

---

## 3. Incident Response

### 3.1 Incident Classification

| Incident Type | Example | Priority | SLA |
|---------------|---------|----------|-----|
| **P0: Complete Outage** | All trace collection stopped | Critical | 15 min response |
| **P1: Partial Outage** | Single component down | High | 30 min response |
| **P2: Degraded Performance** | Slow queries, high latency | Medium | 2 hour response |
| **P3: Minor Issue** | Single service not traced | Low | Next business day |

### 3.2 Runbooks

#### Runbook 1: Jaeger Collector Down

**Symptoms**:
- Alert: `JaegerCollectorDown`
- No new traces in Jaeger UI
- Applications logging connection errors

**Diagnosis**:
```bash
# Check pod status
kubectl get pods -n observability -l app=jaeger-collector

# Check logs
kubectl logs -n observability -l app=jaeger-collector --tail=100

# Check Elasticsearch connectivity
kubectl exec -n observability jaeger-collector-0 -- curl -s http://elasticsearch-master:9200/_cluster/health
```

**Common Causes**:
1. Elasticsearch unavailable
2. OOM (out of memory)
3. Network partition
4. Configuration error

**Resolution**:

```bash
# Option 1: Restart pods
kubectl rollout restart deployment/jaeger-collector -n observability

# Option 2: Scale up (if resource constrained)
kubectl scale deployment/jaeger-collector --replicas=5 -n observability

# Option 3: Check Elasticsearch
kubectl exec -n observability elasticsearch-master-0 -- curl -XPOST 'localhost:9200/_cluster/reroute?retry_failed=true'

# Option 4: Emergency - bypass Kafka, go direct to Elasticsearch
kubectl set env deployment/jaeger-collector SPAN_STORAGE_TYPE=elasticsearch
```

**Prevention**:
- Increase memory limits
- Enable HPA for auto-scaling
- Improve Elasticsearch stability

---

#### Runbook 2: Elasticsearch Disk Full

**Symptoms**:
- Alert: `ElasticsearchDiskUsageHigh`
- Write operations failing
- Cluster status yellow/red

**Immediate Action**:
```bash
# 1. Delete oldest indices
curl -X DELETE "elasticsearch-master.observability:9200/jaeger-span-$(date -d '45 days ago' +%Y-%m-%d)"

# 2. Force merge to free space
curl -X POST "elasticsearch-master.observability:9200/_forcemerge?max_num_segments=1"

# 3. Increase disk quota (temporary)
kubectl patch pvc elasticsearch-master-elasticsearch-master-0 -n observability \
  -p '{"spec":{"resources":{"requests":{"storage":"300Gi"}}}}'

# 4. Enable read-only to prevent writes (emergency)
curl -X PUT "elasticsearch-master.observability:9200/_cluster/settings" \
  -H 'Content-Type: application/json' \
  -d '{"transient": {"cluster.routing.allocation.disk.threshold_enabled": false}}'
```

**Long-term Solutions**:
- Implement ILM policy (see section 6.3)
- Increase sampling (reduce volume)
- Add more storage
- Implement hot-warm-cold architecture

---

#### Runbook 3: High Query Latency

**Symptoms**:
- Alert: `HighTraceQueryLatency`
- Jaeger UI slow to load
- Timeouts on trace queries

**Diagnosis**:
```bash
# Check Elasticsearch query performance
curl -s "elasticsearch-master.observability:9200/_cat/indices?v&s=store.size:desc" | head -20

# Check slow queries
curl -s "elasticsearch-master.observability:9200/_cluster/pending_tasks"

# Check JVM heap usage
kubectl exec -n observability elasticsearch-master-0 -- curl -s http://localhost:9200/_nodes/stats/jvm | jq '.nodes[].jvm.mem.heap_used_percent'
```

**Resolution**:
```bash
# 1. Clear field data cache
curl -X POST "elasticsearch-master.observability:9200/_cache/clear?fielddata=true"

# 2. Force merge old indices
curl -X POST "elasticsearch-master.observability:9200/jaeger-span-$(date -d '7 days ago' +%Y-%m-%d)/_forcemerge?max_num_segments=1"

# 3. Scale up Jaeger Query
kubectl scale deployment/jaeger-query --replicas=4 -n observability

# 4. Increase Elasticsearch heap (if needed)
kubectl set env statefulset/elasticsearch-master ES_JAVA_OPTS="-Xms8g -Xmx8g" -n observability
kubectl rollout restart statefulset/elasticsearch-master -n observability
```

---

#### Runbook 4: Kafka Lag High

**Symptoms**:
- Alert: `JaegerKafkaLagHigh`
- Traces delayed in Jaeger UI
- Ingester pods slow

**Diagnosis**:
```bash
# Check consumer group status
kubectl exec -n observability kafka-0 -- kafka-consumer-groups.sh \
  --describe --group jaeger-ingester --bootstrap-server localhost:9092

# Check ingester logs
kubectl logs -n observability -l app=jaeger-ingester --tail=100
```

**Resolution**:
```bash
# 1. Scale up ingesters
kubectl scale deployment/jaeger-ingester --replicas=6 -n observability

# 2. Increase consumer parallelism
kubectl set env deployment/jaeger-ingester KAFKA_CONSUMER_CONCURRENCY=10

# 3. Temporarily increase Elasticsearch write throughput
kubectl set env deployment/jaeger-ingester ES_BULK_SIZE=1000 ES_BULK_WORKERS=10

# 4. Reset consumer offset (emergency - data loss)
# kubectl exec -n observability kafka-0 -- kafka-consumer-groups.sh \
#   --reset-offsets --group jaeger-ingester --topic jaeger-spans \
#   --to-latest --execute --bootstrap-server localhost:9092
```

---

### 3.3 Incident Response Checklist

```markdown
# Incident Response Template

## Incident Details
- **Incident ID**: INC-XXXX
- **Severity**: [P0/P1/P2/P3]
- **Start Time**: YYYY-MM-DD HH:MM UTC
- **Detected By**: [Alert/User Report/Monitoring]
- **On-Call Engineer**: [Name]

## Impact Assessment
- **Affected Component**: [Collector/Query/Elasticsearch/etc]
- **User Impact**: [None/Low/Medium/High]
- **Services Affected**: [List]
- **Data Loss**: [Yes/No/Unknown]

## Timeline
- **HH:MM** - Incident detected
- **HH:MM** - Investigation started
- **HH:MM** - Root cause identified
- **HH:MM** - Mitigation applied
- **HH:MM** - Service restored
- **HH:MM** - Incident closed

## Root Cause
[Detailed description]

## Resolution
[Steps taken to resolve]

## Prevention
- [ ] Action item 1
- [ ] Action item 2
- [ ] Action item 3

## Post-Incident Review
- **Date**: [Schedule meeting]
- **Attendees**: [List]
- **Document**: [Link]
```

---

## 4. Maintenance Procedures

### 4.1 Upgrade Procedures

#### Upgrade OpenTelemetry Collector

```bash
# 1. Check current version
kubectl get deployment otel-collector-gateway -n observability -o jsonpath='{.spec.template.spec.containers[0].image}'

# 2. Review changelog
# https://github.com/open-telemetry/opentelemetry-collector-contrib/releases

# 3. Test in staging
helm upgrade otel-collector open-telemetry/opentelemetry-collector \
  --set image.tag=0.90.0 \
  -n observability-staging

# 4. Upgrade in production (rolling update)
kubectl set image deployment/otel-collector-gateway \
  otel-collector=otel/opentelemetry-collector-contrib:0.90.0 \
  -n observability

# 5. Monitor rollout
kubectl rollout status deployment/otel-collector-gateway -n observability

# 6. Verify
curl http://otel-collector-gateway.observability:13133/
```

#### Upgrade Jaeger

```bash
# 1. Backup current config
kubectl get deployment jaeger-collector -n observability -o yaml > jaeger-collector-backup.yaml

# 2. Upgrade via Helm
helm repo update
helm upgrade jaeger jaegertracing/jaeger \
  --version 0.72.0 \
  -n observability \
  -f jaeger-values.yaml

# 3. Monitor
kubectl get pods -n observability -w

# 4. Verify
curl http://jaeger-query.observability:16686/api/services
```

#### Upgrade Elasticsearch

**⚠️  WARNING: Elasticsearch upgrades require careful planning**

```bash
# 1. Read upgrade guide
# https://www.elastic.co/guide/en/elasticsearch/reference/current/setup-upgrade.html

# 2. Check compatibility matrix
# Current: 8.10 → Target: 8.11 (minor upgrade OK)

# 3. Disable shard allocation
kubectl exec -n observability elasticsearch-master-0 -- curl -X PUT "localhost:9200/_cluster/settings" \
  -H 'Content-Type: application/json' \
  -d '{"persistent": {"cluster.routing.allocation.enable": "primaries"}}'

# 4. Upgrade one node at a time
kubectl set image statefulset/elasticsearch-master \
  elasticsearch=docker.elastic.co/elasticsearch/elasticsearch:8.11.0 \
  -n observability

# Wait for pod to be ready
kubectl wait --for=condition=ready pod/elasticsearch-master-0 -n observability --timeout=600s

# 5. Re-enable shard allocation
kubectl exec -n observability elasticsearch-master-0 -- curl -X PUT "localhost:9200/_cluster/settings" \
  -H 'Content-Type: application/json' \
  -d '{"persistent": {"cluster.routing.allocation.enable": "all"}}'

# 6. Wait for cluster to stabilize
kubectl exec -n observability elasticsearch-master-0 -- curl "localhost:9200/_cluster/health?wait_for_status=green&timeout=5m"

# 7. Repeat for remaining nodes
```

### 4.2 Backup Procedures

#### Elasticsearch Snapshots

```bash
# 1. Configure snapshot repository (one-time setup)
kubectl exec -n observability elasticsearch-master-0 -- curl -X PUT "localhost:9200/_snapshot/backup" \
  -H 'Content-Type: application/json' \
  -d '{
  "type": "s3",
  "settings": {
    "bucket": "company-elasticsearch-backups",
    "region": "us-east-1",
    "base_path": "jaeger-traces"
  }
}'

# 2. Create daily snapshot (via CronJob)
cat <<EOF | kubectl apply -f -
apiVersion: batch/v1
kind: CronJob
metadata:
  name: elasticsearch-snapshot
  namespace: observability
spec:
  schedule: "0 2 * * *"  # 2 AM daily
  jobTemplate:
    spec:
      template:
        spec:
          containers:
          - name: snapshot
            image: curlimages/curl:latest
            command:
            - sh
            - -c
            - |
              SNAPSHOT_NAME="snapshot-\$(date +%Y-%m-%d)"
              curl -X PUT "elasticsearch-master:9200/_snapshot/backup/\$SNAPSHOT_NAME?wait_for_completion=false" \
                -H 'Content-Type: application/json' \
                -d '{
                "indices": "jaeger-*",
                "ignore_unavailable": true,
                "include_global_state": false
              }'
          restartPolicy: OnFailure
EOF

# 3. Verify snapshots
kubectl exec -n observability elasticsearch-master-0 -- curl "localhost:9200/_snapshot/backup/_all?pretty"

# 4. Restore from snapshot (if needed)
kubectl exec -n observability elasticsearch-master-0 -- curl -X POST "localhost:9200/_snapshot/backup/snapshot-2025-10-14/_restore" \
  -H 'Content-Type: application/json' \
  -d '{
  "indices": "jaeger-span-2025-10-14",
  "ignore_unavailable": true,
  "include_global_state": false,
  "rename_pattern": "(.+)",
  "rename_replacement": "restored_$1"
}'
```

### 4.3 Disaster Recovery

#### Scenario 1: Complete Elasticsearch Cluster Loss

**RTO**: 2 hours
**RPO**: 24 hours (last snapshot)

**Recovery Steps**:
```bash
# 1. Deploy new Elasticsearch cluster
helm install elasticsearch-new elastic/elasticsearch \
  -f elasticsearch-values.yaml \
  -n observability

# 2. Restore latest snapshot
kubectl exec -n observability elasticsearch-new-master-0 -- curl -X POST "localhost:9200/_snapshot/backup/latest/_restore"

# 3. Update Jaeger to point to new cluster
kubectl set env deployment/jaeger-collector \
  ES_SERVER_URLS=http://elasticsearch-new-master:9200 \
  -n observability

# 4. Verify
curl http://jaeger-query.observability:16686/api/services
```

#### Scenario 2: Complete Trace System Loss

**Impact**: No new traces collected
**Service Impact**: None (services continue to function)

**Recovery Steps**:
```bash
# 1. Redeploy entire stack
helm install jaeger jaegertracing/jaeger -f jaeger-values.yaml -n observability
helm install otel-collector open-telemetry/opentelemetry-collector -f otel-values.yaml -n observability

# 2. Wait for readiness
kubectl wait --for=condition=ready pod -l app=jaeger-collector -n observability --timeout=600s

# 3. Services automatically reconnect (no action needed)
# Traces will resume flowing
```

---

## 5. Troubleshooting

### 5.1 Common Issues & Solutions

#### Issue: "Context deadline exceeded" errors

**Symptom**: Applications logging OTLP export errors

**Cause**: OTel Collector unreachable or slow

**Solution**:
```bash
# Test connectivity
kubectl exec -it deployment/myservice -- nc -zv otel-collector-agent 4317

# Check collector health
kubectl get pods -n observability -l app=otel-collector-agent

# Check collector logs
kubectl logs -n observability -l app=otel-collector-agent --tail=100 | grep ERROR

# Increase timeout (if needed)
kubectl set env deployment/myservice OTEL_EXPORTER_OTLP_TIMEOUT=30000
```

---

#### Issue: Traces missing spans

**Symptom**: Incomplete traces in Jaeger

**Cause**: Context not propagated correctly

**Diagnosis**:
```bash
# Check application logs for trace context
kubectl logs -f deployment/myservice | grep -i traceparent

# Verify trace context in HTTP headers
kubectl exec -it deployment/myservice -- tcpdump -A -s 0 'tcp port 8080 and (((ip[2:2] - ((ip[0]&0xf)<<2)) - ((tcp[12]&0xf0)>>2)) != 0)' | grep traceparent
```

**Solution**: Fix context propagation in code (see LLD examples)

---

#### Issue: High cardinality causing performance issues

**Symptom**: Elasticsearch slow, high memory usage

**Cause**: Too many unique span attributes (e.g., including UUIDs)

**Solution**:
```yaml
# Add attribute processor to OTel Collector
processors:
  attributes:
    actions:
      # Remove high-cardinality attributes
      - key: user.id
        action: delete
      - key: request.id
        action: delete
      # Or hash them
      - key: session.id
        action: hash
```

---

### 5.2 Debugging Tools

```bash
# 1. Jaeger CLI
# Install: go install github.com/jaegertracing/jaeger/cmd/jaeger@latest

# Query traces
jaeger-cli query --server=http://jaeger-query.observability:16686 \
  --service=order-service \
  --operation=createOrder \
  --lookback=1h

# 2. OTel Collector Debug Exporter
# Add to collector config temporarily:
exporters:
  logging:
    loglevel: debug

# View exported spans in logs
kubectl logs -f -n observability otel-collector-gateway-xxx | jq .

# 3. Trace via curl
TRACE_ID="0af7651916cd43dd8448eb211c80319c"
curl -s "http://jaeger-query.observability:16686/api/traces/$TRACE_ID" | jq .

# 4. Generate test traces
# Use Jaeger HotROD demo app
kubectl run hotrod --image=jaegertracing/example-hotrod:latest \
  --env JAEGER_AGENT_HOST=otel-collector-agent \
  -n observability
```

---

## 6. Performance Tuning

### 6.1 Sampling Optimization

**Goal**: Reduce trace volume by 50-80% while maintaining visibility

**Strategy**: Tail-based sampling

```yaml
# OTel Collector config
processors:
  tail_sampling:
    decision_wait: 10s
    num_traces: 100000
    expected_new_traces_per_sec: 1000
    policies:
      # Keep 100% of errors
      - name: errors
        type: status_code
        status_code:
          status_codes: [ERROR]

      # Keep 100% of slow requests (>2s)
      - name: slow-traces
        type: latency
        latency:
          threshold_ms: 2000

      # Keep 100% of specific critical services
      - name: critical-services
        type: string_attribute
        string_attribute:
          key: service.name
          values:
            - payment-service
            - auth-service

      # Keep 5% of everything else
      - name: probabilistic
        type: probabilistic
        probabilistic:
          sampling_percentage: 5
```

**Before/After Analysis**:
```bash
# Before
# Traces/day: 10M
# Storage: 500GB/month
# Cost: $2000/month

# After (with above sampling)
# Traces/day: 3M (70% reduction)
# Storage: 150GB/month
# Cost: $600/month
# Savings: $1400/month
```

### 6.2 Query Performance Optimization

#### Elasticsearch Index Optimization

```bash
# 1. Optimize index settings
curl -X PUT "elasticsearch-master.observability:9200/jaeger-span-*/_settings" \
  -H 'Content-Type: application/json' \
  -d '{
  "index": {
    "refresh_interval": "30s",
    "number_of_replicas": 1,
    "codec": "best_compression"
  }
}'

# 2. Create index template with optimized mappings
curl -X PUT "elasticsearch-master.observability:9200/_index_template/jaeger-span" \
  -H 'Content-Type: application/json' \
  -d '{
  "index_patterns": ["jaeger-span-*"],
  "template": {
    "settings": {
      "number_of_shards": 3,
      "number_of_replicas": 2,
      "refresh_interval": "30s",
      "codec": "best_compression"
    },
    "mappings": {
      "properties": {
        "traceID": {"type": "keyword"},
        "spanID": {"type": "keyword"},
        "operationName": {"type": "keyword"},
        "startTime": {"type": "date"},
        "duration": {"type": "long"},
        "tags": {
          "type": "nested",
          "properties": {
            "key": {"type": "keyword"},
            "value": {"type": "keyword"}
          }
        }
      }
    }
  }
}'

# 3. Force merge old indices (reduce segment count)
curl -X POST "elasticsearch-master.observability:9200/jaeger-span-$(date -d '7 days ago' +%Y-%m-%d)/_forcemerge?max_num_segments=1"
```

### 6.3 Index Lifecycle Management (ILM)

```bash
# Create ILM policy
curl -X PUT "elasticsearch-master.observability:9200/_ilm/policy/jaeger-ilm" \
  -H 'Content-Type: application/json' \
  -d '{
  "policy": {
    "phases": {
      "hot": {
        "min_age": "0ms",
        "actions": {
          "rollover": {
            "max_age": "1d",
            "max_primary_shard_size": "50gb"
          },
          "set_priority": {
            "priority": 100
          }
        }
      },
      "warm": {
        "min_age": "3d",
        "actions": {
          "forcemerge": {
            "max_num_segments": 1
          },
          "shrink": {
            "number_of_shards": 1
          },
          "set_priority": {
            "priority": 50
          }
        }
      },
      "cold": {
        "min_age": "7d",
        "actions": {
          "freeze": {},
          "set_priority": {
            "priority": 0
          }
        }
      },
      "delete": {
        "min_age": "30d",
        "actions": {
          "delete": {}
        }
      }
    }
  }
}'

# Apply to indices
curl -X PUT "elasticsearch-master.observability:9200/jaeger-span-*/_settings" \
  -H 'Content-Type: application/json' \
  -d '{
  "index.lifecycle.name": "jaeger-ilm",
  "index.lifecycle.rollover_alias": "jaeger-span-write"
}'
```

---

## 7. Cost Management

### 7.1 Cost Breakdown

| Component | Monthly Cost | Optimization Opportunity |
|-----------|--------------|-------------------------|
| Elasticsearch | $1,800 | Use cold storage, ILM |
| OTel Collectors | $180 | Right-size replicas |
| Jaeger | $600 | Optimize queries |
| Kafka | $180 | Reduce retention |
| Data Transfer | $500 | Use compression |
| **Total** | **$3,260** | **Target: $2,000** |

### 7.2 Cost Optimization Strategies

#### Strategy 1: Aggressive Sampling
**Savings**: 50%
**Implementation**: Reduce sampling to 5% (from 10%)

#### Strategy 2: Shorter Retention
**Savings**: 30%
**Implementation**: Reduce from 30 days to 14 days

#### Strategy 3: Hot-Warm-Cold Architecture
**Savings**: 40%
**Implementation**: Move old data to S3 (via snapshot)

```bash
# Archive old indices to S3
curl -X PUT "elasticsearch-master.observability:9200/_snapshot/s3-archive" \
  -H 'Content-Type: application/json' \
  -d '{
  "type": "s3",
  "settings": {
    "bucket": "jaeger-archive",
    "storage_class": "GLACIER"
  }
}'

# Snapshot and delete old indices
curl -X PUT "elasticsearch-master.observability:9200/_snapshot/s3-archive/archive-$(date +%Y-%m)?wait_for_completion=false" \
  -H 'Content-Type: application/json' \
  -d '{
  "indices": "jaeger-span-$(date -d '30 days ago' +%Y-%m-*)",
  "include_global_state": false
}'

# Delete after snapshot completes
curl -X DELETE "elasticsearch-master.observability:9200/jaeger-span-$(date -d '30 days ago' +%Y-%m-*)"
```

#### Strategy 4: Right-Sizing

```bash
# Analyze resource usage
kubectl top nodes
kubectl top pods -n observability

# Example: Reduce OTel Collector Gateway replicas
kubectl scale deployment/otel-collector-gateway --replicas=2 -n observability

# Reduce Elasticsearch heap (if over-provisioned)
kubectl set env statefulset/elasticsearch-master ES_JAVA_OPTS="-Xms4g -Xmx4g" -n observability
```

---

## 8. Best Practices

### 8.1 Do's ✅

1. **Monitor the monitoring system**
   - Set up alerts for trace system health
   - Track ingestion rate, query latency, storage usage

2. **Implement ILM early**
   - Automate data lifecycle
   - Prevent storage issues

3. **Use tail-based sampling**
   - Keep errors and slow requests
   - Sample normal traffic aggressively

4. **Document everything**
   - Runbooks for common issues
   - Architecture diagrams up-to-date
   - Change log maintained

5. **Test disaster recovery**
   - Regular DR drills
   - Validate backups monthly

6. **Optimize for cost**
   - Review spending monthly
   - Right-size resources
   - Archive old data

7. **Enable security**
   - TLS everywhere
   - Authentication on Jaeger UI
   - PII scrubbing

8. **Train the team**
   - Regular workshops
   - Share debugging successes
   - Maintain FAQ

### 8.2 Don'ts ❌

1. **Don't ignore alerts**
   - Address warnings before they become critical
   - Tune noisy alerts

2. **Don't store PII**
   - Scrub sensitive data
   - Regular audits

3. **Don't sample at 100% in production**
   - Use intelligent sampling
   - Monitor costs

4. **Don't skip backups**
   - Automate snapshots
   - Test restores

5. **Don't upgrade without testing**
   - Test in staging first
   - Have rollback plan

6. **Don't over-instrument**
   - Balance observability vs. performance
   - Focus on critical paths

7. **Don't ignore performance**
   - Monitor impact on applications
   - Optimize regularly

8. **Don't set-and-forget**
   - Regular reviews
   - Continuous optimization

---

## 9. Appendix

### 9.1 Monthly Report Template

```markdown
# Distributed Tracing Monthly Report - [Month Year]

## Executive Summary
- System uptime: [XX.XX%]
- Traces collected: [XXM]
- Query latency (p99): [XXXms]
- Cost: $[XXXX] (budget: $XXXX)

## Key Metrics
| Metric | Target | Actual | Status |
|--------|--------|--------|--------|
| Service Coverage | 100% | [XX%] | [✅/⚠️/🔴] |
| Trace Completion | >95% | [XX%] | [✅/⚠️/🔴] |
| Query Latency (p99) | <500ms | [XXms] | [✅/⚠️/🔴] |
| System Uptime | 99.9% | [XX.XX%] | [✅/⚠️/🔴] |

## Incidents
- P0: [X] (list)
- P1: [X] (list)
- P2: [X] (list)

## Cost Analysis
- Elasticsearch: $[XXX]
- Collectors: $[XXX]
- Jaeger: $[XXX]
- Other: $[XXX]
- **Total**: $[XXXX]

## Usage Statistics
- Active users: [XX]
- Traces queried: [XXXX]
- Most queried services: [list]

## Improvements This Month
- [Improvement 1]
- [Improvement 2]

## Action Items for Next Month
- [ ] Action item 1
- [ ] Action item 2

## Feedback & Requests
- [Team feedback summary]
- [Feature requests]
```

### 9.2 Useful Commands Reference

```bash
# Quick health check
kubectl get pods -n observability

# Check Elasticsearch health
kubectl exec -n observability elasticsearch-master-0 -- curl -s localhost:9200/_cluster/health

# Check Jaeger metrics
kubectl port-forward -n observability svc/jaeger-collector 14269:14269 &
curl localhost:14269/metrics

# Trace a request end-to-end
TRACE_ID=$(curl -s -i https://api.company.com/test | grep X-Trace-Id | cut -d' ' -f2)
echo "Trace: https://jaeger.company.com/trace/$TRACE_ID"

# Export traces for analysis
curl -s "http://jaeger-query:16686/api/traces?service=myservice&limit=100" > traces.json

# Check disk usage
kubectl exec -n observability elasticsearch-master-0 -- df -h | grep /usr/share/elasticsearch

# Force garbage collection (Elasticsearch)
kubectl exec -n observability elasticsearch-master-0 -- curl -X POST localhost:9200/_nodes/gc

# Test OTel Collector endpoint
echo '{"resourceSpans":[]}' | curl -X POST -H "Content-Type: application/json" -d @- http://otel-collector-gateway:4318/v1/traces
```

---

*Document Version: 1.0*
*Last Updated: October 15, 2025*
*Status: Operational*
