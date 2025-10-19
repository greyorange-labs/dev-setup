# Distributed Tracing Implementation Guide

## Table of Contents
1. [Prerequisites](#prerequisites)
2. [Phase 1: Infrastructure Setup](#phase-1-infrastructure-setup)
3. [Phase 2: Service Instrumentation](#phase-2-service-instrumentation)
4. [Phase 3: Testing & Validation](#phase-3-testing--validation)
5. [Phase 4: Production Deployment](#phase-4-production-deployment)
6. [Phase 5: Optimization](#phase-5-optimization)
7. [Troubleshooting Guide](#troubleshooting-guide)

---

## 1. Prerequisites

### 1.1 Infrastructure Requirements

| Component | Requirement | Purpose |
|-----------|-------------|---------|
| **Kubernetes Cluster** | v1.24+ | Host all components |
| **Helm** | v3.0+ | Package management |
| **Storage Class** | Dynamic provisioning | Persistent volumes |
| **Ingress Controller** | NGINX/Traefik | External access |
| **Certificate Manager** | cert-manager | TLS certificates |
| **Monitoring** | Prometheus + Grafana | Monitor the monitoring |

### 1.2 Software Requirements

- Docker 20.10+
- kubectl 1.24+
- Helm 3.10+
- curl, jq (for testing)

### 1.3 Access Requirements

- Kubernetes admin access
- Container registry access
- DNS management access

### 1.4 Knowledge Requirements

- Kubernetes basics
- Distributed systems concepts
- Basic understanding of tracing

---

## 2. Phase 1: Infrastructure Setup

**Duration**: 2 weeks
**Goal**: Deploy core tracing infrastructure

### 2.1 Step 1: Create Namespace

```bash
# Create observability namespace
kubectl create namespace observability

# Label namespace for monitoring
kubectl label namespace observability monitoring=enabled

# Set as default namespace (optional)
kubectl config set-context --current --namespace=observability
```

### 2.2 Step 2: Deploy Elasticsearch

**Option A: Using Helm (Recommended)**

```bash
# Add Elastic Helm repository
helm repo add elastic https://helm.elastic.co
helm repo update

# Create values file
cat <<EOF > elasticsearch-values.yaml
clusterName: "jaeger-elasticsearch"
nodeGroup: "master"

# Master nodes
replicas: 3
minimumMasterNodes: 2

# Resources
resources:
  requests:
    cpu: "2000m"
    memory: "4Gi"
  limits:
    cpu: "4000m"
    memory: "8Gi"

# Storage
volumeClaimTemplate:
  accessModes: ["ReadWriteOnce"]
  storageClassName: "gp3"  # Adjust based on your provider
  resources:
    requests:
      storage: 200Gi

# JVM heap size (50% of memory)
esJavaOpts: "-Xmx4g -Xms4g"

# Enable security (production)
esConfig:
  elasticsearch.yml: |
    xpack.security.enabled: true
    xpack.security.transport.ssl.enabled: true
    xpack.security.http.ssl.enabled: true

# Service
service:
  type: ClusterIP
  ports:
    - name: http
      port: 9200
      targetPort: 9200
    - name: transport
      port: 9300
      targetPort: 9300

# Index lifecycle management
extraEnvs:
  - name: ELASTIC_PASSWORD
    valueFrom:
      secretKeyRef:
        name: elasticsearch-credentials
        key: password

# Prometheus exporter
prometheus:
  enabled: true
  port: 9114

EOF

# Create secret for Elasticsearch password
kubectl create secret generic elasticsearch-credentials \
  --from-literal=password=YourStrongPassword123! \
  -n observability

# Install Elasticsearch
helm install elasticsearch elastic/elasticsearch \
  -f elasticsearch-values.yaml \
  -n observability \
  --version 8.10.0

# Wait for Elasticsearch to be ready
kubectl wait --for=condition=ready pod \
  -l app=elasticsearch-master \
  -n observability \
  --timeout=600s
```

**Verify Elasticsearch**:

```bash
# Port forward to access Elasticsearch
kubectl port-forward svc/elasticsearch-master 9200:9200 -n observability &

# Check cluster health
curl -u elastic:YourStrongPassword123! http://localhost:9200/_cluster/health?pretty

# Expected output:
# {
#   "cluster_name" : "jaeger-elasticsearch",
#   "status" : "green",
#   "number_of_nodes" : 3,
#   ...
# }
```

### 2.3 Step 3: Deploy Kafka (Optional but Recommended)

```bash
# Add Bitnami Helm repository
helm repo add bitnami https://charts.bitnami.com/bitnami
helm repo update

# Create Kafka values file
cat <<EOF > kafka-values.yaml
replicaCount: 3

# Persistence
persistence:
  enabled: true
  storageClass: "gp3"
  size: 100Gi

# Resources
resources:
  requests:
    cpu: 500m
    memory: 2Gi
  limits:
    cpu: 1000m
    memory: 4Gi

# Zookeeper (required for Kafka)
zookeeper:
  enabled: true
  replicaCount: 3
  persistence:
    enabled: true
    size: 10Gi

# Kafka configuration
config: |
  auto.create.topics.enable=true
  default.replication.factor=3
  min.insync.replicas=2
  num.partitions=12
  log.retention.hours=1
  compression.type=gzip

# Metrics for monitoring
metrics:
  kafka:
    enabled: true
  jmx:
    enabled: true

EOF

# Install Kafka
helm install kafka bitnami/kafka \
  -f kafka-values.yaml \
  -n observability \
  --version 26.4.0

# Wait for Kafka to be ready
kubectl wait --for=condition=ready pod \
  -l app.kubernetes.io/name=kafka \
  -n observability \
  --timeout=600s
```

**Verify Kafka**:

```bash
# Port forward to Kafka
kubectl port-forward svc/kafka 9092:9092 -n observability &

# Create test topic
kubectl exec -it kafka-0 -n observability -- \
  kafka-topics.sh --create \
  --topic jaeger-spans \
  --partitions 12 \
  --replication-factor 3 \
  --bootstrap-server localhost:9092

# List topics
kubectl exec -it kafka-0 -n observability -- \
  kafka-topics.sh --list \
  --bootstrap-server localhost:9092
```

### 2.4 Step 4: Deploy Jaeger

```bash
# Add Jaeger Helm repository
helm repo add jaegertracing https://jaegertracing.github.io/helm-charts
helm repo update

# Create Jaeger values file
cat <<EOF > jaeger-values.yaml
# Use production deployment strategy
provisionDataStore:
  cassandra: false
  elasticsearch: true

# Storage configuration
storage:
  type: elasticsearch
  elasticsearch:
    host: elasticsearch-master
    port: 9200
    scheme: http
    user: elastic
    password: YourStrongPassword123!
  options:
    es:
      server-urls: http://elasticsearch-master:9200
      username: elastic
      password: YourStrongPassword123!
      index-prefix: jaeger
      num-shards: 3
      num-replicas: 2

# Kafka configuration (buffering)
ingester:
  enabled: true
  replicaCount: 3
  kafka:
    consumer:
      brokers:
        - kafka:9092
      topic: jaeger-spans
      groupID: jaeger-ingester
  resources:
    requests:
      cpu: 500m
      memory: 1Gi
    limits:
      cpu: 1000m
      memory: 2Gi

# Collector configuration
collector:
  replicaCount: 3
  service:
    type: ClusterIP
    grpc:
      port: 14250
    http:
      port: 14268
    zipkin:
      port: 9411
    otlp:
      grpc:
        port: 4317
      http:
        port: 4318
  resources:
    requests:
      cpu: 1000m
      memory: 2Gi
    limits:
      cpu: 2000m
      memory: 4Gi
  autoscaling:
    enabled: true
    minReplicas: 3
    maxReplicas: 10
    targetCPUUtilizationPercentage: 70
  env:
    - name: KAFKA_PRODUCER_BROKERS
      value: "kafka:9092"
    - name: KAFKA_PRODUCER_TOPIC
      value: "jaeger-spans"
    - name: SPAN_STORAGE_TYPE
      value: "kafka"

# Query service configuration
query:
  replicaCount: 2
  service:
    type: ClusterIP
    port: 16686
  resources:
    requests:
      cpu: 500m
      memory: 1Gi
    limits:
      cpu: 1000m
      memory: 2Gi
  basePath: /
  env:
    - name: SPAN_STORAGE_TYPE
      value: "elasticsearch"

# Agent configuration (DaemonSet)
agent:
  enabled: false  # We'll use OTel Collector instead

# Enable service monitors for Prometheus
serviceMonitor:
  enabled: true

EOF

# Install Jaeger
helm install jaeger jaegertracing/jaeger \
  -f jaeger-values.yaml \
  -n observability \
  --version 0.71.0

# Wait for Jaeger components to be ready
kubectl wait --for=condition=ready pod \
  -l app.kubernetes.io/name=jaeger \
  -n observability \
  --timeout=600s
```

**Verify Jaeger**:

```bash
# Port forward to Jaeger UI
kubectl port-forward svc/jaeger-query 16686:16686 -n observability &

# Open browser to http://localhost:16686
echo "Jaeger UI: http://localhost:16686"

# Check Jaeger collector health
kubectl port-forward svc/jaeger-collector 14269:14269 -n observability &
curl http://localhost:14269/health
```

### 2.5 Step 5: Deploy OpenTelemetry Collector

**Option A: DaemonSet (Agent Mode)**

```bash
# Create OTel Collector ConfigMap
cat <<EOF | kubectl apply -f -
apiVersion: v1
kind: ConfigMap
metadata:
  name: otel-collector-agent-config
  namespace: observability
data:
  config.yaml: |
    receivers:
      otlp:
        protocols:
          grpc:
            endpoint: 0.0.0.0:4317
          http:
            endpoint: 0.0.0.0:4318

    processors:
      memory_limiter:
        check_interval: 1s
        limit_mib: 512
        spike_limit_mib: 128

      batch:
        timeout: 5s
        send_batch_size: 512
        send_batch_max_size: 1024

      resource:
        attributes:
          - key: k8s.node.name
            value: \${K8S_NODE_NAME}
            action: insert
          - key: k8s.pod.name
            value: \${K8S_POD_NAME}
            action: insert
          - key: k8s.namespace.name
            value: \${K8S_NAMESPACE}
            action: insert

    exporters:
      otlp:
        endpoint: otel-collector-gateway:4317
        tls:
          insecure: true
        compression: gzip
        sending_queue:
          enabled: true
          num_consumers: 5
          queue_size: 1000
        retry_on_failure:
          enabled: true
          initial_interval: 1s
          max_interval: 30s

      logging:
        loglevel: info

    service:
      pipelines:
        traces:
          receivers: [otlp]
          processors: [memory_limiter, resource, batch]
          exporters: [otlp, logging]
EOF

# Deploy OTel Collector DaemonSet
cat <<EOF | kubectl apply -f -
apiVersion: apps/v1
kind: DaemonSet
metadata:
  name: otel-collector-agent
  namespace: observability
spec:
  selector:
    matchLabels:
      app: otel-collector-agent
  template:
    metadata:
      labels:
        app: otel-collector-agent
    spec:
      containers:
      - name: otel-collector
        image: otel/opentelemetry-collector-contrib:0.89.0
        command:
          - "/otelcol-contrib"
          - "--config=/conf/config.yaml"
        ports:
        - containerPort: 4317  # OTLP gRPC
          name: otlp-grpc
          protocol: TCP
        - containerPort: 4318  # OTLP HTTP
          name: otlp-http
          protocol: TCP
        - containerPort: 13133 # Health check
          name: health
          protocol: TCP
        env:
        - name: K8S_NODE_NAME
          valueFrom:
            fieldRef:
              fieldPath: spec.nodeName
        - name: K8S_POD_NAME
          valueFrom:
            fieldRef:
              fieldPath: metadata.name
        - name: K8S_NAMESPACE
          valueFrom:
            fieldRef:
              fieldPath: metadata.namespace
        resources:
          requests:
            cpu: 100m
            memory: 256Mi
          limits:
            cpu: 200m
            memory: 512Mi
        volumeMounts:
        - name: config
          mountPath: /conf
        livenessProbe:
          httpGet:
            path: /
            port: 13133
          initialDelaySeconds: 10
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /
            port: 13133
          initialDelaySeconds: 5
          periodSeconds: 5
      volumes:
      - name: config
        configMap:
          name: otel-collector-agent-config
      serviceAccountName: otel-collector-agent
---
apiVersion: v1
kind: ServiceAccount
metadata:
  name: otel-collector-agent
  namespace: observability
---
apiVersion: v1
kind: Service
metadata:
  name: otel-collector-agent
  namespace: observability
spec:
  type: ClusterIP
  clusterIP: None  # Headless service
  selector:
    app: otel-collector-agent
  ports:
  - name: otlp-grpc
    port: 4317
    targetPort: 4317
  - name: otlp-http
    port: 4318
    targetPort: 4318
EOF
```

**Option B: Gateway Collector**

```bash
# Create Gateway Collector ConfigMap (full config from LLD doc)
cat <<EOF | kubectl apply -f -
apiVersion: v1
kind: ConfigMap
metadata:
  name: otel-collector-gateway-config
  namespace: observability
data:
  config.yaml: |
    receivers:
      otlp:
        protocols:
          grpc:
            endpoint: 0.0.0.0:4317
          http:
            endpoint: 0.0.0.0:4318

    processors:
      memory_limiter:
        check_interval: 1s
        limit_mib: 1024
        spike_limit_mib: 256

      batch:
        timeout: 10s
        send_batch_size: 1024
        send_batch_max_size: 2048

      resource:
        attributes:
          - key: cluster.name
            value: production-k8s
            action: insert

      # Tail-based sampling
      tail_sampling:
        decision_wait: 10s
        num_traces: 100000
        expected_new_traces_per_sec: 1000
        policies:
          - name: error-policy
            type: status_code
            status_code:
              status_codes: [ERROR]
          - name: latency-policy
            type: latency
            latency:
              threshold_ms: 1000
          - name: probabilistic-policy
            type: probabilistic
            probabilistic:
              sampling_percentage: 10

    exporters:
      otlp/jaeger:
        endpoint: jaeger-collector:4317
        tls:
          insecure: true
        compression: gzip

      kafka:
        brokers:
          - kafka:9092
        topic: jaeger-spans
        encoding: otlp_proto

      logging:
        loglevel: info

    service:
      pipelines:
        traces:
          receivers: [otlp]
          processors: [memory_limiter, resource, tail_sampling, batch]
          exporters: [otlp/jaeger, kafka, logging]
EOF

# Deploy Gateway Collector
cat <<EOF | kubectl apply -f -
apiVersion: apps/v1
kind: Deployment
metadata:
  name: otel-collector-gateway
  namespace: observability
spec:
  replicas: 3
  selector:
    matchLabels:
      app: otel-collector-gateway
  template:
    metadata:
      labels:
        app: otel-collector-gateway
    spec:
      containers:
      - name: otel-collector
        image: otel/opentelemetry-collector-contrib:0.89.0
        command:
          - "/otelcol-contrib"
          - "--config=/conf/config.yaml"
        ports:
        - containerPort: 4317
          name: otlp-grpc
        - containerPort: 4318
          name: otlp-http
        - containerPort: 13133
          name: health
        resources:
          requests:
            cpu: 500m
            memory: 1Gi
          limits:
            cpu: 1000m
            memory: 2Gi
        volumeMounts:
        - name: config
          mountPath: /conf
        livenessProbe:
          httpGet:
            path: /
            port: 13133
        readinessProbe:
          httpGet:
            path: /
            port: 13133
      volumes:
      - name: config
        configMap:
          name: otel-collector-gateway-config
---
apiVersion: v1
kind: Service
metadata:
  name: otel-collector-gateway
  namespace: observability
spec:
  type: ClusterIP
  selector:
    app: otel-collector-gateway
  ports:
  - name: otlp-grpc
    port: 4317
    targetPort: 4317
  - name: otlp-http
    port: 4318
    targetPort: 4318
---
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: otel-collector-gateway
  namespace: observability
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: otel-collector-gateway
  minReplicas: 3
  maxReplicas: 10
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
EOF
```

### 2.6 Step 6: Expose Jaeger UI

```bash
# Create Ingress for Jaeger UI
cat <<EOF | kubectl apply -f -
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: jaeger-ui
  namespace: observability
  annotations:
    nginx.ingress.kubernetes.io/ssl-redirect: "true"
    cert-manager.io/cluster-issuer: "letsencrypt-prod"
spec:
  ingressClassName: nginx
  tls:
  - hosts:
    - jaeger.yourcompany.com
    secretName: jaeger-tls
  rules:
  - host: jaeger.yourcompany.com
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: jaeger-query
            port:
              number: 16686
EOF

# Update DNS to point jaeger.yourcompany.com to your ingress controller
```

### 2.7 Step 7: Verify Complete Setup

```bash
# Check all pods are running
kubectl get pods -n observability

# Expected output (all Running):
# NAME                                      READY   STATUS    RESTARTS   AGE
# elasticsearch-master-0                    1/1     Running   0          10m
# elasticsearch-master-1                    1/1     Running   0          9m
# elasticsearch-master-2                    1/1     Running   0          8m
# kafka-0                                   1/1     Running   0          5m
# kafka-1                                   1/1     Running   0          4m
# kafka-2                                   1/1     Running   0          3m
# jaeger-collector-xxx                      1/1     Running   0          2m
# jaeger-query-xxx                          1/1     Running   0          2m
# jaeger-ingester-xxx                       1/1     Running   0          2m
# otel-collector-agent-xxx                  1/1     Running   0          1m
# otel-collector-gateway-xxx                1/1     Running   0          1m

# Check services
kubectl get svc -n observability

# Test connectivity
kubectl run test-pod --image=curlimages/curl:latest -n observability --rm -it -- sh

# Inside test pod:
# Test OTel Collector
curl http://otel-collector-gateway:13133/

# Test Jaeger Collector
curl http://jaeger-collector:14269/

# Test Elasticsearch
curl http://elasticsearch-master:9200/_cluster/health
```

---

## 3. Phase 2: Service Instrumentation

**Duration**: 4-6 weeks
**Goal**: Instrument all microservices

### 3.1 Instrumentation Strategy

**Recommended Approach**: Phased Rollout

1. **Week 1-2**: Pilot services (2-3 critical services)
2. **Week 3-4**: High-traffic services
3. **Week 5-6**: Remaining services

### 3.2 Java Services

**Step 1: Add Java Agent to Dockerfile**

```dockerfile
FROM eclipse-temurin:21-jre-alpine

# Download OpenTelemetry Java Agent
ADD https://github.com/open-telemetry/opentelemetry-java-instrumentation/releases/download/v1.32.0/opentelemetry-javaagent.jar /app/opentelemetry-javaagent.jar

# Copy application JAR
COPY target/myservice.jar /app/myservice.jar

# Environment variables for OpenTelemetry
ENV JAVA_TOOL_OPTIONS="-javaagent:/app/opentelemetry-javaagent.jar"
ENV OTEL_SERVICE_NAME=myservice
ENV OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector-agent:4317
ENV OTEL_EXPORTER_OTLP_PROTOCOL=grpc
ENV OTEL_TRACES_SAMPLER=parentbased_always_on
ENV OTEL_RESOURCE_ATTRIBUTES=deployment.environment=production,service.version=1.0.0

ENTRYPOINT ["java", "-jar", "/app/myservice.jar"]
```

**Step 2: Update Kubernetes Deployment**

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: myservice
spec:
  template:
    spec:
      containers:
      - name: myservice
        image: myservice:latest
        env:
        - name: OTEL_SERVICE_NAME
          value: "myservice"
        - name: OTEL_EXPORTER_OTLP_ENDPOINT
          value: "http://otel-collector-agent:4317"
        - name: OTEL_RESOURCE_ATTRIBUTES
          value: "deployment.environment=production,service.version=1.0.0,service.namespace=microservices"
        - name: JAVA_TOOL_OPTIONS
          value: "-javaagent:/app/opentelemetry-javaagent.jar"
```

**Step 3: Deploy and Verify**

```bash
# Deploy service
kubectl apply -f myservice-deployment.yaml

# Check logs for OTel initialization
kubectl logs -f deployment/myservice | grep -i opentelemetry

# Expected: "[otel.javaagent] OpenTelemetry Javaagent enabled"

# Generate test traffic
kubectl run -it --rm test --image=curlimages/curl --restart=Never -- \
  curl http://myservice:8080/api/test

# Check Jaeger UI for traces
# Navigate to: https://jaeger.yourcompany.com
# Service: myservice
```

### 3.3 Python Services

**Step 1: Update requirements.txt**

```txt
# Add to requirements.txt
opentelemetry-api==1.21.0
opentelemetry-sdk==1.21.0
opentelemetry-exporter-otlp-proto-grpc==1.21.0
opentelemetry-instrumentation==0.42b0
opentelemetry-instrumentation-fastapi==0.42b0
opentelemetry-instrumentation-requests==0.42b0
opentelemetry-instrumentation-psycopg2==0.42b0
```

**Step 2: Create tracing.py**

```python
# See detailed code in LLD document section 2.2.2
```

**Step 3: Update main.py**

```python
# MUST be first import
from tracing import setup_telemetry

setup_telemetry()

# Now import other modules
from fastapi import FastAPI
...
```

**Step 4: Update Dockerfile**

```dockerfile
FROM python:3.11-slim

WORKDIR /app

# Copy requirements and install
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Copy application code
COPY . .

# Environment variables
ENV OTEL_SERVICE_NAME=myservice
ENV OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector-agent:4317
ENV ENVIRONMENT=production

CMD ["python", "main.py"]
```

### 3.4 Erlang Services

**Step 1: Add dependencies to rebar.config**

```erlang
{deps, [
    {opentelemetry_api, "~> 1.3"},
    {opentelemetry, "~> 1.3"},
    {opentelemetry_exporter, "~> 1.3"}
]}.
```

**Step 2: Update sys.config** (see LLD section 2.3.2)

**Step 3: Deploy and verify**

```bash
# Build release
rebar3 release

# Deploy to Kubernetes with env vars
env:
- name: OTEL_EXPORTER_OTLP_ENDPOINT
  value: "http://otel-collector-agent:4318"
```

---

## 4. Phase 3: Testing & Validation

**Duration**: 1 week
**Goal**: Ensure tracing works correctly

### 4.1 End-to-End Trace Test

```bash
# Generate a test request that spans multiple services
curl -X POST https://api.yourcompany.com/api/orders \
  -H "Content-Type: application/json" \
  -d '{"customer_id": "123", "items": [{"id": "456", "quantity": 2}]}'

# Capture trace ID from response headers
# X-Trace-Id: 0af7651916cd43dd8448eb211c80319c

# Search in Jaeger UI
# 1. Open https://jaeger.yourcompany.com
# 2. Select service: api-gateway
# 3. Paste Trace ID
# 4. Verify all expected spans are present
```

### 4.2 Validation Checklist

- [ ] All services appear in Jaeger service list
- [ ] Traces show complete request flow across services
- [ ] Database queries are captured
- [ ] Kafka message passing is traced
- [ ] Error traces are captured with stack traces
- [ ] Slow requests (>1s) are sampled
- [ ] Service dependency graph is accurate
- [ ] Trace queries return in <500ms

### 4.3 Performance Impact Assessment

```bash
# Before tracing (baseline)
ab -n 10000 -c 100 https://api.yourcompany.com/api/test

# After tracing
ab -n 10000 -c 100 https://api.yourcompany.com/api/test

# Compare:
# - Requests per second (should be >99% of baseline)
# - Mean latency (should be <1ms increase)
# - Memory usage (check with kubectl top pods)
```

---

## 5. Phase 4: Production Deployment

**Duration**: 2 weeks
**Goal**: Full production rollout

### 5.1 Gradual Rollout Plan

| Week | Services | Traffic % | Risk |
|------|----------|-----------|------|
| 1 | Critical services (5) | 100% | Medium |
| 2 | High-traffic services (10) | 100% | Medium |
| 3 | Remaining services (50+) | 100% | Low |

### 5.2 Monitoring During Rollout

```bash
# Monitor OTel Collector
kubectl top pods -n observability -l app=otel-collector-gateway

# Monitor Jaeger ingestion rate
kubectl port-forward -n observability svc/jaeger-collector 14269:14269
curl http://localhost:14269/metrics | grep jaeger_collector

# Monitor Elasticsearch health
kubectl port-forward -n observability svc/elasticsearch-master 9200:9200
curl http://localhost:9200/_cluster/health?pretty
```

### 5.3 Rollback Procedure

If issues arise:

```bash
# Option 1: Disable tracing for specific service
kubectl set env deployment/myservice OTEL_SDK_DISABLED=true

# Option 2: Scale down OTel Collectors (emergency)
kubectl scale deployment/otel-collector-gateway --replicas=0 -n observability

# Option 3: Revert to previous deployment
kubectl rollout undo deployment/myservice
```

---

## 6. Phase 5: Optimization

**Duration**: Ongoing
**Goal**: Fine-tune performance and costs

### 6.1 Sampling Optimization

```yaml
# Adjust tail-based sampling based on data
tail_sampling:
  policies:
    # Increase error sampling
    - name: error-policy
      type: status_code
      status_code:
        status_codes: [ERROR]

    # Reduce fast request sampling
    - name: probabilistic-policy
      type: probabilistic
      probabilistic:
        sampling_percentage: 5  # Reduced from 10%
```

### 6.2 Index Optimization

```bash
# Create ILM policy for automatic index deletion
curl -X PUT "elasticsearch-master:9200/_ilm/policy/jaeger-ilm-policy" \
  -H 'Content-Type: application/json' \
  -d '{
  "policy": {
    "phases": {
      "hot": {
        "actions": {
          "rollover": {
            "max_age": "1d",
            "max_size": "50gb"
          }
        }
      },
      "warm": {
        "min_age": "7d",
        "actions": {
          "readonly": {},
          "forcemerge": {
            "max_num_segments": 1
          }
        }
      },
      "cold": {
        "min_age": "14d",
        "actions": {
          "freeze": {}
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
```

---

## 7. Troubleshooting Guide

### 7.1 No Traces Appearing

**Symptoms**: Services instrumented but no traces in Jaeger

**Diagnosis**:

```bash
# Check OTel Collector logs
kubectl logs -f deployment/otel-collector-gateway -n observability

# Check Jaeger collector logs
kubectl logs -f deployment/jaeger-collector -n observability

# Check application logs
kubectl logs -f deployment/myservice | grep -i otel
```

**Common Causes**:
1. Wrong collector endpoint
2. Network policy blocking traffic
3. Sampling set to 0%
4. OTel SDK not initialized

**Solutions**:

```bash
# Test connectivity from pod
kubectl exec -it deployment/myservice -- sh
nc -zv otel-collector-agent 4317

# Check environment variables
kubectl exec -it deployment/myservice -- env | grep OTEL

# Temporarily set to always sample
kubectl set env deployment/myservice OTEL_TRACES_SAMPLER=always_on
```

### 7.2 High Latency

**Symptoms**: Traces causing >2% latency increase

**Diagnosis**:

```bash
# Check batching configuration
kubectl get cm otel-collector-gateway-config -n observability -o yaml

# Check Kafka lag (if using)
kubectl exec -it kafka-0 -n observability -- \
  kafka-consumer-groups.sh --describe --group jaeger-ingester \
  --bootstrap-server localhost:9092
```

**Solutions**:

```yaml
# Increase batch size
batch:
  timeout: 10s
  send_batch_size: 2048  # Increased
  send_batch_max_size: 4096  # Increased
```

### 7.3 Missing Spans

**Symptoms**: Incomplete traces

**Common Causes**:
1. Context not propagated
2. Async operations not traced
3. Service crashed before export

**Solutions**:

```java
// Java: Ensure context propagation in async code
CompletableFuture.supplyAsync(() -> {
    // Code here
}, Context.current().wrapExecutor(executor));
```

```python
# Python: Use context in async functions
async def my_function():
    with tracer.start_as_current_span("operation"):
        await some_async_operation()
```

### 7.4 Storage Full

**Symptoms**: Elasticsearch disk usage >85%

**Immediate Action**:

```bash
# Delete old indices
curl -X DELETE "elasticsearch-master:9200/jaeger-span-2025-09-*"

# Force merge to free space
curl -X POST "elasticsearch-master:9200/_forcemerge?max_num_segments=1"
```

**Long-term Solution**:
- Enable ILM (see section 6.2)
- Increase sampling (reduce volume)
- Add more storage

---

## 8. Success Metrics

After implementation, measure:

| Metric | Target | Current | Status |
|--------|--------|---------|--------|
| Service Coverage | 100% | __% | |
| Trace Completion Rate | >95% | __% | |
| Query Latency (p99) | <500ms | __ms | |
| Performance Impact | <1% | __% | |
| MTTR Reduction | 40% | __% | |
| Team Adoption | 80% | __% | |

---

## 9. Next Steps

After successful deployment:

1. **Training**: Conduct workshops for developers
2. **Documentation**: Create runbooks for common issues
3. **Alerts**: Set up alerts for trace system health
4. **Optimization**: Continuously tune sampling and retention
5. **Integration**: Integrate with incident management tools

---

*Document Version: 1.0*
*Last Updated: October 15, 2025*
*Status: Implementation Ready*
