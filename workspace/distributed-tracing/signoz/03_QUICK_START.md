# SigNoz Quick Start Guide

## What is SigNoz?

SigNoz is an **all-in-one observability platform** that provides:
- 📊 **Metrics Monitoring** (like Prometheus + Grafana)
- 🔍 **Distributed Tracing** (like Jaeger)
- 📝 **Log Management** (like ELK/Loki)
- 📈 **Dashboards & Alerting** (built-in)

All unified in a single platform!

---

## Quick Decision: When to Choose SigNoz?

### ✅ Choose SigNoz if:
- You need **all three**: traces + metrics + logs
- You want **unified observability** (one tool, one UI)
- You need **built-in dashboards and alerting**
- You have **resources** (8GB+ RAM for POC, 63GB+ for prod)
- You prioritize **query performance** (ClickHouse is fast)
- Your team is **small** (less operational overhead)

### ⚠️ Consider Jaeger instead if:
- You **only need tracing** (don't pay for unused features)
- You have **limited resources** (Jaeger uses 1/5th the resources)
- You want **faster POC testing** (30s vs 3min startup)
- You prioritize **maturity** (9 years vs 3 years)
- You already have **Prometheus + Grafana** setup

---

## 5-Minute Quick Start

### Prerequisites
- Docker + Docker Compose installed
- 8GB+ RAM available
- 20GB+ disk space

### Step 1: Install SigNoz

```bash
# Clone SigNoz repository
git clone https://github.com/SigNoz/signoz.git
cd signoz/deploy

# Install (pulls all containers)
./install.sh

# Wait 2-3 minutes for all services to start
# Watch progress
docker-compose logs -f
```

### Step 2: Access SigNoz UI

```bash
# Open in browser
open http://localhost:8080

# First time: Create admin account
# Email: your-email@example.com
# Password: (create strong password)
```

### Step 3: Instrument Your First Application

**Java Example**:
```bash
# Download OpenTelemetry Java Agent
wget https://github.com/open-telemetry/opentelemetry-java-instrumentation/releases/latest/download/opentelemetry-javaagent.jar

# Run your application
java -javaagent:opentelemetry-javaagent.jar \
  -Dotel.service.name=my-service \
  -Dotel.exporter.otlp.endpoint=http://localhost:4317 \
  -Dotel.traces.exporter=otlp \
  -Dotel.metrics.exporter=otlp \
  -Dotel.logs.exporter=otlp \
  -jar my-application.jar
```

**Python Example**:
```bash
# Install packages
pip install opentelemetry-distro opentelemetry-exporter-otlp

# Auto-instrument
opentelemetry-bootstrap -a install

# Run your app
opentelemetry-instrument \
  --service_name my-service \
  --exporter_otlp_endpoint http://localhost:4317 \
  python app.py
```

### Step 4: Generate Traffic & View

```bash
# Generate some requests to your app
curl http://localhost:8080/api/endpoint

# Go to SigNoz UI
# Navigate to "Services" tab
# Click on your service
# View traces, metrics, and logs!
```

---

## What You Get Out of the Box

### 1. Services Page
- List of all instrumented services
- Key metrics (RPS, error rate, latency)
- Service health overview

### 2. Traces Page
- Search traces by service, operation, tags
- Flamegraph visualization
- Span details with attributes
- Trace comparison

### 3. Metrics Page (NEW vs Jaeger!)
- Custom dashboards
- PromQL queries
- RED metrics (Rate, Errors, Duration)
- Infrastructure metrics

### 4. Logs Page (NEW vs Jaeger!)
- Log search with filters
- Automatic correlation with traces
- JSON log parsing
- Live tail

### 5. Service Map
- Visual dependency graph
- Request rates between services
- Error rates per connection

### 6. Alerts (NEW vs Jaeger!)
- Create alert rules
- Multiple notification channels:
  - Slack
  - PagerDuty
  - Email
  - Webhooks

---

## Key Features Demo

### Feature 1: Unified View (Traces + Logs)

```bash
# In SigNoz UI:
1. Find a slow trace
2. Click "View Logs" button
3. See all logs for that trace automatically
4. No manual correlation needed!
```

### Feature 2: Custom Dashboards

```bash
# In SigNoz UI:
1. Go to Dashboards
2. Click "New Dashboard"
3. Add panels:
   - Request rate (PromQL)
   - Error rate
   - P99 latency
   - Custom metrics
4. Save and share with team
```

### Feature 3: Alerting

```bash
# Create alert:
1. Go to Alerts → New Alert
2. Set condition:
   - Metric: http_server_request_duration_seconds
   - Condition: P99 > 1 second
   - For: 5 minutes
3. Add notification: Slack channel
4. Save
```

---

## Resource Usage (Real Numbers)

### Local POC
```
CPU:     4-8 cores
Memory:  5-8GB
Disk:    6-10GB
Startup: 2-3 minutes
```

### Production (50 services)
```
CPU:     20+ cores
Memory:  63GB+
Disk:    1.5TB+
Cost:    ~$45k/year (AWS)
```

**Compare with Jaeger**:
- Jaeger POC: 2 cores, 1GB RAM, 30s startup
- Jaeger Prod: 8 cores, 20GB RAM, ~$36k/year

**Trade-off**: SigNoz uses more resources but includes metrics + logs + dashboards + alerting!

---

## Common Issues & Solutions

### Issue: High Memory Usage

```bash
# Reduce ClickHouse memory
docker-compose down
# Edit clickhouse-config.xml
# Set: <max_memory_usage>2000000000</max_memory_usage>
docker-compose up -d
```

### Issue: Containers Won't Start

```bash
# Check Docker resources
docker stats

# Increase Docker memory limit
# Docker Desktop → Settings → Resources → Memory → 8GB+
```

### Issue: No Data Showing

```bash
# Check if OTel Collector receiving data
docker logs signoz-otel-collector --tail 100

# Check ClickHouse has data
docker exec signoz-clickhouse clickhouse-client --query \
  "SELECT count() FROM signoz_traces.signoz_index_v2"

# Verify app OTLP endpoint is correct: http://localhost:4317
```

---

## Next Steps

1. ✅ **Successfully started SigNoz**
2. ✅ **Instrumented first application**
3. ✅ **Viewed traces, metrics, logs**
4. 📖 **Deep dive**: Read [01_ARCHITECTURE.md](01_ARCHITECTURE.md)
5. 🔧 **Full POC**: Follow [02_LOCAL_POC_SETUP.md](02_LOCAL_POC_SETUP.md)
6. ⚖️ **Compare**: Review [../COMPARISON_ANALYSIS.md](../COMPARISON_ANALYSIS.md)

---

## Evaluation Checklist

Use this to evaluate SigNoz during your POC:

### Features ✓
- [ ] Trace visualization works
- [ ] Service map generates correctly
- [ ] Metrics dashboards are useful
- [ ] Log correlation is automatic
- [ ] Alerting meets needs
- [ ] Query performance is good

### Operations ✓
- [ ] Easy to deploy locally
- [ ] Resource usage is acceptable
- [ ] Troubleshooting is straightforward
- [ ] Documentation is helpful
- [ ] Community is responsive

### Comparison with Jaeger ✓
- [ ] Resource usage difference acceptable?
- [ ] Extra features worth the cost?
- [ ] Unified UI better than separate tools?
- [ ] Query performance noticeably better?
- [ ] Erlang support same as Jaeger?

---

## Key Takeaways

1. **SigNoz = All-in-One**: Traces + Metrics + Logs in one platform
2. **Resource Intensive**: 5x more resources than Jaeger POC
3. **Rich Features**: Built-in dashboards, alerting, log correlation
4. **Same Erlang Support**: Both use OpenTelemetry SDK
5. **Less Mature**: 3 years old vs Jaeger's 9 years
6. **Cost Effective (Full Stack)**: $265k vs $367k for Jaeger + Prom + ELK

**Bottom Line**: SigNoz is excellent if you need unified observability and have the resources. If you only need tracing or have limited resources, start with Jaeger.

---

## Learn More

- **[Architecture Deep Dive](01_ARCHITECTURE.md)** - How SigNoz works
- **[Full POC Guide](02_LOCAL_POC_SETUP.md)** - Comprehensive setup
- **[Comparison Analysis](../COMPARISON_ANALYSIS.md)** - SigNoz vs Jaeger
- **[Official Docs](https://signoz.io/docs/)** - SigNoz documentation

---

*Document Version: 1.0*
*Last Updated: November 7, 2025*
*Status: Quick Start Guide*

