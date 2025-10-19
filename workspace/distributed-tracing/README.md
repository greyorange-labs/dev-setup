# Quick Start Guide - Distributed Tracing

## 🚀 TL;DR

**Recommendation**: Use **OpenTelemetry + Jaeger**

### Why?
- ✅ Best support for your tech stack (Java, Python, Erlang, Node.js)
- ✅ Industry standard (CNCF-backed)
- ✅ No vendor lock-in
- ✅ Cost-effective (~$45k/year vs $60k+ for commercial solutions)
- ✅ Future-proof

### Architecture in 30 Seconds

```
Your Services (Java/Python/Erlang/Node.js)
    ↓ (instrumented with OpenTelemetry SDKs)
OTel Collectors (collect & process traces)
    ↓
Kafka (buffer) → Jaeger (backend) → Elasticsearch (storage)
    ↓
Jaeger UI (visualize & query traces)
```

---

## 📚 Documentation Index

| Document | Purpose | When to Read |
|----------|---------|--------------|
| **[00_OVERVIEW.md](00_OVERVIEW.md)** | Executive summary | Start here |
| **[01_HLD_HIGH_LEVEL_DESIGN.md](01_HLD_HIGH_LEVEL_DESIGN.md)** | Architecture & design decisions | Architecture review |
| **[02_LLD_LOW_LEVEL_DESIGN.md](02_LLD_LOW_LEVEL_DESIGN.md)** | Implementation details & code | During implementation |
| **[03_COMPARISON_TOOLS.md](03_COMPARISON_TOOLS.md)** | OpenTelemetry vs alternatives | Decision making |
| **[04_IMPLEMENTATION_GUIDE.md](04_IMPLEMENTATION_GUIDE.md)** | Step-by-step setup | During deployment |
| **[05_MIGRATION_STRATEGY.md](05_MIGRATION_STRATEGY.md)** | Rollout plan | Planning phase |
| **[06_OPERATIONAL_GUIDE.md](06_OPERATIONAL_GUIDE.md)** | Day-to-day operations | Post-deployment |

---

## 🎯 Quick Implementation Path

### Phase 1: Setup Infrastructure (Week 1-2)
```bash
# Deploy to Kubernetes
1. Deploy Elasticsearch cluster
2. Deploy Kafka cluster
3. Deploy Jaeger (collector, query, UI)
4. Deploy OTel Collectors
5. Verify end-to-end flow
```
**See**: [04_IMPLEMENTATION_GUIDE.md](04_IMPLEMENTATION_GUIDE.md#phase-1-infrastructure-setup)

### Phase 2: Instrument Services (Week 3-4)

#### Java Services (Easiest)
```dockerfile
# Just add Java agent to your Dockerfile
ADD https://github.com/open-telemetry/opentelemetry-java-instrumentation/releases/latest/download/opentelemetry-javaagent.jar /app/otel-agent.jar
ENV JAVA_TOOL_OPTIONS="-javaagent:/app/otel-agent.jar"
ENV OTEL_SERVICE_NAME=myservice
ENV OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4317
```
**Zero code changes required!** ✨

#### Python Services
```python
# Add to requirements.txt
opentelemetry-api
opentelemetry-sdk
opentelemetry-exporter-otlp-proto-grpc
opentelemetry-instrumentation-fastapi

# Add tracing.py (see LLD doc)
# Import at top of main.py
```

#### Erlang Services
```erlang
%% Add to rebar.config
{deps, [
    {opentelemetry_api, "~> 1.3"},
    {opentelemetry, "~> 1.3"}
]}.

%% Configure in sys.config (see LLD doc)
```

**See**: [02_LLD_LOW_LEVEL_DESIGN.md](02_LLD_LOW_LEVEL_DESIGN.md#2-language-specific-implementation)

### Phase 3: Production Rollout (Week 5-8)
```
Week 5-6: High-traffic services
Week 7-8: Remaining services
```
**See**: [05_MIGRATION_STRATEGY.md](05_MIGRATION_STRATEGY.md#3-phased-rollout-plan)

---

## 🔑 Key Concepts

### What is Distributed Tracing?

**Before** (logs only):
```
[Service A] Order created: order_id=123
[Service B] Inventory checked
[Service C] Payment processed
```
**Problem**: Can't correlate these logs across services!

**After** (with tracing):
```
Trace ID: abc123
├─ [Service A] Order created (10ms)
│  ├─ [Service B] Check inventory (5ms)
│  └─ [Service C] Process payment (50ms)
└─ Total: 65ms
```
**Benefit**: See complete request flow across all services! 🎉

### Core Components

| Component | What It Does | Example |
|-----------|--------------|---------|
| **Trace** | Complete request journey | User checkout flow |
| **Span** | Single operation | Database query |
| **Trace ID** | Unique ID for trace | `abc123` |
| **Span ID** | Unique ID for span | `span456` |
| **Context** | Propagated metadata | HTTP headers |

---

## 💰 Cost Estimate

### Year 1
- **Infrastructure**: $43,000 (AWS, self-hosted)
- **Setup effort**: $10,000 (2 engineers × 2 weeks)
- **Maintenance**: $50,000 (0.5 FTE)
- **Total**: ~$105,000

### Year 2+
- **Infrastructure**: $45,000/year
- **Maintenance**: $50,000/year
- **Total**: ~$95,000/year

**vs Commercial APM** (Datadog/New Relic): $60,000+/year with less flexibility

**See**: [03_COMPARISON_TOOLS.md](03_COMPARISON_TOOLS.md#5-cost-analysis)

---

## ⚡ Common Commands

### Check Health
```bash
# All pods running?
kubectl get pods -n observability

# Elasticsearch healthy?
kubectl exec -n observability elasticsearch-master-0 -- \
  curl -s localhost:9200/_cluster/health

# Traces flowing?
curl http://jaeger-query:16686/api/services
```

### Debug Issues
```bash
# Find trace by ID
curl "http://jaeger-query:16686/api/traces/0af7651916cd43dd8448eb211c80319c"

# Check collector logs
kubectl logs -n observability -l app=otel-collector-gateway --tail=100

# Test connectivity
kubectl exec -it deployment/myservice -- nc -zv otel-collector 4317
```

### Generate Test Traces
```bash
# Make request with trace context
curl -H "traceparent: 00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01" \
  https://api.yourcompany.com/test
```

**See**: [06_OPERATIONAL_GUIDE.md](06_OPERATIONAL_GUIDE.md#troubleshooting) for more

---

## 🎓 Learning Path

### Week 1: Understanding
- [ ] Read [00_OVERVIEW.md](00_OVERVIEW.md)
- [ ] Review [01_HLD_HIGH_LEVEL_DESIGN.md](01_HLD_HIGH_LEVEL_DESIGN.md)
- [ ] Watch OpenTelemetry intro videos

### Week 2: Planning
- [ ] Read [03_COMPARISON_TOOLS.md](03_COMPARISON_TOOLS.md)
- [ ] Review [05_MIGRATION_STRATEGY.md](05_MIGRATION_STRATEGY.md)
- [ ] Plan pilot services

### Week 3-4: Implementation
- [ ] Follow [04_IMPLEMENTATION_GUIDE.md](04_IMPLEMENTATION_GUIDE.md)
- [ ] Instrument pilot services
- [ ] Deploy to staging

### Week 5+: Rollout
- [ ] Gradual production rollout
- [ ] Train teams
- [ ] Optimize based on [06_OPERATIONAL_GUIDE.md](06_OPERATIONAL_GUIDE.md)

---

## 🆘 Getting Help

### Internal Resources
- **Slack**: #distributed-tracing
- **Wiki**: https://wiki.company.com/tracing
- **Office Hours**: Tue/Thu 2-3pm
- **On-Call**: PagerDuty rotation

### External Resources
- **OpenTelemetry Docs**: https://opentelemetry.io/docs/
- **Jaeger Docs**: https://www.jaegertracing.io/docs/
- **CNCF Slack**: #opentelemetry channel

---

## ✅ Success Metrics

After implementation, you should achieve:

| Metric | Target | How to Measure |
|--------|--------|----------------|
| **Service Coverage** | 100% | All services in Jaeger |
| **Performance Impact** | <1% | Before/after benchmarks |
| **MTTR Reduction** | 40% | Incident resolution time |
| **Team Adoption** | 80% | Traces used in debugging |
| **Query Speed** | <500ms | p99 query latency |

---

## 🚨 Common Pitfalls & Solutions

### ❌ Pitfall 1: "I don't see any traces!"
**Solutions**:
- Check OTel Collector endpoint is correct
- Verify network connectivity
- Check sampling isn't set to 0%
- Review application logs for errors

### ❌ Pitfall 2: "Traces are incomplete"
**Solutions**:
- Ensure trace context is propagated in HTTP headers
- Check async operations are instrumented
- Verify all services are instrumented

### ❌ Pitfall 3: "System is too slow"
**Solutions**:
- Increase batch sizes in collectors
- Enable async span export
- Reduce sampling rate
- Scale collectors horizontally

### ❌ Pitfall 4: "Storage is filling up"
**Solutions**:
- Implement ILM (Index Lifecycle Management)
- Reduce sampling rate
- Shorten retention period
- Archive old data to S3

**See**: [06_OPERATIONAL_GUIDE.md](06_OPERATIONAL_GUIDE.md#5-troubleshooting) for detailed troubleshooting

---

## 📋 Pre-Deployment Checklist

- [ ] Kubernetes cluster ready (1.24+)
- [ ] Storage class configured
- [ ] Ingress controller deployed
- [ ] Monitoring (Prometheus) available
- [ ] Budget approved (~$105k Year 1)
- [ ] Team trained on basics
- [ ] Pilot services identified
- [ ] Rollback plan documented

---

## 🎬 Next Steps

1. **Get Buy-In**: Share [00_OVERVIEW.md](00_OVERVIEW.md) with stakeholders
2. **Architecture Review**: Present [01_HLD_HIGH_LEVEL_DESIGN.md](01_HLD_HIGH_LEVEL_DESIGN.md)
3. **Plan Deployment**: Use [05_MIGRATION_STRATEGY.md](05_MIGRATION_STRATEGY.md)
4. **Start Implementation**: Follow [04_IMPLEMENTATION_GUIDE.md](04_IMPLEMENTATION_GUIDE.md)
5. **Go Live**: Refer to [06_OPERATIONAL_GUIDE.md](06_OPERATIONAL_GUIDE.md)

---

## 📞 Contact

- **Technical Lead**: [Your Name]
- **Architecture Review**: [Architecture Team]
- **Implementation Support**: [Platform Team]

---

**Good luck! You're about to gain superpowers in debugging distributed systems! 🦸‍♂️**

---

*Quick Start Guide Version: 1.0*
*Last Updated: October 15, 2025*
