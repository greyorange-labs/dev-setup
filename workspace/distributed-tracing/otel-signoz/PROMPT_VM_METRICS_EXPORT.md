# Prompt: Export Erlang VM Metrics to SigNoz

## Context

I'm implementing OpenTelemetry distributed tracing with SigNoz for my Erlang application (`butler_server_develop`). I've already set up trace export successfully, and now I want to **export Erlang VM metrics** (memory, process counts, scheduler stats, ETS tables) to SigNoz alongside traces.

## Current Setup

Based on my distributed tracing implementation:

1. **Already Implemented:**
   - OpenTelemetry tracing working with SigNoz
   - Dependencies: `opentelemetry_api ~> 1.8`, `opentelemetry_exporter`, `opentelemetry`
   - OTLP exporter configured to send traces to SigNoz at `http://localhost:4318`
   - `sys.config` has OpenTelemetry configuration for traces

2. **Existing Monitoring:**
   - I already have Prometheus metrics infrastructure (visible in Grafana dashboards)
   - Metrics like: `erlang_vm_memory_bytes_total`, `erlang_vm_process_count`, `erlang_vm_ets_table_memory_words`
   - Using Lager for logging

3. **What I Want:**
   - Export Erlang VM metrics to SigNoz (NOT logs, just metrics for now)
   - Correlate metrics with existing traces using the same `service.name`
   - Monitor: memory usage, process counts, scheduler utilization, ETS table sizes, message queue depths

## Questions

### 1. Implementation Options

I see two options:

**Option A: Native OpenTelemetry Metrics (via OTLP)**
- Use `opentelemetry_api` metrics instrumentation
- Create a custom collector module that periodically reads `erlang:memory()`, `erlang:system_info()`, `erlang:statistics()`
- Export via OTLP to SigNoz (same endpoint, different path)

**Option B: Keep Prometheus + SigNoz Scraping**
- Continue using existing Prometheus exporter
- Configure SigNoz OTLP Collector to scrape Prometheus metrics endpoint
- Less code changes

**Which option do you recommend and why?**

### 2. Configuration Analysis

Please analyze my current configuration:

**Check these files in `/Users/amar.c/workspace/gm_core/butler_server_develop/`:**
- `rebar.config` - What OpenTelemetry dependencies do I have?
- `config/sys.config` or `config/*.config` - Current OpenTelemetry configuration
- Any existing Prometheus exporter setup (search for `prometheus` in dependencies)
- `apps/*/src/*.app.src` - Application dependencies

### 3. Implementation Plan

Based on my current setup, please provide:

1. **Required Dependencies** - What do I need to add to `rebar.config`?
2. **Configuration Changes** - How to update `sys.config` to enable metrics export?
3. **Code Implementation** - A complete Erlang module for VM metrics collection
4. **SigNoz Configuration** - Any changes needed on the SigNoz side?

## Specific Metrics I Want to Export

### Memory Metrics (Gauges)
- `erlang.vm.memory.total` - Total memory
- `erlang.vm.memory.processes` - Process memory
- `erlang.vm.memory.system` - System memory
- `erlang.vm.memory.ets` - ETS table memory
- `erlang.vm.memory.binary` - Binary memory
- `erlang.vm.memory.atom` - Atom table memory

### Process Metrics (Gauges)
- `erlang.vm.process.count` - Current process count
- `erlang.vm.process.limit` - Process limit
- `erlang.vm.process.message_queue_total` - Total messages across all queues

### Scheduler Metrics (Gauges)
- `erlang.vm.scheduler.count` - Number of schedulers
- `erlang.vm.scheduler.utilization` - Per-scheduler utilization

### ETS Metrics (Gauges) - Top 10 tables
- `erlang.vm.ets.memory` - Per-table memory usage
- `erlang.vm.ets.size` - Per-table row count

### System Info (Gauges)
- `erlang.vm.uptime` - System uptime in seconds
- `erlang.vm.reductions` - Total reductions (Counter)

## Technical Requirements

1. **Collection Frequency:** Every 30-60 seconds (configurable)
2. **Resource Attributes:** Must include `service.name` matching trace config
3. **Minimal Overhead:** < 1% CPU impact for metrics collection
4. **Error Handling:** Graceful failures if SigNoz is unavailable
5. **Supervision:** Metrics collector should be supervised, restart on crash

## Expected Output

Please provide:

1. **Analysis** of my current setup and what's already in place
2. **Step-by-step implementation guide** with exact code changes
3. **Complete code example** for a VM metrics collector module
4. **Configuration snippets** for `rebar.config`, `sys.config`, `.app.src`
5. **Testing steps** to verify metrics are flowing to SigNoz
6. **SigNoz queries** to visualize the metrics

## References

My distributed tracing docs are at:
- `/Users/amar.c/workspace/dev-setup/workspace/distributed-tracing/otel-signoz/01_EXECUTIVE_SUMMARY.md`
- `/Users/amar.c/workspace/dev-setup/workspace/distributed-tracing/otel-signoz/02_TECHNICAL_ARCHITECTURE.md`
- `/Users/amar.c/workspace/dev-setup/workspace/distributed-tracing/otel-signoz/03_IMPLEMENTATION_GUIDE.md`

OpenTelemetry Erlang documentation:
- https://github.com/open-telemetry/opentelemetry-erlang
- https://hexdocs.pm/opentelemetry_api/
- https://opentelemetry.io/docs/specs/otel/metrics/

SigNoz documentation:
- https://signoz.io/docs/userguide/metrics/
- https://signoz.io/docs/instrumentation/erlang/

## Success Criteria

✅ VM metrics visible in SigNoz UI under "Metrics" section
✅ Metrics correlated with traces via `service.name` attribute
✅ Grafana-style dashboards showing memory/process/ETS trends in SigNoz
✅ < 2% performance overhead for metrics collection
✅ Metrics export continues even if SigNoz is temporarily unavailable

---

**Please analyze my butler_server_develop workspace and provide the complete implementation plan.**
