# OpenTelemetry + SigNoz Distributed Tracing
## Executive Summary & High-Level Design

**Document Version:** 1.0
**Last Updated:** November 2025
**Status:** Production Implementation Plan

---

## Table of Contents

1. [Executive Overview](#executive-overview)
2. [Business Case](#business-case)
3. [Technology Selection](#technology-selection)
4. [High-Level Architecture](#high-level-architecture)
5. [Cost-Benefit Analysis](#cost-benefit-analysis)
6. [Rollout Strategy](#rollout-strategy)
7. [Success Metrics](#success-metrics)
8. [Risk Assessment](#risk-assessment)
9. [Recommendations](#recommendations)

---

## Executive Overview

### Problem Statement

In our distributed microservices architecture spanning multiple technology stacks (Erlang, Java, Python), we face critical challenges:

- **❌ No end-to-end visibility** across service boundaries
- **❌ Difficult to trace requests** through multiple services
- **❌ Mean Time To Resolution (MTTR)** is high for production issues
- **❌ Performance bottlenecks** are hard to identify
- **❌ Manual correlation** of logs across services is time-consuming

### Solution: OpenTelemetry + SigNoz

We propose implementing **OpenTelemetry (OTel)** for distributed tracing with **SigNoz** as the observability backend.

**Key Benefits:**
- ✅ **Complete request visibility** from client → gateway → auth → backend
- ✅ **Industry standard** (CNCF graduated project)
- ✅ **Vendor-neutral** (no lock-in)
- ✅ **Multi-language support** (Python, Java, Erlang, Go, etc.)
- ✅ **Cost-effective** (open-source + self-hosted)
- ✅ **Production-ready** (used by Google, Microsoft, AWS, Uber, Shopify)

### Current Architecture

```mermaid
graph LR
    Client["Client<br/>(Python)"] --> Gateway["API Gateway<br/>(Java)"]
    Gateway --> Auth["Auth Service<br/>(Java)"]
    Auth --> GMCore["GM Core<br/>(Erlang)"]

    Gateway --> Backend["Multiple Backend Services<br/>(Kafka, Databases, External APIs)"]
    Auth --> Backend
    GMCore --> Backend

    style Client fill:#e1f5ff
    style Gateway fill:#fff4e1
    style Auth fill:#fff4e1
    style GMCore fill:#ffe1e1
    style Backend fill:#f0f0f0
```

**Problem:** No unified view of request flow, errors, or performance.

### Proposed Architecture with Tracing

```mermaid
graph TB
    Client["Client"] --> Gateway["API Gateway"]
    Gateway --> Auth["Auth Service"]
    Auth --> GMCore["GM Core"]

    Client -.->|"W3C Trace Context<br/>trace_id: abc123"| TC[Trace Context]
    Gateway -.->|"trace_id: abc123"| TC
    Auth -.->|"trace_id: abc123"| TC
    GMCore -.->|"trace_id: abc123"| TC

    Client -->|"OTLP Export"| SigNoz
    Gateway -->|"OTLP Export"| SigNoz
    Auth -->|"OTLP Export"| SigNoz
    GMCore -->|"OTLP Export"| SigNoz

    subgraph SigNoz["SigNoz Platform"]
        Receiver["OTLP Receiver<br/>(port 4318)"]
        ClickHouse["ClickHouse DB<br/>(Trace Storage)"]
        Query["Query Service"]
        UI["Web UI (port 8080)<br/>Trace Viewer"]

        Receiver --> ClickHouse
        ClickHouse --> Query
        Query --> UI
    end

    style Client fill:#e1f5ff
    style Gateway fill:#fff4e1
    style Auth fill:#fff4e1
    style GMCore fill:#ffe1e1
    style SigNoz fill:#d4f4dd
    style TC fill:#f0f0f0,stroke-dasharray: 5 5
```

**Benefit:** Complete visibility with shared `trace_id` across all services.

---

## Business Case

### Current Pain Points
- Request timeout debugging
- Performance degradation
- Inter-service errors
- API failure analysis
- Resource bottlenecks

### Expected Improvements
- Improved debugging efficiency
- Reduced MTTR
- Faster identification of bottlenecks
- Improved performance analysis
- Reduced production issue resolution time
- Improved cross-service debugging

---

## Technology Selection

### Why OpenTelemetry?

OpenTelemetry (OTel) is the **industry standard** for observability instrumentation.

#### Key Advantages

| Feature                    | Benefit                                         |
| -------------------------- | ----------------------------------------------- |
| **CNCF Graduated Project** | Production-ready, industry-backed               |
| **Vendor Neutral**         | No lock-in, switch backends anytime             |
| **Multi-Language Support** | Python, Java, Erlang, Go, .NET, Rust, Ruby, PHP |
| **W3C Standard**           | Uses W3C Trace Context for propagation          |
| **Auto-Instrumentation**   | Minimal code changes required                   |
| **Future-Proof**           | All vendors converging on OTel                  |

#### Industry Adoption

**Companies using OpenTelemetry in production:**
- Google Cloud (Cloud Trace)
- Microsoft Azure (Application Insights)
- AWS (X-Ray supports OTLP)
- Uber, Shopify, Spotify, GitHub
- Every major observability vendor

### Alternatives Comparison

| Solution                   | Pros                                                                         | Cons                                                  | Verdict                |
| -------------------------- | ---------------------------------------------------------------------------- | ----------------------------------------------------- | ---------------------- |
| **OpenTelemetry + SigNoz** | ✅ Open-source<br/>✅ Vendor-neutral<br/>✅ Multi-language<br/>✅ Cost-effective | ⚠️ Self-hosted management                              | ✅ **RECOMMENDED**      |
| **Jaeger Client**          | ✅ Lightweight                                                                | ❌ Deprecated (recommends OTel)<br/>❌ Limited features | ❌ Avoid                |
| **Zipkin**                 | ✅ Simple                                                                     | ❌ Non-standard (B3 format)<br/>❌ Declining adoption   | ❌ Avoid                |
| **Datadog APM**            | ✅ Best-in-class UI<br/>✅ Managed service                                     | ❌ **Expensive** ($5K-50K+/month)<br/>❌ Vendor lock-in | ⚠️ Budget-dependent     |
| **New Relic**              | ✅ Full APM suite<br/>✅ Managed                                               | ❌ **Expensive** ($3K-30K+/month)<br/>❌ Vendor lock-in | ⚠️ Budget-dependent     |
| **Elastic APM**            | ✅ Integrates with ELK                                                        | ❌ Complex setup<br/>❌ Resource-heavy                  | ⚠️ If already using ELK |


### Why SigNoz?

| Feature                | Description                              |
| ---------------------- | ---------------------------------------- |
| **Open-Source**        | Apache 2.0 license, free to use          |
| **OTLP Native**        | Built specifically for OpenTelemetry     |
| **ClickHouse Backend** | Fast columnar database, great for traces |
| **Modern UI**          | Clean, intuitive trace visualization     |
| **Self-Hosted**        | Data sovereignty, no vendor dependency   |
| **Active Development** | Growing community, regular updates       |

**Alternatives to SigNoz:**
- **Jaeger:** Older, less feature-rich
- **Grafana Tempo:** Good, but requires more setup
- **SigNoz Cloud:** Managed option if self-hosting is a concern

---

## High-Level Architecture

### System Components

```mermaid
graph TB
    subgraph Apps["Application Services"]
        Java["Java Apps<br/>• API Gateway<br/>• Auth"]
        Erlang["Erlang Apps<br/>• GM Core"]
        Python["Python Apps<br/>• Clients (Test)"]
    end

    Apps -->|"OpenTelemetry<br/>SDK/Agent"| OTLP

    OTLP["OTLP Export<br/>(port 4318)"]

    OTLP --> SigNoz

    subgraph SigNoz["SigNoz Platform"]
        Receiver["OTLP Receiver<br/>(port 4318)"]
        DB["ClickHouse<br/>Database"]
        Query["Query Service"]
        UI["Web UI<br/>(port 8080)"]

        Receiver --> DB
        DB --> Query
        Query --> UI
    end

    style Apps fill:#e1f5ff
    style SigNoz fill:#d4f4dd
```

### Trace Flow Example

**Scenario:** User requests order diagnostics

```mermaid
sequenceDiagram
    participant Client
    participant Gateway as API Gateway
    participant Auth as Auth Service
    participant GMCore as GM Core
    participant SigNoz

    Note over Client: 1. Generate trace_id: abc123
    Client->>Gateway: GET /api-gateway/gm_core/...<br/>traceparent: 00-abc123-span001-01

    Note over Gateway: 2. Extract trace_id: abc123<br/>Create span: "auth.validate"
    Gateway->>Auth: Validate<br/>traceparent: 00-abc123-span002-01

    Note over Auth: 3. Validate credentials<br/>Create span: "db.query"
    Auth-->>Gateway: Response
    Auth->>SigNoz: Export spans

    Note over Gateway: 4. Continue trace_id: abc123<br/>Create span: "http.client.gm_core"
    Gateway->>GMCore: Forward Request<br/>traceparent: 00-abc123-span003-01

    Note over GMCore: 5. Extract trace_id: abc123<br/>Create span: "diagnose.infrastructure"<br/>Execute business logic
    GMCore-->>Gateway: Response
    GMCore->>SigNoz: Export spans

    Gateway-->>Client: Response
    Gateway->>SigNoz: Export spans
    Client->>SigNoz: Export spans

    Note over SigNoz: 6. Collect all spans with trace_id: abc123<br/>Build complete trace tree<br/>Display in Web UI
```

**Result:** Complete visibility from Client → Gateway → Auth → GMCore

### Technology Stack by Service

| Service            | Language | OTel Implementation | Auto-Instrumentation         |
| ------------------ | -------- | ------------------- | ---------------------------- |
| **API Gateway**    | Java     | Java Agent          | ✅ Spring WebFlux, HTTP, JDBC |
| **Auth Service**   | Java     | Java Agent          | ✅ Spring MVC, Tomcat, JDBC   |
| **GM Core**        | Erlang   | OTel Erlang SDK     | ✅ HTTP, Partial Kafka        |
| **Python Clients** | Python   | OTel Python SDK     | ✅ requests library           |

---

## Application Performance Impact

| Service Type          | CPU Overhead | Memory Overhead | Network Overhead |
| --------------------- | ------------ | --------------- | ---------------- |
| **Java (with agent)** | < 1%         | 50-100MB        | ~1KB/request     |
| **Erlang (SDK)**      | < 1%         | 10-50MB         | ~1KB/request     |
| **Python (SDK)**      | < 1%         | 10-30MB         | ~1KB/request     |

**Impact Assessment:** **Negligible** - Production systems typically won't notice the overhead.

### Qualitative Benefits

- ✅ **Improved Developer Productivity** - Less time debugging, more time building
- ✅ **Better Customer Experience** - Faster issue resolution, less downtime
- ✅ **Enhanced System Understanding** - Visual map of system behavior
- ✅ **Proactive Problem Detection** - Identify issues before customers report them
- ✅ **Data-Driven Optimization** - Performance improvements based on real data
- ✅ **Reduced Cognitive Load** - No more mental correlation of logs

---


## Environment Strategy

| Environment                   | Sampling Rate         | Purpose                            |
| ----------------------------- | --------------------- | ---------------------------------- |
| **Development**               | 100% (always_on)      | Full visibility for debugging      |
| **Staging**                   | 100% (always_on)      | Performance testing & validation   |
| **Production (Pilot)**        | 100% (always_on)      | Initial validation of 1-2 services |
| **Production (Full)**         | 10% (trace_id_ratio)  | Balance visibility vs overhead     |
| **Production (High-Traffic)** | 1-5% (trace_id_ratio) | High-volume services               |

## Risk Mitigation

| Risk                     | Mitigation Strategy                                                                                |
| ------------------------ | -------------------------------------------------------------------------------------------------- |
| **Performance Impact**   | • Start with 10% sampling<br/>• Monitor CPU/memory<br/>• Emergency off switch ready                |
| **Resource Exhaustion**  | • Right-size SigNoz infrastructure<br/>• Set data retention policies<br/>• Monitor disk usage      |
| **Service Disruption**   | • Deploy during low-traffic windows<br/>• Rollback plan ready<br/>• Gradual rollout                |
| **Configuration Errors** | • Thorough testing in dev/staging<br/>• Configuration validation scripts<br/>• Peer review process |

---

## Success Metrics

### Key Performance Indicators (KPIs)

#### Operational Metrics
- Reduce Mean Time To Detection
- Reduce Mean Time To Resolution
- Debug Time per Issue*
- P1 Incident Resolution Time

#### System Health Metrics

| Metric                        | Target     | Monitoring          |
| ----------------------------- | ---------- | ------------------- |
| **Trace Sampling Rate**       | 10% (prod) | SigNoz dashboard    |
| **Trace Export Success Rate** | > 99.5%    | OTel metrics        |
| **End-to-End Latency Impact** | < 1ms      | Application metrics |
| **SigNoz Query Performance**  | < 2s (p95) | SigNoz UI           |


---

## Risk Assessment

### Technical Risks

| Risk                        | Probability | Impact | Mitigation                                                                                   |
| --------------------------- | ----------- | ------ | -------------------------------------------------------------------------------------------- |
| **Performance Degradation** | Low         | High   | • Start with low sampling (10%)<br/>• Monitor continuously<br/>• Emergency disable mechanism |
| **Storage Capacity**        | Medium      | Medium | • Implement data retention policies<br/>• Monitor disk usage<br/>• Scale storage proactively |
| **Network Overhead**        | Low         | Low    | • Batch span export (default)<br/>• Compression enabled<br/>• Monitor network metrics        |
| **OTel SDK Bugs**           | Low         | Medium | • Use stable versions only<br/>• Test thoroughly in staging<br/>• Have rollback plan         |

### Operational Risks

| Risk                      | Probability | Impact | Mitigation                                                                                       |
| ------------------------- | ----------- | ------ | ------------------------------------------------------------------------------------------------ |
| **Team Adoption**         | Medium      | High   | • Comprehensive training<br/>• Clear documentation<br/>• Regular demos & success stories         |
| **Configuration Drift**   | Medium      | Low    | • Infrastructure as Code<br/>• Configuration validation<br/>• Regular audits                     |
| **SigNoz Downtime**       | Low         | Medium | • High availability setup<br/>• Monitoring & alerting<br/>• Application continues without traces |
| **Data Privacy Concerns** | Low         | High   | • Never log PII in spans<br/>• Attribute sanitization<br/>• Regular security reviews             |  |

### Overall Risk Rating: **LOW**

**Why Low Risk:**
- ✅ Industry-standard technology (OTel)
- ✅ Minimal code changes required
- ✅ Gradual rollout strategy
- ✅ Emergency off switch available
- ✅ Strong community support
- ✅ Proven in production at scale

---

### Decision Required

- [ ] Infrastructure provisioning (SigNoz servers)
- [ ] Rollout timeline

---

## Appendices

### Appendix A: Technology Stack Summary

| Component                  | Technology        | Version | Purpose                |
| -------------------------- | ----------------- | ------- | ---------------------- |
| **Instrumentation**        | OpenTelemetry     | 1.x+    | Trace generation       |
| **Java Instrumentation**   | OTel Java Agent   | 2.10.0+ | Auto-instrumentation   |
| **Erlang Instrumentation** | opentelemetry_api | 1.8+    | Manual instrumentation |
| **Protocol**               | OTLP/HTTP         | 1.0     | Trace export           |
| **Backend**                | SigNoz            | Latest  | Trace storage & UI     |
| **Database**               | ClickHouse        | 23.x+   | Trace persistence      |

### Appendix B: References

**OpenTelemetry:**
- Official Docs: https://opentelemetry.io
- CNCF Project: https://www.cncf.io/projects/opentelemetry/
- W3C Trace Context: https://www.w3.org/TR/trace-context/

**SigNoz:**
- Official Docs: https://signoz.io/docs/
- GitHub: https://github.com/SigNoz/signoz
- Deployment Guide: https://signoz.io/docs/install/

---

**Document Status:** ✅ Ready for Review
**Next Review Date:** Post-implementation

