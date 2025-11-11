# OpenTelemetry + SigNoz Distributed Tracing
## Implementation Guide & Developer Handbook

**Document Version:** 1.0
**Last Updated:** November 2025
**Target Audience:** Developers, Technical Leads

---

## Table of Contents

1. [Implementation Overview](#implementation-overview)
2. [Java Services Implementation](#java-services-implementation)
3. [Erlang Services Implementation](#erlang-services-implementation)
4. [Refactoring Requirements](#refactoring-requirements)
5. [Developer Guidelines](#developer-guidelines)
6. [Testing & Validation](#testing--validation)
7. [Common Patterns & Examples](#common-patterns--examples)
8. [Troubleshooting](#troubleshooting)

---

## Implementation Overview

### Prerequisites

Before implementing OpenTelemetry tracing:

**Infrastructure:**
- ✅ SigNoz deployed and accessible (port 4318 for OTLP, port 8080 for UI)
- ✅ Network connectivity from application servers to SigNoz
- ✅ Sufficient resources allocated (see 02_TECHNICAL_ARCHITECTURE.md)

**Development Environment:**
- ✅ Java: JDK 11+ (recommended: JDK 21)
- ✅ Erlang: OTP 25+ (recommended: OTP 26)
- ✅ Build tools: Maven 3.6+, rebar3 3.20+
- ✅ Access to artifact repositories

**Knowledge:**
- Basic understanding of distributed tracing concepts
- Familiarity with your service's architecture
- Understanding of your deployment process

### Implementation Effort Estimates

| Service Type                  | Setup Time | Refactoring Time | Testing Time | Total          |
| ----------------------------- | ---------- | ---------------- | ------------ | -------------- |
| **Java Service (with agent)** | 1 hour     | 0-2 hours        | 2 hours      | **3-5 hours**  |
| **Erlang Service**            | 2 hours    | 4-8 hours        | 2-4 hours    | **8-14 hours** |
| **Integration Testing**       | -          | -                | 4-8 hours    | **4-8 hours**  |

**Per service average:** 1-2 days of developer time

---

## Java Services Implementation

### Step 1: Download OpenTelemetry Java Agent

**One-time setup per deployment environment:**

```bash
# Download to shared location (e.g., /opt/observability/)
cd /opt/observability/

# Latest version
curl -L -O https://github.com/open-telemetry/opentelemetry-java-instrumentation/releases/latest/download/opentelemetry-javaagent.jar

# OR specific version (recommended for production)
VERSION=2.10.0
curl -L -O https://github.com/open-telemetry/opentelemetry-java-instrumentation/releases/download/v${VERSION}/opentelemetry-javaagent.jar

# Verify download
ls -lh opentelemetry-javaagent.jar
# Expected: ~16 MB file
```

### Step 2: Configure Service Startup

#### Option A: Command Line Arguments (Quick Start)

```bash
java \
  -javaagent:/opt/observability/opentelemetry-javaagent.jar \
  -Dotel.service.name=api-gateway \
  -Dotel.traces.exporter=otlp \
  -Dotel.exporter.otlp.endpoint=http://localhost:4318 \
  -Dotel.exporter.otlp.protocol=http/protobuf \
  -Dotel.traces.sampler=always_on \
  -Dotel.propagators=tracecontext,baggage \
  -Dotel.resource.attributes=environment=dev,version=1.0.0 \
  -jar target/api-gateway.war
```

#### Option B: Environment Variables (Recommended for Production)

```bash
# Create environment file: /etc/default/api-gateway
export JAVA_TOOL_OPTIONS="-javaagent:/opt/observability/opentelemetry-javaagent.jar"
export OTEL_SERVICE_NAME="api-gateway"
export OTEL_TRACES_EXPORTER="otlp"
export OTEL_EXPORTER_OTLP_ENDPOINT="http://signoz-collector:4318"
export OTEL_EXPORTER_OTLP_PROTOCOL="http/protobuf"
export OTEL_TRACES_SAMPLER="traceidratio"
export OTEL_TRACES_SAMPLER_ARG="0.1"  # 10% sampling
export OTEL_PROPAGATORS="tracecontext,baggage"
export OTEL_RESOURCE_ATTRIBUTES="environment=production,version=1.0.0,team=platform"

# Run service (agent auto-attaches)
java -jar target/api-gateway.war
```

#### Option C: systemd Service Configuration

```ini
# /etc/systemd/system/api-gateway.service
[Unit]
Description=API Gateway Service with OpenTelemetry
After=network.target

[Service]
Type=simple
User=appuser
WorkingDirectory=/opt/api-gateway
EnvironmentFile=/etc/default/api-gateway
ExecStart=/usr/bin/java -jar /opt/api-gateway/api-gateway.war
Restart=on-failure
RestartSec=10

[Install]
WantedBy=multi-user.target
```

```bash
# Reload and restart
sudo systemctl daemon-reload
sudo systemctl restart api-gateway
sudo systemctl status api-gateway

# Check logs for OTel agent initialization
sudo journalctl -u api-gateway -f | grep -i opentelemetry
# Expected: "OpenTelemetry Javaagent 2.x.x started"
```

### Step 3: Configuration Options

#### Essential Configuration

| Variable                      | Value                  | Purpose              |
| ----------------------------- | ---------------------- | -------------------- |
| `OTEL_SERVICE_NAME`           | Service identifier     | Appears in SigNoz UI |
| `OTEL_TRACES_EXPORTER`        | `otlp`                 | Use OTLP protocol    |
| `OTEL_EXPORTER_OTLP_ENDPOINT` | `http://host:4318`     | SigNoz OTLP receiver |
| `OTEL_TRACES_SAMPLER`         | `traceidratio`         | Sampling algorithm   |
| `OTEL_TRACES_SAMPLER_ARG`     | `0.1`                  | 10% sampling rate    |
| `OTEL_PROPAGATORS`            | `tracecontext,baggage` | W3C standard         |

#### Optional Configuration

| Variable                         | Value                   | Purpose                 |
| -------------------------------- | ----------------------- | ----------------------- |
| `OTEL_RESOURCE_ATTRIBUTES`       | `key=value,key2=value2` | Extra metadata          |
| `OTEL_EXPORTER_OTLP_HEADERS`     | `auth=token`            | Authentication          |
| `OTEL_EXPORTER_OTLP_COMPRESSION` | `gzip`                  | Enable compression      |
| `OTEL_EXPORTER_OTLP_TIMEOUT`     | `10000`                 | Timeout in milliseconds |

#### Filtering Unwanted Traces

**Disable database tracing (removes noise):**

```bash
export OTEL_INSTRUMENTATION_JDBC_ENABLED=false
export OTEL_INSTRUMENTATION_SPRING_DATA_ENABLED=false
export OTEL_INSTRUMENTATION_HIBERNATE_ENABLED=false
export OTEL_INSTRUMENTATION_JDBC_DATASOURCE_ENABLED=false
```

**Ignore specific HTTP endpoints:**

```bash
export OTEL_INSTRUMENTATION_HTTP_SERVER_IGNORE_PATHS="/actuator/*,/health,/metrics"
```

**Disable specific instrumentations:**

```bash
# Keep only HTTP tracing, disable everything else
export OTEL_INSTRUMENTATION_SPRING_WEBFLUX_ENABLED=true
export OTEL_INSTRUMENTATION_SPRING_WEB_ENABLED=true
export OTEL_INSTRUMENTATION_KAFKA_ENABLED=false
export OTEL_INSTRUMENTATION_REDIS_ENABLED=false
```

### Step 4: Verify Java Service

#### Check Logs

```bash
# Look for agent initialization
grep -i "opentelemetry" /var/log/api-gateway.log

# Expected output:
# [otel.javaagent 2024-xx-xx] OpenTelemetry Javaagent 2.10.0
# [otel.javaagent 2024-xx-xx] Using OtlpHttpSpanExporter with endpoint=http://localhost:4318
```

#### Test HTTP Request

```bash
# Make a test request
curl -X GET http://localhost:8080/api-gateway/gm_core/api/butler_shared/v2/diagnose/infra

# Check response headers for trace context
curl -v http://localhost:8080/api-gateway/gm_core/status 2>&1 | grep traceparent
# Expected: traceparent: 00-<trace_id>-<span_id>-01
```

#### Verify in SigNoz

1. Open SigNoz UI: http://localhost:8080
2. Navigate to **Services** → Should see `api-gateway`
3. Navigate to **Traces** → Filter by service: `api-gateway`
4. Click any trace → Verify spans appear

### Step 5: Zero Code Changes Required ✨

**The Java agent automatically instruments:**
- ✅ All HTTP requests (incoming and outgoing)
- ✅ Database queries (JDBC, Hibernate, JPA)
- ✅ Kafka producers and consumers
- ✅ Redis operations
- ✅ Spring framework internals

**No application code changes needed!**

### Optional: Manual Instrumentation (Advanced)

If you want custom spans for specific business logic:

```java
import io.opentelemetry.api.GlobalOpenTelemetry;
import io.opentelemetry.api.trace.Span;
import io.opentelemetry.api.trace.Tracer;
import io.opentelemetry.context.Scope;

public class OrderService {

    private static final Tracer tracer =
        GlobalOpenTelemetry.getTracer("order-service");

    public void processOrder(String orderId) {
        // Create custom span
        Span span = tracer.spanBuilder("process_order")
            .setAttribute("order.id", orderId)
            .startSpan();

        try (Scope scope = span.makeCurrent()) {
            // Your business logic
            validateOrder(orderId);
            span.addEvent("order_validated");

            saveOrder(orderId);
            span.addEvent("order_saved");

            span.setAttribute("order.status", "completed");
        } catch (Exception e) {
            span.recordException(e);
            span.setStatus(StatusCode.ERROR, "Order processing failed");
            throw e;
        } finally {
            span.end();
        }
    }
}
```

**When to use manual instrumentation:**
- Complex business logic workflows
- Custom metrics and events
- Specific attributes for debugging

**When NOT needed:**
- HTTP requests (auto-instrumented)
- Database queries (auto-instrumented)
- Kafka messages (auto-instrumented)

---

## Erlang Services Implementation

### Step 1: Add Dependencies

#### Update `rebar.config`

```erlang
% Add OpenTelemetry dependencies
{deps, [
    % Core dependencies
    {opentelemetry_api, "~> 1.8"},
    {opentelemetry_semantic_conventions, "~> 1.27"},  % Optional but recommended

    % Your existing dependencies
    {cowboy, "~> 2.9"},
    {jsx, "~> 3.1"},
    % ...
]}.

{relx, [
    {release, {butler_server, "1.0.0"}, [
        % OpenTelemetry dependencies (ORDER MATTERS!)
        opentelemetry_exporter,      % Must be BEFORE opentelemetry
        {opentelemetry, temporary},  % Temporary mode (won't crash app if OTel fails)

        % Your application
        butler_server
    ]},

    {mode, prod},
    {include_erts, true}
]}.
```

**Important:** `opentelemetry_exporter` MUST come before `opentelemetry` in the release!

#### Update `.app.src`

```erlang
% src/butler_server.app.src
{application, butler_server, [
    {description, "GM Core with OpenTelemetry tracing"},
    {vsn, "1.0.0"},
    {registered, []},
    {mod, {butler_server_app, []}},
    {applications, [
        kernel,
        stdlib,
        opentelemetry_api,  % Add this
        cowboy,
        % ... your other dependencies
    ]},
    {env, []},
    {modules, []},
    {licenses, ["Apache-2.0"]},
    {links, []}
]}.
```

### Step 2: Configure OpenTelemetry

#### Create or Update `config/sys.config`

```erlang
[
    {opentelemetry, [
        %% Service identification
        {service_name, <<"butler_server">>},

        %% Resource attributes (metadata about your service)
        {resource, [
            {service_name, <<"butler_server">>},
            {service_version, <<"1.0.0">>},
            {service_namespace, <<"production">>},
            {deployment_environment, <<"prod">>},
            {'host.name', node()}
        ]},

        %% Exporter type
        {traces_exporter, otlp},

        %% Span processors
        {processors, [
            {otel_batch_processor, #{
                %% Exporter configuration
                exporter => {opentelemetry_exporter, #{
                    endpoints => [
                        {http, "localhost", 4318, []}
                    ]
                }},

                %% Batch settings
                scheduled_delay_ms => 5000,      %% Export every 5 seconds
                max_queue_size => 2048,          %% Max spans in queue
                exporting_timeout_ms => 30000    %% Timeout for export
            }}
        ]},

        %% Context propagators (for distributed tracing)
        {text_map_propagators, [
            trace_context,  %% W3C Trace Context (standard)
            baggage         %% W3C Baggage (for custom data)
        ]},

        %% Sampling (1.0 = 100% of traces)
        {sampler, {parent_based, #{
            root => {always_on},      %% Root spans always sampled (dev/staging)
            remote_parent_sampled => {always_on},
            remote_parent_not_sampled => {always_off},
            local_parent_sampled => {always_on},
            local_parent_not_sampled => {always_off}
        }}}
    ]},

    {opentelemetry_exporter, [
        %% Protocol: http_protobuf (default) or grpc
        {otlp_protocol, http_protobuf},

        %% Endpoint
        {otlp_endpoint, "http://localhost:4318"},

        %% Headers (e.g., for authentication)
        {otlp_headers, [
            %% Example: {"Authorization", "Bearer <token>"}
        ]},

        %% Compression (optional)
        {otlp_compression, gzip}
    ]}
].
```

#### Environment-Specific Configurations

**Development (config/dev.config):**

```erlang
[
    {opentelemetry, [
        {service_name, <<"butler_server_dev">>},
        {sampler, {parent_based, #{root => {always_on}}}}  % 100% sampling
    ]},
    {opentelemetry_exporter, [
        {otlp_endpoint, "http://localhost:4318"}
    ]}
].
```

**Production (config/prod.config):**

```erlang
[
    {opentelemetry, [
        {service_name, <<"butler_server">>},
        {sampler, {parent_based, #{
            root => {trace_id_ratio_based, 0.1}  % 10% sampling
        }}}
    ]},
    {opentelemetry_exporter, [
        {otlp_endpoint, "http://signoz-collector.prod.internal:4318"}
    ]}
].
```

### Step 3: Add Tracing to Code

#### Add Include Directive

```erlang
% At the top of your module
-module(order_handler).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").
```

#### Instrument HTTP Handlers

**Cowboy Handler Example:**

```erlang
-module(order_handler).
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

init(Req, State) ->
    %% Extract trace context from HTTP headers
    Headers = cowboy_req:headers(Req),
    HeadersList = maps:to_list(Headers),
    ParentCtx = otel_propagator_text_map:extract(HeadersList),
    otel_ctx:attach(ParentCtx),

    %% Create span for this HTTP request
    ?with_span(<<"POST /orders">>, #{
        kind => ?SPAN_KIND_SERVER,
        attributes => #{
            <<"http.method">> => cowboy_req:method(Req),
            <<"http.url">> => cowboy_req:path(Req),
            <<"http.route">> => <<"/orders">>
        }
    }, fun() ->
        %% Process request
        handle_request(Req, State)
    end).

handle_request(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    OrderData = jsx:decode(Body, [return_maps]),

    %% Add span attributes
    OrderId = maps:get(<<"order_id">>, OrderData),
    ?set_attribute(<<"order.id">>, OrderId),

    %% Process order (creates child span)
    Result = process_order(OrderId, OrderData),

    %% Return response
    Response = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(Result), Req2),

    {ok, Response, State}.
```

#### Instrument Business Logic

```erlang
process_order(OrderId, OrderData) ->
    ?with_span(<<"process_order">>, #{
        kind => ?SPAN_KIND_INTERNAL,
        attributes => #{
            <<"order.id">> => OrderId,
            <<"order.items_count">> => length(maps:get(<<"items">>, OrderData))
        }
    }, fun() ->
        ?add_event(<<"order.processing_started">>, #{}),

        %% Validate order
        case validate_order(OrderData) of
            {ok, ValidOrder} ->
                ?add_event(<<"order.validated">>, #{}),

                %% Save to database
                ok = save_to_mnesia(OrderId, ValidOrder),
                ?set_attribute(<<"order.saved">>, true),

                %% Publish to Kafka
                ok = publish_to_kafka(OrderId, ValidOrder),
                ?set_attribute(<<"order.published">>, true),

                ?set_status(?OTEL_STATUS_OK, <<"Order processed successfully">>),
                #{status => success, order_id => OrderId};

            {error, Reason} ->
                ?set_status(?OTEL_STATUS_ERROR, <<"Validation failed">>),
                ?set_attribute(<<"error.type">>, atom_to_binary(Reason)),
                ?add_event(<<"order.validation_failed">>, #{
                    <<"reason">> => atom_to_binary(Reason)
                }),
                #{status => error, reason => Reason}
        end
    end).
```

#### Instrument Database Operations

```erlang
save_to_mnesia(OrderId, OrderData) ->
    ?with_span(<<"db.mnesia.write">>, #{
        kind => ?SPAN_KIND_CLIENT,
        attributes => #{
            <<"db.system">> => <<"mnesia">>,
            <<"db.operation">> => <<"write">>,
            <<"db.table">> => <<"orders">>,
            <<"order.id">> => OrderId
        }
    }, fun() ->
        Order = #order{
            id = OrderId,
            data = OrderData,
            created_at = erlang:system_time(millisecond)
        },

        case mnesia:transaction(fun() -> mnesia:write(Order) end) of
            {atomic, ok} ->
                ?set_status(?OTEL_STATUS_OK, <<"Order saved">>),
                ok;
            {aborted, Reason} ->
                ?set_status(?OTEL_STATUS_ERROR, <<"Failed to save order">>),
                ?set_attribute(<<"error">>, atom_to_binary(Reason)),
                {error, Reason}
        end
    end).
```

#### Instrument Kafka Operations

**Producer (Publishing):**

```erlang
publish_to_kafka(OrderId, OrderData) ->
    ?with_span(<<"kafka.publish">>, #{
        kind => ?SPAN_KIND_PRODUCER,
        attributes => #{
            <<"messaging.system">> => <<"kafka">>,
            <<"messaging.destination">> => <<"order_events">>,
            <<"messaging.message_id">> => OrderId
        }
    }, fun() ->
        %% Get current context and inject into Kafka headers
        Ctx = otel_ctx:get_current(),
        TraceHeaders = otel_propagator_text_map:inject(Ctx),

        %% Convert to Kafka headers format
        KafkaHeaders = lists:map(fun({K, V}) ->
            {erlang:list_to_binary(K), V}
        end, TraceHeaders),

        %% Publish message with trace context
        Message = #{
            key => OrderId,
            value => jsx:encode(OrderData),
            headers => KafkaHeaders  %% IMPORTANT: Include trace headers!
        },

        case brod:produce_sync(kafka_client, <<"order_events">>, 0, <<>>, [Message]) of
            ok ->
                ?set_status(?OTEL_STATUS_OK, <<"Message published">>),
                ok;
            {error, Reason} ->
                ?set_status(?OTEL_STATUS_ERROR, <<"Failed to publish">>),
                {error, Reason}
        end
    end).
```

**Consumer (Receiving):**

```erlang
handle_kafka_message(Topic, Partition, Message) ->
    %% Extract trace context from Kafka headers
    Headers = maps:get(headers, Message, []),
    HeadersList = lists:map(fun({K, V}) ->
        {binary_to_list(K), V}
    end, Headers),

    ParentCtx = otel_propagator_text_map:extract(HeadersList),
    otel_ctx:attach(ParentCtx),

    %% Create span for message consumption
    ?with_span(<<"kafka.consume">>, #{
        kind => ?SPAN_KIND_CONSUMER,
        attributes => #{
            <<"messaging.system">> => <<"kafka">>,
            <<"messaging.source">> => Topic,
            <<"messaging.kafka.partition">> => Partition
        }
    }, fun() ->
        %% Process message
        Value = maps:get(value, Message),
        Data = jsx:decode(Value, [return_maps]),
        process_event(Data)
    end).
```

### Step 4: Handle Async Processes

#### Helper Module for Context Propagation

```erlang
-module(otel_helpers).
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

%% Spawn with trace context
spawn_with_context(Fun) ->
    Ctx = otel_ctx:get_current(),
    spawn(fun() ->
        otel_ctx:attach(Ctx),
        Fun()
    end).

%% gen_server:cast with context
cast_with_context(Server, Message) ->
    Ctx = otel_ctx:get_current(),
    gen_server:cast(Server, {with_context, Message, Ctx}).

%% gen_server:call with context
call_with_context(Server, Message) ->
    Ctx = otel_ctx:get_current(),
    gen_server:call(Server, {with_context, Message, Ctx}).
```

#### gen_server with Tracing

```erlang
-module(order_manager).
-behaviour(gen_server).
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

%% Client API
process_order_async(OrderId) ->
    ?with_span(<<"order_manager.request">>, #{}, fun() ->
        Ctx = otel_ctx:get_current(),
        gen_server:cast(?MODULE, {process_order, OrderId, Ctx})
    end).

%% Server callbacks
handle_cast({process_order, OrderId, ParentCtx}, State) ->
    %% Attach parent context
    otel_ctx:attach(ParentCtx),

    %% Create span in this process
    ?with_span(<<"order_manager.process">>, #{
        attributes => #{<<"order.id">> => OrderId}
    }, fun() ->
        %% Process order
        Result = do_work(OrderId),
        ?set_attribute(<<"result">>, Result),
        Result
    end),

    {noreply, State}.
```

### Step 5: Build and Deploy

```bash
# Fetch dependencies
rebar3 get-deps

# Compile
rebar3 compile

# Build release
rebar3 release

# OR build production release with specific config
rebar3 as prod release

# Start release
_build/default/rel/butler_server/bin/butler_server start

# Check logs
tail -f _build/default/rel/butler_server/log/erlang.log.1

# Expected: OpenTelemetry SDK initialization messages
```

### Step 6: Verify Erlang Service

```bash
# Check if OTel apps are running
_build/default/rel/butler_server/bin/butler_server remote_console
> application:which_applications().
% Should see: opentelemetry, opentelemetry_api, opentelemetry_exporter

# Make a test request
curl -X POST http://localhost:8181/api/orders \
  -H "Content-Type: application/json" \
  -d '{"order_id": "123", "items": [{"sku": "ABC"}]}'

# Check SigNoz UI
# Navigate to Services → Should see "butler_server"
# Navigate to Traces → Filter by service
```

---

## Refactoring Requirements

### Java Services: Summary

| Refactoring Type  | Effort        | Description                                |
| ----------------- | ------------- | ------------------------------------------ |
| **Agent Setup**   | 1 hour        | Download agent, configure startup          |
| **Configuration** | 30 min        | Environment variables or command-line args |
| **Code Changes**  | **0 hours**   | **Zero code changes required!**            |
| **Testing**       | 2 hours       | Validate traces, performance testing       |
| **Total**         | **3-4 hours** | Per service                                |

### Erlang Services: Summary

| Refactoring Type    | Effort          | Description                        |
| ------------------- | --------------- | ---------------------------------- |
| **Dependencies**    | 30 min          | Add to rebar.config, .app.src      |
| **Configuration**   | 1 hour          | sys.config setup                   |
| **HTTP Handlers**   | 2-3 hours       | Add `?with_span` to controllers    |
| **Business Logic**  | 3-5 hours       | Instrument critical paths          |
| **Async Processes** | 2-4 hours       | Context propagation helpers        |
| **Testing**         | 2-4 hours       | Validate traces, integration tests |
| **Total**           | **10-17 hours** | Per service                        |

### Refactoring Checklist

#### Java Service Checklist

- [ ] Download OpenTelemetry Java agent
- [ ] Add agent to service startup (systemd/Docker/etc.)
- [ ] Configure service name (`OTEL_SERVICE_NAME`)
- [ ] Configure OTLP endpoint (`OTEL_EXPORTER_OTLP_ENDPOINT`)
- [ ] Set sampling rate (`OTEL_TRACES_SAMPLER`)
- [ ] (Optional) Disable unwanted instrumentations
- [ ] Test service startup
- [ ] Verify traces in SigNoz
- [ ] Performance baseline comparison
- [ ] Update deployment documentation

#### Erlang Service Checklist

- [ ] Add `opentelemetry_api` to dependencies
- [ ] Add `opentelemetry` and `opentelemetry_exporter` to release
- [ ] Add `opentelemetry_api` to `.app.src` applications
- [ ] Create/update `sys.config` with OTel configuration
- [ ] Add `-include_lib("opentelemetry_api/include/otel_tracer.hrl")` to modules
- [ ] Instrument HTTP handlers (extract context, create spans)
- [ ] Instrument business logic (critical paths)
- [ ] Instrument database operations
- [ ] Instrument Kafka producers/consumers
- [ ] Create context propagation helpers for async processes
- [ ] Update gen_server callbacks to accept context
- [ ] Test service startup
- [ ] Verify traces in SigNoz
- [ ] Performance baseline comparison
- [ ] Update deployment documentation

---

## Developer Guidelines

### When to Add Tracing

#### ✅ Always Trace

**Critical paths:**
- HTTP request handlers (entry points)
- External API calls (HTTP clients)
- Database operations (queries, transactions)
- Message queue operations (Kafka produce/consume)
- Cache operations (if performance-critical)

**Business logic:**
- Order processing workflows
- Payment transactions
- Authentication/authorization
- Data transformations
- Background jobs (important ones)

#### ⚠️ Consider Tracing

- Complex algorithms (if performance matters)
- Batch operations
- File I/O operations
- Third-party integrations

#### ❌ Don't Trace

- Simple getters/setters
- Utility functions (string manipulation, etc.)
- Internal loops (unless performance debugging)
- Very frequent operations (>1000/sec per service)

### Span Naming Conventions

**Good span names:**
```
✅ "POST /api/orders"
✅ "db.query.find_order"
✅ "kafka.publish.order_events"
✅ "process_payment"
✅ "validate_customer"
```

**Bad span names:**
```
❌ "function_123"
❌ "do_something"
❌ "handle"
❌ "loop"
❌ "temp"
```

**Naming pattern:**
```
<component>.<action>.<resource>

Examples:
- http.request.get_user
- db.query.orders
- kafka.consume.events
- cache.get.session
```

### Attribute Guidelines

#### Use Semantic Conventions

**HTTP attributes:**
```erlang
?set_attributes(#{
    <<"http.method">> => <<"POST">>,
    <<"http.url">> => <<"https://api.example.com/orders">>,
    <<"http.status_code">> => 200,
    <<"http.route">> => <<"/orders">>
}).
```

**Database attributes:**
```erlang
?set_attributes(#{
    <<"db.system">> => <<"postgresql">>,
    <<"db.operation">> => <<"SELECT">>,
    <<"db.table">> => <<"orders">>,
    <<"db.statement">> => <<"SELECT * FROM orders WHERE id = ?">>  % Sanitize!
}).
```

**Messaging attributes:**
```erlang
?set_attributes(#{
    <<"messaging.system">> => <<"kafka">>,
    <<"messaging.destination">> => <<"order_events">>,
    <<"messaging.operation">> => <<"publish">>,
    <<"messaging.message_id">> => MessageId
}).
```

#### Custom Attributes

**Business-specific:**
```erlang
?set_attributes(#{
    <<"order.id">> => OrderId,
    <<"order.total">> => 99.99,
    <<"order.items_count">> => 3,
    <<"customer.id">> => CustomerId,
    <<"payment.method">> => <<"credit_card">>
}).
```

#### What NOT to Log

**Never include sensitive data:**
```erlang
% ❌ BAD - Contains sensitive data
?set_attributes(#{
    <<"password">> => Password,  % Never!
    <<"credit_card_number">> => CCNumber,  % Never!
    <<"api_key">> => ApiKey,  % Never!
    <<"ssn">> => SSN  % Never!
}).

% ✅ GOOD - Safe identifiers only
?set_attributes(#{
    <<"user.id">> => UserId,  % Safe
    <<"order.id">> => OrderId,  % Safe
    <<"payment.method">> => <<"credit_card">>,  % Safe (type only)
    <<"transaction.id">> => TxnId  % Safe
}).
```

### Error Handling

**Always set status on errors:**

```erlang
process_payment(PaymentData) ->
    ?with_span(<<"process_payment">>, #{}, fun() ->
        try
            Result = payment_gateway:charge(PaymentData),
            ?set_status(?OTEL_STATUS_OK, <<"Payment successful">>),
            ?set_attribute(<<"payment.result">>, <<"success">>),
            {ok, Result}
        catch
            error:insufficient_funds ->
                ?set_status(?OTEL_STATUS_ERROR, <<"Insufficient funds">>),
                ?set_attribute(<<"payment.error">>, <<"insufficient_funds">>),
                ?add_event(<<"payment.failed">>, #{
                    <<"reason">> => <<"insufficient_funds">>
                }),
                {error, insufficient_funds};

            Class:Reason:Stacktrace ->
                ?set_status(?OTEL_STATUS_ERROR, <<"Payment failed">>),
                ?set_attribute(<<"payment.error">>, atom_to_binary(Reason)),
                ?add_event(<<"payment.exception">>, #{
                    <<"exception.type">> => atom_to_binary(Class),
                    <<"exception.message">> => iolist_to_binary(io_lib:format("~p", [Reason]))
                }),
                erlang:raise(Class, Reason, Stacktrace)
        end
    end).
```

### Performance Best Practices

#### Minimize Span Creation

```erlang
% ❌ BAD - Too many spans
lists:foreach(fun(Item) ->
    ?with_span(<<"process_item">>, #{}, fun() ->
        process(Item)  % Creates span for EACH item
    end)
end, Items).

% ✅ GOOD - Single span for batch
?with_span(<<"process_items">>, #{
    attributes => #{<<"items.count">> => length(Items)}
}, fun() ->
    lists:foreach(fun(Item) -> process(Item) end, Items)
end).
```

#### Batch Operations

```erlang
% Create one span for the entire batch
?with_span(<<"db.batch_insert">>, #{
    attributes => #{
        <<"db.operation">> => <<"INSERT">>,
        <<"db.table">> => <<"orders">>,
        <<"db.batch_size">> => length(Orders)
    }
}, fun() ->
    Results = [save_order(Order) || Order <- Orders],
    ?set_attribute(<<"db.rows_affected">>, length(Results)),
    Results
end).
```

---

## Testing & Validation

### Unit Testing

**Test that spans are created:**

```erlang
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

process_order_test() ->
    %% Start OTel (test mode)
    ok = application:start(opentelemetry_api),

    %% Call function
    Result = process_order(<<"test-order-123">>, #{}),

    %% Assert result
    ?assertEqual({ok, success}, Result),

    %% Note: Validating exact spans is complex
    %% Focus on functional correctness

    ok = application:stop(opentelemetry_api).
-endif.
```

### Integration Testing

**Test end-to-end trace propagation:**

```bash
#!/bin/bash
# test_tracing.sh

# Generate unique trace ID for testing
TRACE_ID=$(openssl rand -hex 16)
SPAN_ID=$(openssl rand -hex 8)
TRACEPARENT="00-${TRACE_ID}-${SPAN_ID}-01"

echo "Testing with trace_id: ${TRACE_ID}"

# Make request with trace context
curl -X POST http://localhost:8080/api-gateway/gm_core/api/orders \
  -H "Content-Type: application/json" \
  -H "traceparent: ${TRACEPARENT}" \
  -d '{"order_id": "test-123"}'

echo ""
echo "Check SigNoz UI for trace_id: ${TRACE_ID}"
echo "Expected: trace should span Gateway → Auth → Butler"
```

### Performance Testing

**Baseline comparison:**

```bash
#!/bin/bash
# performance_test.sh

# Test WITHOUT tracing
echo "Testing WITHOUT tracing..."
ab -n 10000 -c 100 http://localhost:8080/api/health

# Enable tracing
export OTEL_TRACES_SAMPLER=always_on

# Test WITH tracing (100% sampling - worst case)
echo "Testing WITH tracing (100%)..."
ab -n 10000 -c 100 http://localhost:8080/api/health

# Test WITH tracing (10% sampling)
export OTEL_TRACES_SAMPLER=traceidratio
export OTEL_TRACES_SAMPLER_ARG=0.1
echo "Testing WITH tracing (10%)..."
ab -n 10000 -c 100 http://localhost:8080/api/health

# Compare results
```

### Validation Checklist

- [ ] Traces appear in SigNoz UI
- [ ] Trace ID remains constant across all services
- [ ] Parent-child span relationships are correct
- [ ] Span attributes are present and accurate
- [ ] HTTP status codes are captured
- [ ] Error spans show error status
- [ ] Performance impact < 2% (with 10% sampling)
- [ ] No memory leaks (monitor over 24 hours)
- [ ] Trace context propagates through Kafka

---

## Common Patterns & Examples

### Pattern 1: HTTP Request Handler (Erlang)

```erlang
-module(api_handler).
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

init(Req, State) ->
    %% Extract context from headers
    Headers = cowboy_req:headers(Req),
    ParentCtx = otel_propagator_text_map:extract(maps:to_list(Headers)),
    otel_ctx:attach(ParentCtx),

    %% Create server span
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),

    ?with_span(<<Method/binary, " ", Path/binary>>, #{
        kind => ?SPAN_KIND_SERVER,
        attributes => #{
            <<"http.method">> => Method,
            <<"http.url">> => cowboy_req:uri(Req),
            <<"http.route">> => Path
        }
    }, fun() ->
        handle_request(Req, State)
    end).

handle_request(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),

    try
        Data = jsx:decode(Body, [return_maps]),
        Result = process_request(Data),

        Response = cowboy_req:reply(200, #{
            <<"content-type">> => <<"application/json">>
        }, jsx:encode(Result), Req2),

        ?set_attribute(<<"http.status_code">>, 200),
        ?set_status(?OTEL_STATUS_OK, <<"Success">>),

        {ok, Response, State}
    catch
        error:Reason ->
            ?set_status(?OTEL_STATUS_ERROR, <<"Request failed">>),
            ?set_attribute(<<"error">>, atom_to_binary(Reason)),

            ErrorResponse = cowboy_req:reply(500, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"Internal error">>}), Req2),

            {ok, ErrorResponse, State}
    end.
```

### Pattern 2: External HTTP Call (Erlang)

```erlang
call_external_api(Url, Data) ->
    ?with_span(<<"http.client.call">>, #{
        kind => ?SPAN_KIND_CLIENT,
        attributes => #{
            <<"http.method">> => <<"POST">>,
            <<"http.url">> => Url,
            <<"peer.service">> => <<"external-api">>
        }
    }, fun() ->
        %% Inject trace context into headers
        Ctx = otel_ctx:get_current(),
        TraceHeaders = otel_propagator_text_map:inject(Ctx),

        Headers = [
            {"content-type", "application/json"}
            | TraceHeaders  % Add trace headers
        ],

        Body = jsx:encode(Data),

        case httpc:request(post, {Url, Headers, "application/json", Body}, [], []) of
            {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
                ?set_attribute(<<"http.status_code">>, StatusCode),

                if
                    StatusCode >= 200 andalso StatusCode < 300 ->
                        ?set_status(?OTEL_STATUS_OK, <<"Success">>),
                        {ok, jsx:decode(ResponseBody)};
                    true ->
                        ?set_status(?OTEL_STATUS_ERROR, <<"HTTP error">>),
                        {error, StatusCode}
                end;

            {error, Reason} ->
                ?set_status(?OTEL_STATUS_ERROR, <<"Request failed">>),
                ?set_attribute(<<"error">>, atom_to_binary(Reason)),
                {error, Reason}
        end
    end).
```

### Pattern 3: Database Query (Erlang)

```erlang
find_order_by_id(OrderId) ->
    ?with_span(<<"db.query.find_order">>, #{
        kind => ?SPAN_KIND_CLIENT,
        attributes => #{
            <<"db.system">> => <<"mnesia">>,
            <<"db.operation">> => <<"SELECT">>,
            <<"db.table">> => <<"orders">>,
            <<"order.id">> => OrderId
        }
    }, fun() ->
        StartTime = erlang:monotonic_time(millisecond),

        Result = mnesia:transaction(fun() ->
            mnesia:read(orders, OrderId)
        end),

        Duration = erlang:monotonic_time(millisecond) - StartTime,
        ?set_attribute(<<"db.duration_ms">>, Duration),

        case Result of
            {atomic, [Order]} ->
                ?set_status(?OTEL_STATUS_OK, <<"Order found">>),
                ?set_attribute(<<"db.rows_found">>, 1),
                {ok, Order};
            {atomic, []} ->
                ?set_status(?OTEL_STATUS_ERROR, <<"Order not found">>),
                ?set_attribute(<<"db.rows_found">>, 0),
                {error, not_found};
            {aborted, Reason} ->
                ?set_status(?OTEL_STATUS_ERROR, <<"Query failed">>),
                ?set_attribute(<<"error">>, atom_to_binary(Reason)),
                {error, Reason}
        end
    end).
```

### Pattern 4: Background Job with New Trace (Erlang)

```erlang
%% Scheduled cleanup job - starts its own trace
run_cleanup_job() ->
    %% Don't use parent context - this is a new root trace
    ?with_span(<<"background.cleanup_old_orders">>, #{
        kind => ?SPAN_KIND_INTERNAL,
        attributes => #{
            <<"job.type">> => <<"scheduled">>,
            <<"job.name">> => <<"cleanup">>
        }
    }, fun() ->
        ?add_event(<<"job.started">>, #{}),

        %% Find old orders
        OldOrders = find_orders_older_than(days(30)),
        ?set_attribute(<<"orders.found">>, length(OldOrders)),

        %% Delete them
        Deleted = delete_orders(OldOrders),
        ?set_attribute(<<"orders.deleted">>, Deleted),

        ?add_event(<<"job.completed">>, #{
            <<"orders.deleted">> => Deleted
        }),

        {ok, Deleted}
    end).
```

---

## Troubleshooting

### Issue: Traces Not Appearing in SigNoz

**Symptoms:**
- Service starts successfully
- No traces in SigNoz UI

**Debugging steps:**

```bash
# 1. Check SigNoz is reachable
curl http://localhost:4318/v1/traces
# Expected: 405 Method Not Allowed (means endpoint is up)

# 2. Check service configuration
# Java:
echo $OTEL_EXPORTER_OTLP_ENDPOINT
# Erlang:
grep otlp_endpoint config/sys.config

# 3. Enable debug logging
# Java:
-Dotel.javaagent.debug=true

# Erlang: Add to sys.config
{opentelemetry, [
    {logging, #{level => debug}}
]}.

# 4. Check for export errors in logs
# Java:
grep -i "error" /var/log/service.log | grep -i otel

# Erlang:
grep -i "export" _build/default/rel/service/log/erlang.log.*
```

### Issue: Trace Context Not Propagating

**Symptoms:**
- Multiple disconnected traces instead of one complete trace
- Missing parent-child relationships

**Check:**

```erlang
% Erlang: Ensure you're extracting AND attaching context

% ❌ BAD - Only extracting, not attaching
Headers = cowboy_req:headers(Req),
ParentCtx = otel_propagator_text_map:extract(maps:to_list(Headers)),
% Missing: otel_ctx:attach(ParentCtx)

% ✅ GOOD
Headers = cowboy_req:headers(Req),
ParentCtx = otel_propagator_text_map:extract(maps:to_list(Headers)),
otel_ctx:attach(ParentCtx),  % Must attach!
```

```bash
# Java: Ensure propagators are configured
-Dotel.propagators=tracecontext,baggage
```

### Issue: High Memory Usage

**Symptoms:**
- Memory grows continuously
- OOM errors

**Solutions:**

```erlang
% Erlang: Check batch processor settings
{processors, [
    {otel_batch_processor, #{
        scheduled_delay_ms => 5000,      % Export frequently
        max_queue_size => 2048,          % Limit queue size
        exporting_timeout_ms => 30000
    }}
]}.
```

```bash
# Java: Increase heap size
-Xmx2G  # Add 100-200MB for OTel agent
```

### Issue: Performance Degradation

**Symptoms:**
- Increased latency
- High CPU usage

**Solutions:**

1. **Reduce sampling:**

```bash
# From 100% to 10%
export OTEL_TRACES_SAMPLER=traceidratio
export OTEL_TRACES_SAMPLER_ARG=0.1
```

2. **Disable specific instrumentations:**

```bash
# Java: Disable database tracing
export OTEL_INSTRUMENTATION_JDBC_ENABLED=false
export OTEL_INSTRUMENTATION_SPRING_DATA_ENABLED=false
```

3. **Temporarily disable tracing:**

```bash
# Java:
export OTEL_TRACES_SAMPLER=always_off

# Erlang:
{sampler, {parent_based, #{root => {always_off}}}}
```

---

## Summary

### Quick Reference

| Task                   | Java        | Erlang            |
| ---------------------- | ----------- | ----------------- |
| **Setup Time**         | 1 hour      | 2-3 hours         |
| **Code Changes**       | None (auto) | Moderate (macros) |
| **Refactoring Effort** | Low         | Medium            |
| **Maintenance**        | Minimal     | Low               |

### Key Takeaways

**Java:**
- ✅ Zero code changes with Java agent
- ✅ 100+ libraries auto-instrumented
- ✅ Quick to implement and deploy
- ✅ Minimal performance overhead

**Erlang:**
- ⚠️ Manual instrumentation required
- ⚠️ Context propagation needs attention
- ✅ Flexible and powerful
- ✅ Minimal performance overhead

### Next Steps

1. **Review this guide** with your development team
2. **Start with one service** (pilot implementation)
3. **Validate in development** environment
4. **Roll out gradually** to production
5. **Refer to operational guide** for monitoring

---

**Document Status:** ✅ Ready for Development
**Last Updated:** November 2025
**Maintainer:** Platform Engineering Team

