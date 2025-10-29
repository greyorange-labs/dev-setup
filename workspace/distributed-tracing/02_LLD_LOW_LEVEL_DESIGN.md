# Low-Level Design: Distributed Tracing Implementation

## Table of Contents
1. [Introduction](#introduction)
2. [Language-Specific Implementation](#language-specific-implementation)
3. [Infrastructure Components Configuration](#infrastructure-components-configuration)
4. [Database & Message Queue Instrumentation](#database--message-queue-instrumentation)
5. [Custom Span Creation](#custom-span-creation)
6. [Error Handling & Logging](#error-handling--logging)
7. [Performance Optimization](#performance-optimization)
8. [Testing Strategy](#testing-strategy)
9. [Code Examples](#code-examples)

---

## 1. Introduction

### 1.1 Purpose
This document provides detailed implementation specifications for integrating distributed tracing across all services in our ecosystem.

### 1.2 Technology Stack Summary

| Language | Framework | OTel SDK Version | Auto-Instrumentation | Manual Required |
|----------|-----------|------------------|---------------------|-----------------|
| Java 21 | Spring Boot 3.x | 1.32+ | Yes | Minimal |
| Python 3 | FastAPI/Django | 1.21+ | Yes | Some |
| Erlang 27 | OTP/Cowboy | 1.3+ | Partial | More |
| Node.js | Express | 1.18+ | Yes | Minimal |

---

## 2. Language-Specific Implementation

### 2.1 Java Services (Spring Boot)

#### 2.1.1 Dependencies (Maven)

```xml
<!-- pom.xml -->
<properties>
    <opentelemetry.version>1.32.0</opentelemetry.version>
    <opentelemetry-alpha.version>1.32.0-alpha</opentelemetry-alpha.version>
</properties>

<dependencies>
    <!-- OpenTelemetry API -->
    <dependency>
        <groupId>io.opentelemetry</groupId>
        <artifactId>opentelemetry-api</artifactId>
        <version>${opentelemetry.version}</version>
    </dependency>

    <!-- OpenTelemetry SDK -->
    <dependency>
        <groupId>io.opentelemetry</groupId>
        <artifactId>opentelemetry-sdk</artifactId>
        <version>${opentelemetry.version}</version>
    </dependency>

    <!-- OTLP Exporter -->
    <dependency>
        <groupId>io.opentelemetry</groupId>
        <artifactId>opentelemetry-exporter-otlp</artifactId>
        <version>${opentelemetry.version}</version>
    </dependency>

    <!-- Spring Boot Auto-configuration -->
    <dependency>
        <groupId>io.opentelemetry.instrumentation</groupId>
        <artifactId>opentelemetry-spring-boot-starter</artifactId>
        <version>${opentelemetry-alpha.version}</version>
    </dependency>
</dependencies>
```

#### 2.1.2 Application Configuration

```yaml
# application.yml
spring:
  application:
    name: order-service

otel:
  # Service identification
  service:
    name: ${spring.application.name}
    version: ${application.version:1.0.0}

  # Resource attributes
  resource:
    attributes:
      deployment.environment: ${ENVIRONMENT:production}
      service.namespace: microservices
      service.instance.id: ${HOSTNAME}

  # Exporter configuration
  exporter:
    otlp:
      endpoint: http://otel-collector:4317
      protocol: grpc
      compression: gzip
      timeout: 10s
      headers:
        authorization: Bearer ${OTEL_AUTH_TOKEN:}

  # Tracing configuration
  traces:
    sampler:
      # Use parent-based sampling with 100% rate
      # Tail-based sampling done at collector
      type: parentbased_always_on

  # Propagation format
  propagators:
    - tracecontext  # W3C Trace Context
    - baggage       # W3C Baggage

  # Instrumentation configuration
  instrumentation:
    spring-webmvc:
      enabled: true
    spring-webflux:
      enabled: true
    jdbc:
      enabled: true
      datasource:
        enabled: true
    kafka:
      enabled: true
    redis:
      enabled: true
    httpclient:
      enabled: true
    resttemplate:
      enabled: true

# Logging correlation
logging:
  pattern:
    level: "trace_id=%mdc{trace_id} span_id=%mdc{span_id} %p"
```

#### 2.1.3 Java Agent Usage (Recommended for Zero Code Change)

**Download Agent:**
```bash
wget https://github.com/open-telemetry/opentelemetry-java-instrumentation/releases/latest/download/opentelemetry-javaagent.jar
```

**JVM Arguments:**
```bash
java -javaagent:opentelemetry-javaagent.jar \
  -Dotel.service.name=order-service \
  -Dotel.resource.attributes=deployment.environment=production,service.version=1.0.0 \
  -Dotel.exporter.otlp.endpoint=http://otel-collector:4317 \
  -Dotel.exporter.otlp.protocol=grpc \
  -Dotel.traces.sampler=parentbased_always_on \
  -Dotel.instrumentation.common.default-enabled=true \
  -Dotel.instrumentation.jdbc.statement-sanitizer.enabled=true \
  -jar order-service.jar
```

**Dockerfile:**
```dockerfile
FROM eclipse-temurin:21-jre-alpine

# Add OpenTelemetry Java Agent
ADD https://github.com/open-telemetry/opentelemetry-java-instrumentation/releases/latest/download/opentelemetry-javaagent.jar /app/opentelemetry-javaagent.jar

# Copy application
COPY target/order-service.jar /app/order-service.jar

# Environment variables
ENV JAVA_OPTS="-javaagent:/app/opentelemetry-javaagent.jar"
ENV OTEL_SERVICE_NAME=order-service
ENV OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4317

ENTRYPOINT ["sh", "-c", "java $JAVA_OPTS -jar /app/order-service.jar"]
```

#### 2.1.4 Manual Instrumentation (Advanced Use Cases)

```java
package com.company.orderservice.service;

import io.opentelemetry.api.OpenTelemetry;
import io.opentelemetry.api.trace.Span;
import io.opentelemetry.api.trace.SpanKind;
import io.opentelemetry.api.trace.StatusCode;
import io.opentelemetry.api.trace.Tracer;
import io.opentelemetry.context.Context;
import io.opentelemetry.context.Scope;
import org.springframework.stereotype.Service;

@Service
public class OrderService {

    private final Tracer tracer;

    public OrderService(OpenTelemetry openTelemetry) {
        this.tracer = openTelemetry.getTracer("order-service", "1.0.0");
    }

    /**
     * Create order with manual span creation
     */
    public Order createOrder(OrderRequest request) {
        // Create a new span
        Span span = tracer.spanBuilder("createOrder")
            .setSpanKind(SpanKind.INTERNAL)
            .startSpan();

        // Make span current
        try (Scope scope = span.makeCurrent()) {

            // Add attributes (tags)
            span.setAttribute("order.id", request.getId());
            span.setAttribute("order.amount", request.getAmount());
            span.setAttribute("customer.id", request.getCustomerId());
            span.setAttribute("order.items.count", request.getItems().size());

            // Add event (annotation)
            span.addEvent("Order validation started");

            // Business logic
            validateOrder(request);

            span.addEvent("Order validation completed");

            // Call database (auto-instrumented)
            Order order = orderRepository.save(request.toOrder());

            // Call external service with manual child span
            checkInventory(order);

            // Send Kafka message (auto-instrumented)
            publishOrderCreatedEvent(order);

            // Set success status
            span.setStatus(StatusCode.OK);
            span.setAttribute("order.created", true);

            return order;

        } catch (Exception e) {
            // Record exception
            span.recordException(e);
            span.setStatus(StatusCode.ERROR, e.getMessage());
            throw e;
        } finally {
            // End span
            span.end();
        }
    }

    /**
     * Check inventory with manual child span
     */
    private void checkInventory(Order order) {
        Span span = tracer.spanBuilder("checkInventory")
            .setSpanKind(SpanKind.CLIENT)
            .startSpan();

        try (Scope scope = span.makeCurrent()) {
            span.setAttribute("http.method", "POST");
            span.setAttribute("http.url", "http://inventory-service/check");
            span.setAttribute("peer.service", "inventory-service");

            // HTTP call (auto-instrumented if using RestTemplate/WebClient)
            inventoryClient.check(order.getItems());

            span.setStatus(StatusCode.OK);
        } catch (Exception e) {
            span.recordException(e);
            span.setStatus(StatusCode.ERROR);
            throw e;
        } finally {
            span.end();
        }
    }

    /**
     * Extract current trace context for logging
     */
    public void logWithTraceContext(String message) {
        Span currentSpan = Span.current();
        String traceId = currentSpan.getSpanContext().getTraceId();
        String spanId = currentSpan.getSpanContext().getSpanId();

        log.info("TraceId: {}, SpanId: {}, Message: {}", traceId, spanId, message);
    }
}
```

#### 2.1.5 Spring Boot Configuration Class

```java
package com.company.orderservice.config;

import io.opentelemetry.api.OpenTelemetry;
import io.opentelemetry.api.common.Attributes;
import io.opentelemetry.api.trace.propagation.W3CTraceContextPropagator;
import io.opentelemetry.context.propagation.ContextPropagators;
import io.opentelemetry.exporter.otlp.trace.OtlpGrpcSpanExporter;
import io.opentelemetry.sdk.OpenTelemetrySdk;
import io.opentelemetry.sdk.resources.Resource;
import io.opentelemetry.sdk.trace.SdkTracerProvider;
import io.opentelemetry.sdk.trace.export.BatchSpanProcessor;
import io.opentelemetry.sdk.trace.samplers.Sampler;
import io.opentelemetry.semconv.resource.attributes.ResourceAttributes;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.time.Duration;

@Configuration
public class OpenTelemetryConfig {

    @Value("${otel.service.name}")
    private String serviceName;

    @Value("${otel.service.version:1.0.0}")
    private String serviceVersion;

    @Value("${otel.exporter.otlp.endpoint}")
    private String otlpEndpoint;

    @Value("${otel.resource.attributes.deployment.environment:production}")
    private String environment;

    @Bean
    public OpenTelemetry openTelemetry() {
        // Create resource with service information
        Resource resource = Resource.getDefault()
            .merge(Resource.create(Attributes.builder()
                .put(ResourceAttributes.SERVICE_NAME, serviceName)
                .put(ResourceAttributes.SERVICE_VERSION, serviceVersion)
                .put(ResourceAttributes.DEPLOYMENT_ENVIRONMENT, environment)
                .put(ResourceAttributes.SERVICE_NAMESPACE, "microservices")
                .put(ResourceAttributes.SERVICE_INSTANCE_ID,
                    System.getenv("HOSTNAME"))
                .build()));

        // Create OTLP exporter
        OtlpGrpcSpanExporter spanExporter = OtlpGrpcSpanExporter.builder()
            .setEndpoint(otlpEndpoint)
            .setTimeout(Duration.ofSeconds(10))
            .setCompression("gzip")
            .build();

        // Create batch span processor
        BatchSpanProcessor batchProcessor = BatchSpanProcessor.builder(spanExporter)
            .setMaxQueueSize(2048)
            .setMaxExportBatchSize(512)
            .setScheduleDelay(Duration.ofSeconds(5))
            .setExporterTimeout(Duration.ofSeconds(30))
            .build();

        // Create tracer provider
        SdkTracerProvider tracerProvider = SdkTracerProvider.builder()
            .setResource(resource)
            .addSpanProcessor(batchProcessor)
            .setSampler(Sampler.parentBased(Sampler.alwaysOn()))
            .build();

        // Create OpenTelemetry instance
        OpenTelemetrySdk openTelemetry = OpenTelemetrySdk.builder()
            .setTracerProvider(tracerProvider)
            .setPropagators(ContextPropagators.create(
                W3CTraceContextPropagator.getInstance()))
            .buildAndRegisterGlobal();

        // Add shutdown hook
        Runtime.getRuntime().addShutdownHook(new Thread(tracerProvider::close));

        return openTelemetry;
    }
}
```

---

### 2.2 Python Services (FastAPI/Django)

#### 2.2.1 Dependencies

```txt
# requirements.txt

# Core OpenTelemetry
opentelemetry-api==1.21.0
opentelemetry-sdk==1.21.0

# OTLP Exporter
opentelemetry-exporter-otlp-proto-grpc==1.21.0

# Auto-instrumentation
opentelemetry-instrumentation==0.42b0

# Framework-specific
opentelemetry-instrumentation-fastapi==0.42b0  # For FastAPI
opentelemetry-instrumentation-django==0.42b0   # For Django

# Database instrumentation
opentelemetry-instrumentation-psycopg2==0.42b0
opentelemetry-instrumentation-sqlalchemy==0.42b0

# HTTP client instrumentation
opentelemetry-instrumentation-requests==0.42b0
opentelemetry-instrumentation-urllib3==0.42b0
opentelemetry-instrumentation-httpx==0.42b0

# Message queue instrumentation
opentelemetry-instrumentation-kafka-python==0.42b0

# Redis instrumentation
opentelemetry-instrumentation-redis==0.42b0

# Logging integration
opentelemetry-instrumentation-logging==0.42b0
```

#### 2.2.2 FastAPI Application Setup

```python
# main.py
from fastapi import FastAPI, Request
from opentelemetry import trace
from opentelemetry.sdk.trace import TracerProvider
from opentelemetry.sdk.trace.export import BatchSpanProcessor
from opentelemetry.exporter.otlp.proto.grpc.trace_exporter import OTLPSpanExporter
from opentelemetry.sdk.resources import Resource, SERVICE_NAME, SERVICE_VERSION
from opentelemetry.instrumentation.fastapi import FastAPIInstrumentor
from opentelemetry.instrumentation.requests import RequestsInstrumentor
from opentelemetry.instrumentation.psycopg2 import Psycopg2Instrumentor
from opentelemetry.instrumentation.redis import RedisInstrumentor
from opentelemetry.trace.propagation.tracecontext import TraceContextTextMapPropagator
import os
import logging

# Configure logging with trace context
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s trace_id=%(otelTraceID)s span_id=%(otelSpanID)s %(levelname)s %(message)s'
)

def setup_telemetry():
    """
    Setup OpenTelemetry tracing
    """
    # Create resource with service information
    resource = Resource.create({
        SERVICE_NAME: os.getenv("OTEL_SERVICE_NAME", "inventory-service"),
        SERVICE_VERSION: os.getenv("SERVICE_VERSION", "1.0.0"),
        "deployment.environment": os.getenv("ENVIRONMENT", "production"),
        "service.namespace": "microservices",
        "service.instance.id": os.getenv("HOSTNAME", "unknown"),
    })

    # Create tracer provider
    tracer_provider = TracerProvider(resource=resource)

    # Create OTLP exporter
    otlp_exporter = OTLPSpanExporter(
        endpoint=os.getenv("OTEL_EXPORTER_OTLP_ENDPOINT", "http://otel-collector:4317"),
        insecure=True,  # Use TLS in production
        compression="gzip"
    )

    # Add batch span processor
    span_processor = BatchSpanProcessor(
        otlp_exporter,
        max_queue_size=2048,
        schedule_delay_millis=5000,
        max_export_batch_size=512,
        export_timeout_millis=30000
    )
    tracer_provider.add_span_processor(span_processor)

    # Set global tracer provider
    trace.set_tracer_provider(tracer_provider)

    return tracer_provider

# Initialize telemetry
tracer_provider = setup_telemetry()
tracer = trace.get_tracer(__name__)

# Create FastAPI app
app = FastAPI(title="Inventory Service")

# Auto-instrument FastAPI
FastAPIInstrumentor.instrument_app(app)

# Auto-instrument HTTP clients
RequestsInstrumentor().instrument()

# Auto-instrument database
Psycopg2Instrumentor().instrument()

# Auto-instrument Redis
RedisInstrumentor().instrument()

@app.middleware("http")
async def add_trace_context_to_logs(request: Request, call_next):
    """
    Middleware to add trace context to logging context
    """
    span = trace.get_current_span()
    if span:
        span_context = span.get_span_context()
        # Add to logging MDC
        logging.LoggerAdapter(logging.getLogger(), {
            'otelTraceID': format(span_context.trace_id, '032x'),
            'otelSpanID': format(span_context.span_id, '016x')
        })

    response = await call_next(request)
    return response

@app.get("/health")
async def health_check():
    """Health check endpoint (minimal tracing)"""
    return {"status": "healthy"}

@app.post("/inventory/check")
async def check_inventory(items: list[dict]):
    """
    Check inventory with custom spans
    """
    # Get current span (auto-created by FastAPI instrumentation)
    current_span = trace.get_current_span()
    current_span.set_attribute("inventory.items.count", len(items))

    # Create custom child span
    with tracer.start_as_current_span(
        "validate_items",
        kind=trace.SpanKind.INTERNAL
    ) as span:
        span.set_attribute("validation.type", "inventory")

        # Business logic
        validated_items = validate_items(items)

        span.set_attribute("validation.passed", len(validated_items))
        span.add_event("Items validated")

    # Database query (auto-instrumented)
    with tracer.start_as_current_span(
        "check_stock_levels",
        kind=trace.SpanKind.INTERNAL
    ) as span:
        stock_levels = await check_stock_in_db(validated_items)
        span.set_attribute("db.items_checked", len(stock_levels))

    return {"available": stock_levels}

def validate_items(items: list[dict]) -> list[dict]:
    """Validate items (instrumented)"""
    span = trace.get_current_span()

    try:
        # Validation logic
        validated = [item for item in items if item.get("id")]
        span.set_attribute("validation.success", True)
        return validated
    except Exception as e:
        span.record_exception(e)
        span.set_status(trace.Status(trace.StatusCode.ERROR, str(e)))
        raise

# Graceful shutdown
@app.on_event("shutdown")
def shutdown_event():
    """Flush remaining spans on shutdown"""
    tracer_provider.shutdown()
```

#### 2.2.3 Manual Span Creation (Python)

```python
# services/order_service.py
from opentelemetry import trace
from opentelemetry.trace import Status, StatusCode
import logging

logger = logging.getLogger(__name__)
tracer = trace.get_tracer(__name__)

class OrderService:

    def create_order(self, order_data: dict) -> dict:
        """
        Create order with detailed tracing
        """
        # Create parent span for the entire operation
        with tracer.start_as_current_span(
            "create_order",
            kind=trace.SpanKind.INTERNAL,
            attributes={
                "order.customer_id": order_data.get("customer_id"),
                "order.total_amount": order_data.get("total_amount"),
                "order.items_count": len(order_data.get("items", []))
            }
        ) as span:

            try:
                # Step 1: Validate order
                with tracer.start_as_current_span("validate_order") as validate_span:
                    validate_span.add_event("Starting validation")
                    self._validate_order(order_data)
                    validate_span.add_event("Validation completed")
                    validate_span.set_status(Status(StatusCode.OK))

                # Step 2: Check inventory
                with tracer.start_as_current_span(
                    "check_inventory",
                    kind=trace.SpanKind.CLIENT,
                    attributes={
                        "peer.service": "inventory-service",
                        "http.method": "POST"
                    }
                ) as inventory_span:
                    inventory_available = self._check_inventory(order_data["items"])
                    inventory_span.set_attribute("inventory.available", inventory_available)

                    if not inventory_available:
                        inventory_span.add_event("Insufficient inventory")
                        raise ValueError("Insufficient inventory")

                # Step 3: Create order in database (auto-instrumented)
                order = self._save_order(order_data)
                span.set_attribute("order.id", order["id"])

                # Step 4: Publish event
                with tracer.start_as_current_span(
                    "publish_order_created_event",
                    kind=trace.SpanKind.PRODUCER,
                    attributes={
                        "messaging.system": "kafka",
                        "messaging.destination": "order-events"
                    }
                ) as event_span:
                    self._publish_event(order)
                    event_span.add_event("Event published")

                # Success
                span.set_status(Status(StatusCode.OK))
                span.set_attribute("order.status", "created")
                logger.info(f"Order created successfully: {order['id']}")

                return order

            except ValueError as e:
                # Business error
                span.record_exception(e)
                span.set_status(Status(StatusCode.ERROR, str(e)))
                span.set_attribute("error.type", "validation_error")
                logger.error(f"Order validation failed: {e}")
                raise

            except Exception as e:
                # System error
                span.record_exception(e)
                span.set_status(Status(StatusCode.ERROR, str(e)))
                span.set_attribute("error.type", "system_error")
                logger.exception("Unexpected error creating order")
                raise

    def _check_inventory(self, items: list[dict]) -> bool:
        """Check inventory via HTTP call"""
        import requests

        # HTTP call is auto-instrumented by RequestsInstrumentor
        response = requests.post(
            "http://inventory-service/check",
            json={"items": items},
            timeout=5
        )
        response.raise_for_status()
        return response.json()["available"]
```

#### 2.2.4 Django Setup

```python
# settings.py (Django)
from opentelemetry import trace
from opentelemetry.sdk.trace import TracerProvider
from opentelemetry.sdk.trace.export import BatchSpanProcessor
from opentelemetry.exporter.otlp.proto.grpc.trace_exporter import OTLPSpanExporter
from opentelemetry.sdk.resources import Resource, SERVICE_NAME
from opentelemetry.instrumentation.django import DjangoInstrumentor
import os

# OpenTelemetry setup
resource = Resource.create({
    SERVICE_NAME: os.getenv("OTEL_SERVICE_NAME", "django-service"),
    "deployment.environment": os.getenv("ENVIRONMENT", "production"),
})

tracer_provider = TracerProvider(resource=resource)
otlp_exporter = OTLPSpanExporter(
    endpoint=os.getenv("OTEL_EXPORTER_OTLP_ENDPOINT", "http://otel-collector:4317"),
    insecure=True
)
tracer_provider.add_span_processor(BatchSpanProcessor(otlp_exporter))
trace.set_tracer_provider(tracer_provider)

# Auto-instrument Django
DjangoInstrumentor().instrument()
```

---

### 2.3 Erlang Services (OTP)

#### 2.3.1 Dependencies (rebar.config)

```erlang
%% rebar.config
{deps, [
    {opentelemetry_api, "~> 1.3"},
    {opentelemetry, "~> 1.3"},
    {opentelemetry_exporter, "~> 1.3"},
    {opentelemetry_cowboy, "~> 0.3"},  %% For HTTP instrumentation
    {opentelemetry_ecto, "~> 1.1"}     %% For database instrumentation (if using Ecto)
]}.

{shell, [
    {apps, [opentelemetry]}
]}.
```

#### 2.3.2 Application Configuration

```erlang
%% sys.config
[
    {opentelemetry, [
        {resource, #{
            'service.name' => <<"notification-service">>,
            'service.version' => <<"1.0.0">>,
            'deployment.environment' => <<"production">>,
            'service.namespace' => <<"microservices">>
        }},
        {traces_exporter, {opentelemetry_exporter, #{
            endpoints => [{http, "otel-collector", 4318, []}],  %% HTTP endpoint
            protocol => http_protobuf,
            compression => gzip
        }}},
        {span_processor, batch},
        {batch_span_processor, #{
            scheduled_delay_ms => 5000,
            max_queue_size => 2048,
            exporter_timeout_ms => 30000,
            max_export_batch_size => 512
        }},
        {sampler, {parent_based, #{
            root => always_on,
            remote_parent_sampled => always_on,
            remote_parent_not_sampled => always_on,
            local_parent_sampled => always_on,
            local_parent_not_sampled => always_on
        }}}
    ]}
].
```

#### 2.3.3 Application Startup

```erlang
%% src/notification_service_app.erl
-module(notification_service_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Initialize OpenTelemetry
    ok = application:ensure_started(opentelemetry),
    ok = application:ensure_started(opentelemetry_exporter),

    %% Set up HTTP server with tracing
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/notify", notification_handler, []},
            {"/health", health_handler, []}
        ]}
    ]),

    %% Start Cowboy with OpenTelemetry middleware
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, 8080}],
        #{
            env => #{dispatch => Dispatch},
            middlewares => [
                opentelemetry_cowboy,  %% Add tracing middleware
                cowboy_router,
                cowboy_handler
            ]
        }
    ),

    notification_service_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http_listener),
    ok.
```

#### 2.3.4 HTTP Handler with Tracing

```erlang
%% src/notification_handler.erl
-module(notification_handler).
-behaviour(cowboy_handler).

-export([init/2]).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").

init(Req0, State) ->
    %% Current span is automatically created by opentelemetry_cowboy
    ?set_attribute('http.method', cowboy_req:method(Req0)),
    ?set_attribute('http.target', cowboy_req:path(Req0)),

    %% Read request body
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Notification = jsone:decode(Body),

    %% Process notification with custom span
    Result = process_notification(Notification),

    %% Return response
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsone:encode(Result),
        Req1),

    {ok, Req, State}.

%% Process notification with manual span
process_notification(Notification) ->
    %% Create child span
    SpanCtx = ?start_span(<<"process_notification">>),
    ?set_attributes([
        {'notification.type', maps:get(<<"type">>, Notification)},
        {'notification.recipient', maps:get(<<"recipient">>, Notification)}
    ]),

    try
        %% Add event
        ?add_event(<<"Processing started">>, #{}),

        %% Validate notification
        ok = validate_notification(Notification),

        %% Send via EMQX (with propagation)
        ok = send_mqtt_message(Notification),

        %% Store in Mnesia
        ok = store_notification(Notification),

        ?add_event(<<"Processing completed">>, #{}),
        ?set_status(?OTEL_STATUS_OK),

        #{status => success}
    catch
        Type:Reason:Stacktrace ->
            %% Record exception
            ?record_exception(Type, Reason, Stacktrace),
            ?set_status(?OTEL_STATUS_ERROR, <<"Processing failed">>),
            #{status => error, reason => Reason}
    after
        %% End span
        ?end_span(SpanCtx)
    end.

%% Send MQTT message with trace context propagation
send_mqtt_message(Notification) ->
    SpanCtx = ?start_span(<<"send_mqtt_message">>),
    ?set_attributes([
        {'messaging.system', <<"mqtt">>},
        {'messaging.destination', <<"notifications">>},
        {'messaging.protocol', <<"mqtt">>}
    ]),

    try
        %% Get current trace context
        TraceContext = otel_tracer:current_span_ctx(),
        TraceParent = otel_propagator_trace_context:inject(TraceContext),

        %% Add trace context to MQTT message properties
        Message = #{
            topic => <<"notifications">>,
            payload => jsone:encode(Notification),
            properties => #{
                <<"traceparent">> => TraceParent
            }
        },

        %% Publish to EMQX
        ok = emqx_client:publish(Message),

        ?set_status(?OTEL_STATUS_OK),
        ok
    catch
        _:Error ->
            ?record_exception(error, Error, []),
            ?set_status(?OTEL_STATUS_ERROR),
            {error, Error}
    after
        ?end_span(SpanCtx)
    end.

%% Store in Mnesia with auto-instrumentation
store_notification(Notification) ->
    SpanCtx = ?start_span(<<"store_notification">>),
    ?set_attributes([
        {'db.system', <<"mnesia">>},
        {'db.operation', <<"write">>},
        {'db.name', <<"notifications">>}
    ]),

    try
        F = fun() ->
            mnesia:write(#notification{
                id = maps:get(<<"id">>, Notification),
                type = maps:get(<<"type">>, Notification),
                data = Notification,
                created_at = erlang:system_time(millisecond)
            })
        end,

        case mnesia:transaction(F) of
            {atomic, ok} ->
                ?set_status(?OTEL_STATUS_OK),
                ok;
            {aborted, Reason} ->
                ?set_status(?OTEL_STATUS_ERROR, Reason),
                {error, Reason}
        end
    after
        ?end_span(SpanCtx)
    end.
```

#### 2.3.5 GenServer with Tracing

```erlang
%% src/notification_worker.erl
-module(notification_worker).
-behaviour(gen_server).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-export([start_link/0, send_notification/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send_notification(Notification) ->
    gen_server:call(?MODULE, {send, Notification}).

init([]) ->
    {ok, #{}}.

handle_call({send, Notification}, _From, State) ->
    %% Create span for GenServer call
    SpanCtx = ?start_span(<<"notification_worker.send">>),
    ?set_attribute('worker.operation', 'send_notification'),

    try
        Result = do_send_notification(Notification),
        ?set_status(?OTEL_STATUS_OK),
        {reply, Result, State}
    catch
        Type:Reason:Stack ->
            ?record_exception(Type, Reason, Stack),
            ?set_status(?OTEL_STATUS_ERROR),
            {reply, {error, Reason}, State}
    after
        ?end_span(SpanCtx)
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
```

---

### 2.4 Node.js Services

#### 2.4.1 Dependencies (package.json)

```json
{
  "name": "analytics-service",
  "version": "1.0.0",
  "dependencies": {
    "express": "^4.18.2",
    "@opentelemetry/sdk-node": "^0.45.1",
    "@opentelemetry/api": "^1.7.0",
    "@opentelemetry/auto-instrumentations-node": "^0.40.0",
    "@opentelemetry/exporter-trace-otlp-grpc": "^0.45.1",
    "@opentelemetry/resources": "^1.19.0",
    "@opentelemetry/semantic-conventions": "^1.19.0",
    "@opentelemetry/instrumentation-express": "^0.35.0",
    "@opentelemetry/instrumentation-http": "^0.45.1",
    "@opentelemetry/instrumentation-pg": "^0.38.0",
    "@opentelemetry/instrumentation-redis-4": "^0.37.0"
  }
}
```

#### 2.4.2 Tracing Setup (tracing.js)

```javascript
// tracing.js
const { NodeSDK } = require('@opentelemetry/sdk-node');
const { getNodeAutoInstrumentations } = require('@opentelemetry/auto-instrumentations-node');
const { OTLPTraceExporter } = require('@opentelemetry/exporter-trace-otlp-grpc');
const { Resource } = require('@opentelemetry/resources');
const { SemanticResourceAttributes } = require('@opentelemetry/semantic-conventions');
const { BatchSpanProcessor } = require('@opentelemetry/sdk-trace-base');
const { diag, DiagConsoleLogger, DiagLogLevel } = require('@opentelemetry/api');

// Enable diagnostic logging (for debugging)
// diag.setLogger(new DiagConsoleLogger(), DiagLogLevel.DEBUG);

// Create resource
const resource = Resource.default().merge(
  new Resource({
    [SemanticResourceAttributes.SERVICE_NAME]: process.env.OTEL_SERVICE_NAME || 'analytics-service',
    [SemanticResourceAttributes.SERVICE_VERSION]: process.env.SERVICE_VERSION || '1.0.0',
    [SemanticResourceAttributes.DEPLOYMENT_ENVIRONMENT]: process.env.ENVIRONMENT || 'production',
    'service.namespace': 'microservices',
    'service.instance.id': process.env.HOSTNAME || 'unknown',
  })
);

// Create OTLP exporter
const traceExporter = new OTLPTraceExporter({
  url: process.env.OTEL_EXPORTER_OTLP_ENDPOINT || 'http://otel-collector:4317',
  compression: 'gzip',
});

// Create SDK
const sdk = new NodeSDK({
  resource: resource,
  spanProcessor: new BatchSpanProcessor(traceExporter, {
    maxQueueSize: 2048,
    maxExportBatchSize: 512,
    scheduledDelayMillis: 5000,
    exportTimeoutMillis: 30000,
  }),
  instrumentations: [
    getNodeAutoInstrumentations({
      // Customize auto-instrumentations
      '@opentelemetry/instrumentation-fs': {
        enabled: false,  // Disable filesystem instrumentation (noisy)
      },
      '@opentelemetry/instrumentation-http': {
        enabled: true,
        ignoreIncomingPaths: ['/health', '/metrics'],  // Ignore health checks
      },
      '@opentelemetry/instrumentation-express': {
        enabled: true,
      },
      '@opentelemetry/instrumentation-pg': {
        enabled: true,
        enhancedDatabaseReporting: true,  // Include SQL queries
      },
      '@opentelemetry/instrumentation-redis-4': {
        enabled: true,
      },
    }),
  ],
});

// Start SDK
sdk.start();

// Graceful shutdown
process.on('SIGTERM', () => {
  sdk.shutdown()
    .then(() => console.log('Tracing terminated'))
    .catch((error) => console.log('Error terminating tracing', error))
    .finally(() => process.exit(0));
});

module.exports = sdk;
```

#### 2.4.3 Application Code (app.js)

```javascript
// app.js
// IMPORTANT: Must require tracing before any other modules
require('./tracing');

const express = require('express');
const { trace, context, SpanStatusCode } = require('@opentelemetry/api');
const { SemanticAttributes } = require('@opentelemetry/semantic-conventions');

const app = express();
app.use(express.json());

// Get tracer
const tracer = trace.getTracer('analytics-service', '1.0.0');

// Health check (minimal tracing)
app.get('/health', (req, res) => {
  res.json({ status: 'healthy' });
});

// Analytics endpoint with custom spans
app.post('/analytics/events', async (req, res) => {
  // Current span is auto-created by express instrumentation
  const currentSpan = trace.getActiveSpan();
  currentSpan.setAttribute('event.count', req.body.events.length);

  try {
    // Custom child span for validation
    const validationSpan = tracer.startSpan('validate_events', {
      attributes: {
        'validation.type': 'events',
      },
    });

    await context.with(trace.setSpan(context.active(), validationSpan), async () => {
      try {
        const validEvents = await validateEvents(req.body.events);
        validationSpan.setAttribute('validation.passed', validEvents.length);
        validationSpan.setStatus({ code: SpanStatusCode.OK });
      } catch (error) {
        validationSpan.recordException(error);
        validationSpan.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      } finally {
        validationSpan.end();
      }
    });

    // Process events with custom span
    const processingSpan = tracer.startSpan('process_events');
    await context.with(trace.setSpan(context.active(), processingSpan), async () => {
      try {
        await processEvents(req.body.events);
        processingSpan.addEvent('Events processed successfully');
        processingSpan.setStatus({ code: SpanStatusCode.OK });
      } finally {
        processingSpan.end();
      }
    });

    res.json({ status: 'success', processed: req.body.events.length });
  } catch (error) {
    currentSpan.recordException(error);
    currentSpan.setStatus({ code: SpanStatusCode.ERROR });
    res.status(500).json({ error: error.message });
  }
});

// Function with manual span creation
async function processEvents(events) {
  const span = tracer.startSpan('process_events_detailed', {
    attributes: {
      'events.count': events.length,
    },
  });

  return context.with(trace.setSpan(context.active(), span), async () => {
    try {
      // Store in database (auto-instrumented)
      await storeInDatabase(events);

      // Send to analytics engine
      const analyticsSpan = tracer.startSpan('send_to_analytics', {
        kind: 1, // SpanKind.CLIENT
        attributes: {
          [SemanticAttributes.PEER_SERVICE]: 'analytics-engine',
          [SemanticAttributes.HTTP_METHOD]: 'POST',
        },
      });

      await context.with(trace.setSpan(context.active(), analyticsSpan), async () => {
        try {
          await sendToAnalyticsEngine(events);
          analyticsSpan.setStatus({ code: SpanStatusCode.OK });
        } finally {
          analyticsSpan.end();
        }
      });

      span.setStatus({ code: SpanStatusCode.OK });
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  });
}

// Extract trace context for logging
app.use((req, res, next) => {
  const span = trace.getActiveSpan();
  if (span) {
    const spanContext = span.spanContext();
    req.traceId = spanContext.traceId;
    req.spanId = spanContext.spanId;
    console.log(`[TraceID: ${req.traceId}, SpanID: ${req.spanId}] ${req.method} ${req.path}`);
  }
  next();
});

const PORT = process.env.PORT || 3000;
app.listen(PORT, () => {
  console.log(`Analytics service listening on port ${PORT}`);
});
```

---

## 3. Infrastructure Components Configuration

### 3.1 OpenTelemetry Collector Configuration

```yaml
# otel-collector-config.yaml

receivers:
  # OTLP receiver (primary)
  otlp:
    protocols:
      grpc:
        endpoint: 0.0.0.0:4317
        max_recv_msg_size_mib: 16
        max_concurrent_streams: 100
      http:
        endpoint: 0.0.0.0:4318
        cors:
          allowed_origins:
            - http://*
            - https://*

  # Jaeger receiver (for legacy compatibility)
  jaeger:
    protocols:
      grpc:
        endpoint: 0.0.0.0:14250
      thrift_http:
        endpoint: 0.0.0.0:14268
      thrift_compact:
        endpoint: 0.0.0.0:6831

  # Zipkin receiver (for compatibility)
  zipkin:
    endpoint: 0.0.0.0:9411

processors:
  # Memory limiter (prevent OOM)
  memory_limiter:
    check_interval: 1s
    limit_mib: 1024
    spike_limit_mib: 256

  # Batch processor (improve performance)
  batch:
    timeout: 10s
    send_batch_size: 1024
    send_batch_max_size: 2048

  # Resource processor (add global attributes)
  resource:
    attributes:
      - key: cluster.name
        value: production-k8s
        action: insert
      - key: deployment.environment
        from_attribute: environment
        action: insert

  # Attributes processor (manipulate span attributes)
  attributes:
    actions:
      # Remove sensitive data
      - key: http.request.header.authorization
        action: delete
      - key: db.statement
        action: update
        # Sanitize SQL statements
        from_attribute: db.statement

  # Tail-based sampling (intelligent sampling)
  tail_sampling:
    decision_wait: 10s
    num_traces: 100000
    expected_new_traces_per_sec: 1000
    policies:
      # Always sample errors
      - name: error-policy
        type: status_code
        status_code:
          status_codes: [ERROR]

      # Always sample slow requests (>1s)
      - name: latency-policy
        type: latency
        latency:
          threshold_ms: 1000

      # Sample 10% of successful fast requests
      - name: probabilistic-policy
        type: probabilistic
        probabilistic:
          sampling_percentage: 10

      # Always sample specific services
      - name: critical-services-policy
        type: string_attribute
        string_attribute:
          key: service.name
          values:
            - payment-service
            - auth-service
          enabled_regex_matching: false

  # Filter processor (drop unwanted spans)
  filter:
    traces:
      span:
        - 'attributes["http.target"] == "/health"'
        - 'attributes["http.target"] == "/metrics"'

  # Span processor (enrich spans)
  span:
    name:
      # Rename spans
      from_attributes: ["http.method", "http.route"]
      separator: " "

exporters:
  # OTLP exporter to Jaeger
  otlp/jaeger:
    endpoint: jaeger-collector:4317
    tls:
      insecure: false
      cert_file: /etc/otel/certs/client.crt
      key_file: /etc/otel/certs/client.key
      ca_file: /etc/otel/certs/ca.crt
    compression: gzip
    sending_queue:
      enabled: true
      num_consumers: 10
      queue_size: 5000
    retry_on_failure:
      enabled: true
      initial_interval: 5s
      max_interval: 30s
      max_elapsed_time: 300s

  # Kafka exporter (for buffering)
  kafka:
    brokers:
      - kafka-1:9092
      - kafka-2:9092
      - kafka-3:9092
    protocol_version: 2.8.0
    topic: jaeger-spans
    encoding: otlp_proto
    producer:
      compression: gzip
      max_message_bytes: 1000000
      required_acks: 1
    timeout: 10s
    retry:
      enabled: true
      initial_interval: 5s
      max_interval: 30s

  # Logging exporter (for debugging)
  logging:
    loglevel: debug
    sampling_initial: 5
    sampling_thereafter: 200

  # Prometheus exporter (collector metrics)
  prometheus:
    endpoint: "0.0.0.0:8889"
    namespace: otelcol
    const_labels:
      collector: otel-gateway

extensions:
  # Health check
  health_check:
    endpoint: ":13133"

  # pprof for profiling
  pprof:
    endpoint: ":1777"

  # zpages for diagnostics
  zpages:
    endpoint: ":55679"

service:
  extensions: [health_check, pprof, zpages]

  pipelines:
    # Traces pipeline
    traces:
      receivers: [otlp, jaeger, zipkin]
      processors:
        - memory_limiter
        - resource
        - attributes
        - filter
        - span
        - tail_sampling
        - batch
      exporters: [otlp/jaeger, kafka, logging]

  telemetry:
    logs:
      level: info
    metrics:
      level: detailed
      address: ":8888"
```

### 3.2 Kubernetes Deployment - OTel Collector DaemonSet

```yaml
# otel-collector-daemonset.yaml
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
      batch:
        timeout: 5s
        send_batch_size: 512

    exporters:
      otlp:
        endpoint: otel-collector-gateway:4317
        tls:
          insecure: true
        compression: gzip

    service:
      pipelines:
        traces:
          receivers: [otlp]
          processors: [memory_limiter, batch]
          exporters: [otlp]

---
apiVersion: apps/v1
kind: DaemonSet
metadata:
  name: otel-collector-agent
  namespace: observability
  labels:
    app: otel-collector-agent
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
          name: healthcheck
          protocol: TCP
        env:
        - name: POD_NAME
          valueFrom:
            fieldRef:
              fieldPath: metadata.name
        - name: POD_NAMESPACE
          valueFrom:
            fieldRef:
              fieldPath: metadata.namespace
        - name: NODE_NAME
          valueFrom:
            fieldRef:
              fieldPath: spec.nodeName
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
      tolerations:
      - key: node-role.kubernetes.io/master
        effect: NoSchedule

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
    protocol: TCP
  - name: otlp-http
    port: 4318
    targetPort: 4318
    protocol: TCP
```

### 3.3 Kubernetes Deployment - OTel Collector Gateway

```yaml
# otel-collector-gateway.yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: otel-collector-gateway-config
  namespace: observability
data:
  config.yaml: |
    # Full configuration from section 3.1
    # (Include the complete config.yaml from above)

---
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
        - containerPort: 14250
          name: jaeger-grpc
        - containerPort: 9411
          name: zipkin
        - containerPort: 8889
          name: prometheus
        env:
        - name: POD_NAME
          valueFrom:
            fieldRef:
              fieldPath: metadata.name
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
        - name: tls-certs
          mountPath: /etc/otel/certs
          readOnly: true
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
      - name: tls-certs
        secret:
          secretName: otel-collector-tls

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
  - name: jaeger-grpc
    port: 14250
    targetPort: 14250
  - name: zipkin
    port: 9411
    targetPort: 9411
  - name: prometheus
    port: 8889
    targetPort: 8889
```

---

*Continuing in next file due to length...*

---

*Document Version: 1.0*
*Last Updated: October 15, 2025*
*Status: Draft for Review*
