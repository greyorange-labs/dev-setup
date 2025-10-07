# API Gateway Request Flow Documentation

This document provides comprehensive flowcharts showing how the API Gateway handles different types of API requests, including all the code blocks, classes, methods, configurations, and filters that get executed during API handling.

## Table of Contents

1. [Main API Request Flow](#main-api-request-flow)
2. [Dynamic Routing Flow](#dynamic-routing-flow)
3. [Authentication Flow](#authentication-flow)
4. [Transformation Flow](#transformation-flow)
5. [Error Handling Flow](#error-handling-flow)
6. [WebSocket Flow](#websocket-flow)
7. [Static Route Flow](#static-route-flow)

---

## Main API Request Flow

This flowchart shows the complete request processing pipeline in the API Gateway.

```mermaid
flowchart TD
    A[Client Request] --> B[Spring Cloud Gateway]
    B --> C["RequestIdFilter<br/>Order: -1000<br/>Method: filter()"]
    C --> D["ServiceDiscoveryErrorFilter<br/>Order: -500<br/>Method: filter()"]
    D --> E["WebClientErrorFilter<br/>Order: -400<br/>Method: filter()"]
    E --> F["MaintenanceModeFilter<br/>Order: 0<br/>Method: filter()"]
    F --> G["PreFilterGeneric<br/>Order: -1<br/>Method: filter()"]
    G --> H["PreFilter<br/>Order: 0<br/>Method: filter()"]
    H --> I["AuthSpringFilter<br/>Order: -10<br/>Method: filter()"]
    I --> J["ValidationFilter<br/>Order: 0<br/>Method: filter()"]
    J --> K[Route Matching]
    K --> L{Static or Dynamic Route?}
    L -->|Static| M[Static Route Processing]
    L -->|Dynamic| N[Dynamic Route Processing]
    M --> O[Target Service Call]
    N --> O
    O --> P[Response Processing]
    P --> Q["PostFilterGeneric<br/>Order: -4<br/>Method: filter()"]
    Q --> R["RelayTokenFilter<br/>Order: 10000<br/>Method: filter()"]
    R --> S[Client Response]

    style C fill:#e1f5fe
    style D fill:#e1f5fe
    style E fill:#e1f5fe
    style F fill:#fff3e0
    style G fill:#f3e5f5
    style H fill:#f3e5f5
    style I fill:#ffebee
    style J fill:#e8f5e8
    style K fill:#fff9c4
    style O fill:#e3f2fd
```

### Key Components:

- **RequestIdFilter**: Generates unique request IDs for tracing
- **ServiceDiscoveryErrorFilter**: Handles service discovery failures
- **WebClientErrorFilter**: Manages web client errors
- **MaintenanceModeFilter**: Checks maintenance mode status
- **PreFilterGeneric**: Generic request preprocessing
- **PreFilter**: Client-specific request transformation
- **AuthSpringFilter**: Authentication and authorization
- **ValidationFilter**: Request validation
- **PostFilterGeneric**: Generic response processing
- **RelayTokenFilter**: Token relay for auth services

---

## Dynamic Routing Flow

This flowchart shows how dynamic routes are processed.

```mermaid
flowchart TD
    A[Request Arrives] --> B["AuthSpringFilter.filter()"]
    B --> C["findMatchingDynamicRoute()"]
    C --> D{Match Found?}
    D -->|No| E[Static Route Processing]
    D -->|Yes| F["handleDynamicRouteAuth()"]
    F --> G[Check Auth Required]
    G --> H{Auth Required?}
    H -->|No| I[Proceed to Target]
    H -->|Yes| J[Extract Token]
    J --> K["validateAuthTokenAndProceed()"]
    K --> L[Token Valid?]
    L -->|No| M[Return 401 Unauthorized]
    L -->|Yes| I
    I --> N["DynamicRouteLocator.buildRoute()"]
    N --> O[Path Matching]
    O --> P[Method Matching]
    P --> Q[Apply Filters]
    Q --> R[Target Service Call]
    R --> S[Response Processing]

    style B fill:#ffebee
    style F fill:#ffebee
    style G fill:#fff3e0
    style J fill:#e8f5e8
    style K fill:#e8f5e8
    style N fill:#e3f2fd
    style O fill:#e3f2fd
    style P fill:#e3f2fd
```

### Key Classes and Methods:

- **AuthSpringFilter.findMatchingDynamicRoute()**: Finds matching dynamic route
- **AuthSpringFilter.handleDynamicRouteAuth()**: Handles dynamic route authentication
- **DynamicRouteLocator.buildRoute()**: Builds route with predicates and filters
- **DynamicRouteAuthFilter.matchesPath()**: Matches request path to route pattern
- **DynamicRouteAuthFilter.matchesMethod()**: Matches HTTP method to route methods

---

## Authentication Flow

This flowchart shows the complete authentication process.

```mermaid
flowchart TD
    A[Request with Auth] --> B["AuthSpringFilter.filter()"]
    B --> C[Check Dynamic Route]
    C --> D{Dynamic Route?}
    D -->|Yes| E["handleDynamicRouteAuth()"]
    D -->|No| F[Check Static Auth Config]
    F --> G["api_gateway.enable.oauth_authentication"]
    G --> H{Auth Enabled?}
    H -->|No| I[Skip Auth]
    H -->|Yes| J[Check Excluded URLs]
    J --> K["matchesAnyRequestUrl()"]
    K --> L{Excluded URL?}
    L -->|Yes| I
    L -->|No| M[Check IP Whitelist]
    M --> N["isRequestFromWhitelistedIP()"]
    N --> O{IP Whitelisted?}
    O -->|Yes| I
    O -->|No| P[Extract Token]
    P --> Q["validateAuthTokenAndProceed()"]
    Q --> R["ReactiveAuthClient.customValidateAuthTokenGet()"]
    R --> S[Token Valid?]
    S -->|No| T[Return 401]
    S -->|Yes| U[Proceed to Target]
    I --> U

    style B fill:#ffebee
    style E fill:#ffebee
    style G fill:#fff3e0
    style J fill:#e8f5e8
    style M fill:#e8f5e8
    style P fill:#e3f2fd
    style Q fill:#e3f2fd
    style R fill:#e3f2fd
```

### Key Configuration Properties:

- **api_gateway.enable.oauth_authentication**: Enables/disables authentication
- **gor.whitlelistedIpSubnets**: IP addresses exempt from authentication
- **AUTH_EXCLUDED_API_PATTERNS**: URL patterns that don't require authentication

---

## Transformation Flow

This flowchart shows how request/response transformations are handled.

```mermaid
flowchart TD
    A[Request with Client Header] --> B["PreFilter.shouldFilter()"]
    B --> C{Client Header Present?}
    C -->|No| D[Skip Transformation]
    C -->|Yes| E["PreFilter.filter()"]
    E --> F[Extract Client from Header]
    F --> G["TransformerService.transform()"]
    G --> H[Check Transformation Config]
    H --> I["transformer.enableGeneric"]
    I --> J{Generic Enabled?}
    J -->|No| K[Client-Specific Transform]
    J -->|Yes| L[Generic Transform]
    K --> M[Apply Transformation Rules]
    L --> M
    M --> N[Modified Request Body]
    N --> O[Continue to Target]
    O --> P[Response Processing]
    P --> Q["PostFilterGeneric.filter()"]
    Q --> R[Response Transformation]
    R --> S[Return to Client]

    style B fill:#f3e5f5
    style E fill:#f3e5f5
    style G fill:#e8f5e8
    style H fill:#fff3e0
    style I fill:#fff3e0
    style M fill:#e3f2fd
    style Q fill:#f3e5f5
```

### Key Classes and Methods:

- **PreFilter.shouldFilter()**: Checks if transformation is needed
- **TransformerService.transform()**: Performs request transformation
- **PostFilterGeneric.filter()**: Performs response transformation
- **ValidationBuilder.getAllEnabledValidations()**: Loads validation rules

---

## Error Handling Flow

This flowchart shows how errors are handled throughout the request lifecycle.

```mermaid
flowchart TD
    A[Request Processing] --> B[Filter Chain Execution]
    B --> C{Error Occurs?}
    C -->|No| D[Continue Processing]
    C -->|Yes| E[Error Type Detection]
    E --> F{Error Type?}
    F -->|Service Discovery| G[ServiceDiscoveryErrorFilter]
    F -->|Web Client| H[WebClientErrorFilter]
    F -->|Validation| I[ValidationFilter]
    F -->|Auth| J[AuthSpringFilter]
    F -->|General| K[GatewayExceptionHandler]
    G --> L[Handle Service Discovery Error]
    H --> M[Handle Web Client Error]
    I --> N[Handle Validation Error]
    J --> O[Handle Auth Error]
    K --> P[Centralized Error Handling]
    L --> Q[Error Response]
    M --> Q
    N --> Q
    O --> Q
    P --> Q
    Q --> R[Return Error to Client]

    style E fill:#ffebee
    style G fill:#e1f5fe
    style H fill:#e1f5fe
    style I fill:#e8f5e8
    style J fill:#ffebee
    style K fill:#fff3e0
```

### Key Error Handling Components:

- **ServiceDiscoveryErrorFilter**: Handles service discovery failures
- **WebClientErrorFilter**: Manages web client errors
- **GatewayExceptionHandler**: Centralized error handling
- **ValidationFilter**: Request validation errors
- **AuthSpringFilter**: Authentication errors

---

## WebSocket Flow

This flowchart shows how WebSocket connections are handled.

```mermaid
flowchart TD
    A[WebSocket Request] --> B["DynamicWebSocketFilter.filter()"]
    B --> C[Check WebSocket Routes]
    C --> D["findMatchingWebSocketRoute()"]
    D --> E{Match Found?}
    E -->|No| F[Standard HTTP Processing]
    E -->|Yes| G[Check WebSocket Config]
    G --> H["websocket.enabled"]
    H --> I{WebSocket Enabled?}
    I -->|No| F
    I -->|Yes| J[Apply WebSocket Settings]
    J --> K["idleTimeoutSec"]
    K --> L["maxFrameBytes"]
    L --> M["pingIntervalSec"]
    M --> N["compression"]
    N --> O["subprotocols"]
    O --> P[WebSocket Upgrade]
    P --> Q[WebSocket Connection]
    Q --> R[Handle WebSocket Messages]

    style B fill:#e3f2fd
    style D fill:#e3f2fd
    style G fill:#fff3e0
    style J fill:#e8f5e8
    style P fill:#e1f5fe
    style Q fill:#e1f5fe
```

### Key WebSocket Components:

- **DynamicWebSocketFilter**: Handles WebSocket route matching
- **WebSocketConfig**: Configuration for WebSocket connections
- **WebSocket upgrade handling**: Protocol upgrade from HTTP to WebSocket

---

## Static Route Flow

This flowchart shows how static routes (configured in bootstrap.yml) are processed.

```mermaid
flowchart TD
    A[Request] --> B[Spring Cloud Gateway]
    B --> C[Route Locator]
    C --> D[Static Route Matching]
    D --> E[Path Predicate Matching]
    E --> F[Method Predicate Matching]
    F --> G[Header Predicate Matching]
    G --> H{Route Matched?}
    H -->|No| I[404 Not Found]
    H -->|Yes| J[Apply Route Filters]
    J --> K[RewritePath Filter]
    K --> L[AddRequestHeader Filter]
    L --> M[Load Balancer]
    M --> N[Target Service Call]
    N --> O[Response Processing]
    O --> P[Return to Client]

    style C fill:#e3f2fd
    style D fill:#e3f2fd
    style E fill:#e3f2fd
    style F fill:#e3f2fd
    style G fill:#e3f2fd
    style J fill:#e8f5e8
    style K fill:#e8f5e8
    style L fill:#e8f5e8
    style M fill:#fff3e0
```

### Key Static Route Components:

- **Route Locator**: Finds matching static routes
- **Predicate Matching**: Path, method, and header matching
- **Route Filters**: RewritePath, AddRequestHeader, etc.
- **Load Balancer**: Service discovery and load balancing

---

## Filter Execution Order

The following table shows the execution order of all filters in the API Gateway:

| Order | Filter | Purpose | Configuration |
|-------|--------|---------|---------------|
| -1000 | RequestIdFilter | Generate request IDs | Always enabled |
| -500 | ServiceDiscoveryErrorFilter | Handle service discovery errors | Always enabled |
| -400 | WebClientErrorFilter | Handle web client errors | Always enabled |
| -100 | DynamicRouteAuthFilter | Dynamic route authentication | Dynamic routing enabled |
| -50 | DynamicWebSocketFilter | WebSocket route handling | Dynamic routing enabled |
| -10 | AuthSpringFilter | Authentication | api_gateway.enable.oauth_authentication |
| -4 | PostFilterGeneric | Response transformation | transformer.enableGeneric |
| -1 | PreFilterGeneric | Request transformation | transformer.enableGeneric |
| 0 | PreFilter | Client-specific transformation | Client header present |
| 0 | ValidationFilter | Request validation | validationFilterOrder |
| 0 | MaintenanceModeFilter | Maintenance mode check | maintenance_mode.enabled |
| 1 | Client-specific filters | Various client transformations | Client-specific configs |
| 10000 | RelayTokenFilter | Token relay | Always enabled |

---

## Configuration Properties

### Core Configuration

```yaml
api-gateway:
  enable:
    oauth_authentication: false
    errorHandling: false
  dynamic-routing:
    enabled: true
    routes: []
```

### Transformation Configuration

```yaml
transformer:
  enableGeneric: false
  urlsList:
    ANY: ^/api/orders/\d+/\d+$
    POST: ^/api/orders/\d+/\d+$
```

### Validation Configuration

```yaml
validationFilterOrder: 0
```

### Maintenance Mode Configuration

```yaml
maintenance_mode:
  enabled: false
```

---

## Key Classes and Methods Reference

### Core Filter Classes

1. **RequestIdFilter**
   - `filter()`: Generates unique request IDs
   - `getOrder()`: Returns -1000

2. **AuthSpringFilter**
   - `filter()`: Main authentication logic
   - `findMatchingDynamicRoute()`: Finds dynamic routes
   - `handleDynamicRouteAuth()`: Handles dynamic route auth
   - `validateAuthTokenAndProceed()`: Validates tokens
   - `getOrder()`: Returns -10

3. **DynamicRouteLocator**
   - `buildRoute()`: Builds dynamic routes
   - `matchesPath()`: Path matching logic
   - `matchesMethod()`: Method matching logic

4. **ValidationFilter**
   - `filter()`: Request validation
   - `loadValidations()`: Loads validation rules
   - `getOrder()`: Returns validationFilterOrder

5. **TransformerService**
   - `transform()`: Request transformation
   - `transformResponse()`: Response transformation

### Configuration Classes

1. **DynamicRoutingProperties**
   - Contains dynamic route configuration
   - `DynamicRoute` inner class for route definitions

2. **ErrorHandlingConfig**
   - `gatewayExceptionHandler()`: Centralized error handling

3. **DynamicRouteConfiguration**
   - `dynamicRouteLocator()`: Route locator bean
   - `dynamicRouteRefreshService()`: Route refresh service

This documentation provides a comprehensive view of how the API Gateway processes requests through its various filters, configurations, and components.
