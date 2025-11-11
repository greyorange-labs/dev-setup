# Implementation Plan: rebar3_openapi Plugin

## Overview
Build a rebar3 plugin (`rebar3_openapi`) that provides bidirectional code generation between OpenAPI specs and Erlang/Cowboy handlers, using a hybrid approach that leverages openapi-generator's code generation while maintaining compatibility with your existing project structure.

---

## Plan Structure

### **Phase 1: Project Setup & Architecture Design**
### **Phase 2: OpenAPI → Erlang Code Generation**
### **Phase 3: Erlang → OpenAPI Doc Generation**
### **Phase 4: Integration & Refactoring Support**
### **Phase 5: CI Integration & Documentation**

---

## Phase 1: Project Setup & Architecture Design

### 1.1 Repository Structure
**Location:** `/Users/amar.c/workspace/rebar3_openapi/`

```
rebar3_openapi/
├── README.md
├── rebar.config
├── src/
│   ├── rebar3_openapi.erl              % Main plugin provider
│   ├── rebar3_openapi_prv_generate.erl % OpenAPI → Code
│   ├── rebar3_openapi_prv_extract.erl  % Code → OpenAPI
│   ├── rebar3_openapi_generator.erl    % Wrapper for openapi-generator CLI
│   ├── rebar3_openapi_transformer.erl  % Transform generated code to hybrid pattern
│   ├── rebar3_openapi_parser.erl       % OpenAPI YAML/JSON parser
│   ├── rebar3_openapi_extractor.erl    % Extract OpenAPI from Erlang code
│   ├── rebar3_openapi_validator.erl    % Validate consistency
│   └── rebar3_openapi_utils.erl        % Utilities
├── priv/
│   └── templates/                       % Mustache templates for code generation
│       ├── handler_module.mustache
│       ├── router_module.mustache
│       ├── logic_handler_module.mustache
│       └── trails_wrapper.mustache
├── test/
└── examples/
    └── bsh_http_handler_refactoring/    % Example refactoring guide
```

### 1.2 Dependencies
```erlang
%% rebar.config
{deps, [
    {yamerl, "0.10.0"},           % YAML parsing
    {jsx, "3.1.0"},               % JSON handling
    {mustache, "0.3.0"},          % Template engine
    {redbug, "2.0.7"}             % Debugging utilities
]}.

{plugins, [
    {rebar3_hex, "7.0.7"}         % For hex publishing
]}.
```

### 1.3 Core Design Decisions

**Architecture Pattern:**
- Leverage `openapi-generator` CLI to generate initial code
- Transform generated code to hybrid pattern (router + trails() wrapper)
- Store metadata in `.openapi_manifest.json` to track handler-spec mappings

**File Manifest Structure:**
```json
{
  "version": "1.0",
  "handlers": {
    "bsh_http_handler": {
      "spec_file": "specs/butler_shared.yml",
      "router_module": "bsh_http_router",
      "logic_module": "bsh_http_logic_handler",
      "openapi_json": "apps/butler_shared/priv/openapi/bsh_http.json",
      "last_generated": "2025-10-29T10:30:00Z"
    }
  }
}
```

---

## Phase 2: OpenAPI → Erlang Code Generation

### 2.1 Command Interface

```bash
# Generate new handler from OpenAPI spec
rebar3 openapi generate \
    --spec specs/butler_shared.yml \
    --app butler_shared \
    --handler bsh_http_handler \
    --logic-module bsh_http_logic_handler \
    --package-name butler_shared

# Update existing handler (modify in-place)
rebar3 openapi generate \
    --spec specs/butler_shared.yml \
    --app butler_shared \
    --handler bsh_http_handler \
    --update

# Dry-run (show what would be generated)
rebar3 openapi generate \
    --spec specs/butler_shared.yml \
    --handler bsh_http_handler \
    --dry-run
```

### 2.2 Generation Workflow

**Step 1: Invoke openapi-generator**
```
1. Check if openapi-generator is installed
2. If not, guide user to install or use Docker
3. Generate code to temporary directory:
   openapi-generator generate \
       -i <spec.yml> \
       -g erlang-server \
       -o /tmp/rebar3_openapi_<uuid> \
       --additional-properties=packageName=<name>
```

**Step 2: Parse Generated Code**
```
1. Parse router module to extract:
   - Path → OperationID mappings
   - HTTP methods per path
   - Handler module name
2. Parse handler module to extract:
   - Operation-specific functions
   - Allowed methods
   - Content types
3. Parse API module to extract:
   - Request/response validation rules
   - Schema references
4. Extract openapi.json for Jesse validation
```

**Step 3: Transform to Hybrid Pattern**

Generate 3 modules:

**A. Router Module** (`<name>_router.erl`)
- Contains openapi-generator router logic
- **PLUS** implements `trails()` function for compatibility:
  ```erlang
  %% Generated
  trails() ->
      [{Path, ?MODULE, {Operations, LogicModule}}
       || {Path, Operations} <- get_paths()].
  ```

**B. Handler Module** (`<name>_handler.erl` or modify existing)
- If `--update` flag: Parse existing handler, update only route-related functions
- If new: Generate full handler module
- Contains:
  - `init/2` - Extract operation ID from opts
  - `allowed_methods/2` - Based on OpenAPI operations
  - `content_types_accepted/2` & `content_types_provided/2`
  - `handle_type_accepted/2` & `handle_type_provided/2` - Dispatch to logic handler
  - Validation integration using Jesse

**C. Logic Handler Module** (`<name>_logic_handler.erl`)
- Skeleton with callbacks for each operation
- User implements business logic here
- Example:
  ```erlang
  provide_callback(Class, 'get_status', Req, Context) ->
      %% TODO: Implement business logic
      %% Call your controller: gm_diagnostics_http_controller:handle_api_get_status()
      {<<"Not implemented">>, Req, Context}.
  ```

**Step 4: Generate Jesse Schemas**
```
1. Extract schemas from openapi.json
2. Store full openapi.json in apps/<app>/priv/openapi/<handler>.json
3. Create symlinks or copies for backward compatibility if needed
```

**Step 5: Update Manifest**
```
1. Update .openapi_manifest.json
2. Record spec → handler mapping
3. Store generation metadata
```

### 2.3 Code Modification Strategy (--update mode)

**For Existing Handlers:**
1. Parse existing handler AST using `erl_parse`
2. Identify sections:
   - Exports
   - Route definitions (trails/0 or route functions)
   - Handler callbacks
   - API functions
3. Update ONLY:
   - Route definitions (add new routes)
   - Handler callbacks (ensure proper dispatch)
   - Add new API function stubs with `%% TODO: Implement`
4. PRESERVE:
   - All existing API function implementations
   - All business logic
   - Comments and formatting (best effort)

---

## Phase 3: Erlang → OpenAPI Doc Generation

### 3.1 Command Interface

```bash
# Extract OpenAPI from single handler
rebar3 openapi extract \
    --handler bsh_http_handler \
    --app butler_shared \
    --output specs/butler_shared.yml

# Extract from all handlers in app
rebar3 openapi extract \
    --app butler_shared \
    --output-dir specs/

# Validate generated spec against existing
rebar3 openapi validate \
    --handler bsh_http_handler \
    --spec specs/butler_shared.yml
```

### 3.2 Extraction Workflow

**Step 1: Parse Handler Module**
```
1. Parse .erl file to AST
2. Extract route information:
   - If hybrid handler: Parse router module's get_operations()
   - If legacy handler: Parse trails() and path matching in handle_*_request
3. Extract HTTP methods from allowed_methods/2 or ?ALLOWED_METHODS macro
4. Extract API function names and their path mappings
```

**Step 2: Extract Validation Schemas**
```
1. Find decorator annotations: -decorate({cowboy_utils, validator, [{schema, "name"}]})
2. For each schema name:
   - Look up JSON file in priv/ directories
   - Convert Jesse JSON schema to OpenAPI schema
3. Build schema components section
```

**Step 3: Infer Request/Response Schemas**
```
1. For each API function:
   - Check if decorated with validator → requestBody schema
   - Analyze response builder calls (respond_with_code/jsx:encode) → response schema
   - Extract status codes from function
2. Build OpenAPI paths section:
   - Path from route mapping
   - Method from allowed_methods
   - Request body from decorator schema
   - Responses from inferred status codes
```

**Step 4: Generate OpenAPI Document**
```
1. Build OpenAPI YAML structure:
   - info: Extract from module doc or use defaults
   - servers: Use configured server URLs
   - paths: From extracted routes
   - components/schemas: From Jesse schemas
2. Validate generated OpenAPI (structural validation)
3. Write to output file
```

### 3.3 Schema Conversion (Jesse → OpenAPI)

**Mapping Rules:**
```
Jesse JSON Schema          → OpenAPI Schema
------------------           ----------------
"type": "object"          → type: object
"properties": {...}       → properties: {...}
"required": [...]         → required: [...]
"additionalProperties"    → additionalProperties
"enum": [...]             → enum: [...]
"$ref": "#/definitions/X" → $ref: "#/components/schemas/X"
```

---

## Phase 4: Integration & Refactoring Support

### 4.1 Integration with Existing Project

**Add to project's rebar.config:**
```erlang
{plugins, [
    {rebar3_openapi, {git, "https://github.com/your-org/rebar3_openapi.git", {branch, "main"}}}
]}.

{openapi, [
    {specs_dir, "specs"},
    {manifest_file, ".openapi_manifest.json"},
    {server_url, "http://localhost:8181"},
    {validation_on_generate, true}
]}.
```

**No changes needed to cowboy_routes_manager** - it already supports trails()!

### 4.2 Refactoring Guide for bsh_http_handler

**Document the refactoring steps:**

1. **Backup**: `cp bsh_http_handler.erl bsh_http_handler.erl.backup`

2. **Create OpenAPI spec** (if not exists):
   ```bash
   # Extract current API to OpenAPI
   rebar3 openapi extract \
       --handler bsh_http_handler \
       --app butler_shared \
       --output specs/butler_shared.yml
   ```

3. **Review & enhance spec**:
   - Add descriptions
   - Add examples
   - Refine schemas
   - Get PM/architect approval

4. **Generate hybrid handler**:
   ```bash
   rebar3 openapi generate \
       --spec specs/butler_shared.yml \
       --app butler_shared \
       --handler bsh_http_handler \
       --logic-module bsh_http_logic_handler \
       --update
   ```

5. **Move business logic**:
   - Tool generates skeleton in `bsh_http_logic_handler.erl`
   - Manually move controller calls from handler to logic module:
     ```erlang
     % FROM: bsh_http_handler.erl
     api_get_status(Req, State) ->
         {Code, RespBody} = gm_diagnostics_http_controller:handle_api_get_status(),
         ...

     % TO: bsh_http_logic_handler.erl
     provide_callback(default, 'get_status', Req, Context) ->
         {Code, RespBody} = gm_diagnostics_http_controller:handle_api_get_status(),
         {RespBody, Req, Context}.
     ```

6. **Update route registration** (if needed):
   - May need to register router module instead of handler
   - Or use trails() wrapper (Option B - should work as-is)

7. **Test**:
   - Run existing tests
   - Manual API testing
   - Verify all endpoints work

### 4.3 AI-Assisted Refactoring Prompt

**Provide a prompt template for users:**

```
You are helping refactor an Erlang cowboy_rest handler to use a hybrid openapi-generator pattern.

Current handler module: [paste handler code]
Generated router module: [paste router code]
Generated handler template: [paste generated code]

Please:
1. Extract all API function implementations from the current handler
2. Convert each function to match the logic_handler callback signature:
   provide_callback(Class, OperationID, Req, Context) for GET
   accept_callback(Class, OperationID, Req, Context) for POST/PUT/DELETE
3. Maintain all business logic, controller calls, and response formatting
4. Map the operation IDs as follows: [list operation ID mappings]

Generate the complete logic_handler module.
```

---

## Phase 5: CI Integration & Documentation

### 5.1 CI Validation Command

```bash
# In CI pipeline
rebar3 openapi validate \
    --handler bsh_http_handler \
    --spec specs/butler_shared.yml \
    --strict

# Exit codes:
# 0 - Match
# 1 - Mismatch (with diff output)
# 2 - Error
```

**Validation Checks:**
1. Extract OpenAPI from code
2. Compare with human-written spec
3. Report differences:
   - Missing endpoints
   - Different schemas
   - Different response codes
   - Etc.

### 5.2 Comprehensive Documentation

**README.md sections:**
1. Installation
2. Quick start
3. Commands reference
4. Configuration options
5. OpenAPI → Code workflow
6. Code → OpenAPI workflow
7. Refactoring existing handlers
8. CI integration
9. Troubleshooting
10. Contributing

**Tutorial: Refactoring bsh_http_handler**
- Step-by-step guide with screenshots
- Before/after code examples
- Common pitfalls
- Testing strategies

---

## Implementation Phases Summary

### **Phase 1: Foundation (Week 1-2)**
- ✅ Set up repository structure
- ✅ Implement openapi-generator wrapper
- ✅ Implement OpenAPI parser
- ✅ Create manifest management
- ✅ Basic rebar3 plugin infrastructure

### **Phase 2: OpenAPI → Code (Week 3-4)**
- ✅ Implement generation workflow
- ✅ Code transformation logic (openapi-gen → hybrid)
- ✅ Router module with trails() wrapper
- ✅ Handler module generation
- ✅ Logic handler skeleton generation
- ✅ Jesse schema extraction
- ✅ `--update` mode for existing handlers

### **Phase 3: Code → OpenAPI (Week 5-6)**
- ✅ Handler AST parser
- ✅ Route extraction (hybrid & legacy patterns)
- ✅ Schema extraction from priv/
- ✅ Jesse → OpenAPI conversion
- ✅ OpenAPI document generation
- ✅ Validation command

### **Phase 4: Integration (Week 7)**
- ✅ Test with bsh_http_handler refactoring
- ✅ Refactoring guide & AI prompt
- ✅ Integration documentation
- ✅ Example workflows

### **Phase 5: Polish & Release (Week 8)**
- ✅ CI integration guide
- ✅ Comprehensive documentation
- ✅ Publish to hex.pm
- ✅ Video tutorial (optional)

---

## Technical Considerations

### Error Handling
- Graceful degradation if openapi-generator not installed
- Clear error messages with actionable suggestions
- Backup before modifying files
- Dry-run mode for safety

### Backward Compatibility
- Support legacy handlers without openapi
- Non-breaking changes to existing code
- Gradual migration path

### Extensibility
- Plugin architecture for custom transformations
- Configurable templates
- Hook system for pre/post generation

### Testing Strategy
- Unit tests for parsers and transformers
- Integration tests with sample OpenAPI specs
- End-to-end tests with actual handler generation
- Test against your butler_server_develop project

---

## Risks & Mitigation

| Risk                            | Impact   | Mitigation                                              |
| ------------------------------- | -------- | ------------------------------------------------------- |
| openapi-generator not installed | High     | Provide Docker alternative, clear install docs          |
| Code modification breaks logic  | Critical | Never modify business logic, only routing; backup files |
| AST parsing complexity          | Medium   | Use robust parser libs, handle edge cases               |
| Schema conversion gaps          | Medium   | Support subset, document limitations                    |
| Maintenance burden              | Medium   | Leverage openapi-generator; minimize custom code        |

---

## Success Criteria

✅ Successfully refactor `bsh_http_handler` using the tool
✅ Generate OpenAPI doc from `put_http_handler` that matches API behavior
✅ New handlers can be generated from OpenAPI specs
✅ CI validation pipeline works
✅ Documentation covers all workflows
✅ Tool published to hex.pm
✅ Zero manual changes to `cowboy_routes_manager`

---

## Architecture Decision: Hybrid Approach (Option B)

### Why Hybrid?

**Advantages:**
1. ✅ Leverages openapi-generator's battle-tested code generation
2. ✅ Minimal disruption to existing `cowboy_routes_manager`
3. ✅ Clear migration path from legacy to full openapi-generator pattern (B → C)
4. ✅ New handlers benefit from operation ID pattern
5. ✅ Existing handlers can gradually migrate

**Trade-offs Compared to Options A & C:**

| Aspect                       | Option A (Pure Compatibility) | **Option B (Hybrid)** | Option C (Full openapi-gen) |
| ---------------------------- | ----------------------------- | --------------------- | --------------------------- |
| Consumer Changes             | ✅ None                        | ⚠️ Small (wrapper)     | ❌ Significant               |
| openapi-generator Similarity | ⚠️ Low                         | ✅ High                | ✅ Perfect                   |
| Maintainability              | ⚠️ Medium                      | ✅ Good                | ✅ Excellent                 |
| Refactoring Effort           | ✅ Minimal                     | ⚠️ Medium              | ❌ High                      |
| Future Migration             | ❌ Hard to migrate             | ✅ Easy to Option C    | N/A                         |

### Implementation Details

**Router Module Pattern:**
```erlang
%% Generated router with trails() compatibility
-module(bsh_http_router).
-behaviour(trails_handler).

%% openapi-generator style
-export([get_paths/1, get_operations/0]).

%% trails compatibility
-export([trails/0]).

trails() ->
    %% Wraps openapi-generator router to work with existing infrastructure
    LogicModule = bsh_http_logic_handler,
    Paths = get_paths(LogicModule),
    [{Path, Handler, Opts} || {'_', PathList} <- Paths, {Path, Handler, Opts} <- PathList].
```

### Migration Path (B → C)

When ready to migrate to full openapi-generator pattern:

1. **Phase 1:** All handlers using hybrid pattern (trails() wrapper)
2. **Phase 2:** Update `cowboy_routes_manager` to support direct router modules
3. **Phase 3:** Remove trails() wrapper from routers
4. **Phase 4:** Full openapi-generator pattern

Each phase can be done incrementally without breaking existing handlers.

---

## Appendix: Key Design Decisions

### 1. Why openapi-generator?
- Industry standard, multi-language support
- Well-maintained, active community
- Supports OpenAPI 3.0+
- Jesse validation out of the box

### 2. Why Hybrid Pattern?
- Balance between innovation and stability
- Gradual migration reduces risk
- Easier to get buy-in from team
- Can always evolve to full pattern later

### 3. Why Manifest File?
- Track spec ↔ handler mappings
- Enable incremental updates
- Support CI validation
- Version control friendly

### 4. Why AST Parsing for Extraction?
- More accurate than regex/string parsing
- Can handle complex Erlang syntax
- Enables safe code modification
- Standard library support (erl_parse)

### 5. Why Jesse for Validation?
- Already used in your project
- OpenAPI-compatible schema format
- Erlang-native library
- Good error messages

---

## Next Steps

Once this plan is approved:

1. Create repository structure
2. Set up basic rebar3 plugin infrastructure
3. Implement openapi-generator wrapper
4. Build prototype for simple OpenAPI → Code generation
5. Test with minimal example
6. Iterate and expand functionality
7. Complete all 5 phases

**Estimated Timeline:** 8 weeks for full implementation
**MVP (Phases 1-2):** 4 weeks

---

## References

- [openapi-generator](https://github.com/OpenAPITools/openapi-generator)
- [cowboy REST handlers](https://ninenines.eu/docs/en/cowboy/2.9/guide/rest_handlers/)
- [Jesse JSON Schema Validator](https://github.com/for-GET/jesse)
- [Rebar3 Plugin Tutorial](https://www.rebar3.org/docs/tutorials/building_plugins/)
- [OpenAPI 3.0 Specification](https://swagger.io/specification/)

