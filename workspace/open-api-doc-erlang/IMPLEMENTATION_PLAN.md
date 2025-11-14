# OpenAPI Code-to-Doc Generation Implementation Plan

## Status: In Progress

### Milestone 1: Create Parse Transform ✅ COMPLETED
- Created `gm_opapi_parse_transform.erl`
- Extracts `-opapi_contract` attributes and type definitions
- Stores contracts in ETS table at compile-time

### Milestone 2: Create Contract Registry (IN PROGRESS)
- Create `gm_opapi_contracts.erl` for runtime access to contracts
- Implement ETS table getter functions

### Milestone 3: Create Extractor Module
- Create `gm_opapi_extractor.erl`
- Convert Erlang types to OpenAPI schemas
- Extract operations from handler modules

### Milestone 4: Create Rebar3 Plugin
- Initialize git repo: `gh repo create greyorange-labs/openapi-erl --public`
- Create plugin structure
- Implement provider registration
- Implement extraction provider
- Implement OpenAPI document builder

### Milestone 5: Update Handler Module (Example)
- Add parse transform to `gm_common_http_handler.erl`
- Add `-opapi_contract` attributes
- Add `-type` definitions

### Milestone 6: Integration & Testing
- Add plugin to `rebar.config`
- Test extraction
- Validate generated OpenAPI document

## Approach: Contracts in Erlang Code
- Single source of truth
- Contracts defined using `-opapi_contract` attributes
- Type definitions using `-type` specs
- Parse transform extracts at compile-time

## Workspace
- Project: `/Users/amar.c/workspace/gm_core/butler_server_release`
- Plugin: `/Users/amar.c/workspace/openapi-erl` (to be created)

