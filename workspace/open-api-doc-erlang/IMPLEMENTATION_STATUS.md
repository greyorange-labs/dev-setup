# OpenAPI Erlang Plugin - Implementation Status

**Last Updated:** 2025-11-13

---

## ✅ Phase 1: Plugin Structure - COMPLETED

### Status: ✅ DONE
- [x] Rebar3 plugin compiles with 0 errors/warnings (except unused exports)
- [x] Plugin accepts `--handler <file-path>` and `--output <file-path>` arguments
- [x] Plugin validates inputs and generates basic OpenAPI document
- [x] Fixed all undefined function calls (verified with `rebar3 xref`)
- [x] Plugin successfully runs: `rebar3 opapi extract --handler <path> --output <path>`

### Key Fixes Applied:
1. Used `epp:parse_file/2` instead of non-existent `erl_parse:parse_form_list/1`
2. Fixed `badarg` error: handle binary paths in `build_paths`
3. Fixed `function_clause` error: handle different method types in `method_to_lowercase`
4. Added `parse_file/2` to export list

### Output:
```bash
Found 0 contract(s), 0 type(s), 9 route(s)
SUCCESS: OpenAPI documentation written to /tmp/test_openapi.yaml
```

---

## 🚧 Phase 2: Unit Tests - PENDING

### Status: ⏸️ NOT STARTED
**Priority:** HIGH - Must be done before proceeding to Phase 3

### Components Needing Tests:

#### 2.1 Parser Tests (`test/rebar3_opapi_parser_tests.erl`)
- [ ] Test `parse_file/2` with valid Erlang file
- [ ] Test `parse_file/2` with file containing `-opapi_contract` attributes
- [ ] Test `parse_file/2` with file containing `-type` definitions
- [ ] Test `parse_file/2` with file containing `routes/0` function
- [ ] Test `parse_file/2` with file containing `-include` directives
- [ ] Test `parse_file/2` with non-existent file (error handling)
- [ ] Test `parse_file/2` with malformed Erlang code (error handling)
- [ ] Test `extract_contracts/1` with valid contract attributes
- [ ] Test `extract_contracts/1` with empty list
- [ ] Test `extract_contracts/1` with invalid contract format
- [ ] Test `extract_types/1` with valid type definitions
- [ ] Test `extract_types/1` with parameterized types
- [ ] Test `extract_types/1` with empty list
- [ ] Test `extract_routes/1` with valid routes/0 function
- [ ] Test `extract_routes/1` with multiple routes
- [ ] Test `extract_routes/1` with nested maps in routes
- [ ] Test `extract_routes/1` with no routes/0 function

#### 2.2 Builder Tests (`test/rebar3_opapi_builder_tests.erl`)
- [ ] Test `build/2` with empty operations list
- [ ] Test `build/2` with single operation
- [ ] Test `build/2` with multiple operations
- [ ] Test `build_paths/1` with operations having same path, different methods
- [ ] Test `build_paths/1` with operations having different paths
- [ ] Test `method_to_lowercase/1` with binary input
- [ ] Test `method_to_lowercase/1` with list input
- [ ] Test `method_to_lowercase/1` with atom input
- [ ] Test `build_info/1` with atom app name
- [ ] Test `build_info/1` with binary app name

#### 2.3 YAML Writer Tests (`test/rebar3_opapi_yaml_writer_tests.erl`)
- [ ] Test `write/1` with simple map
- [ ] Test `write/1` with nested maps
- [ ] Test `write/1` with lists
- [ ] Test `write/1` with empty map
- [ ] Test `write/1` with binary values
- [ ] Test `write/1` with atom values
- [ ] Test `write/1` with integer values
- [ ] Test `write/1` with boolean values
- [ ] Test YAML output format correctness

#### 2.4 Integration Tests (`test/rebar3_opapi_integration_tests.erl`)
- [ ] Test end-to-end: parse handler → build OpenAPI → write YAML
- [ ] Test with real handler file (gm_common_http_handler.erl)
- [ ] Test with handler containing contracts
- [ ] Test with handler containing type definitions
- [ ] Validate generated YAML is valid OpenAPI 3.0.x spec

### Test Fixtures Needed:
- [ ] `test/fixtures/simple_handler.erl` - basic handler with routes
- [ ] `test/fixtures/handler_with_contracts.erl` - handler with `-opapi_contract` attributes
- [ ] `test/fixtures/handler_with_types.erl` - handler with `-type` definitions
- [ ] `test/fixtures/handler_with_includes.erl` - handler with `-include` directives
- [ ] `test/fixtures/malformed_handler.erl` - handler with syntax errors
- [ ] `test/fixtures/empty_handler.erl` - handler with no routes

### Test Infrastructure:
- [ ] Set up `rebar.config` test profile
- [ ] Add EUnit to test dependencies
- [ ] Create `test/` directory structure
- [ ] Add test helper modules if needed
- [ ] Document how to run tests in README

### Commands to Run Tests:
```bash
cd /Users/amar.c/workspace/openapi-erl
rebar3 eunit
rebar3 ct  # if using Common Test
```

---

## 📋 Phase 3: Documentation Generation Enhancement - PENDING

### Status: ⏸️ NOT STARTED
**Dependencies:** Phase 2 (Unit Tests) must be completed first

### Tasks:
- [ ] Implement contract extraction from `-opapi_contract` attributes
- [ ] Implement type definition extraction from `-type` attributes
- [ ] Convert Erlang types to OpenAPI schemas
- [ ] Support nested types (records, maps, lists)
- [ ] Support OpenAPI 3.0.x schema features:
  - [ ] `oneOf`
  - [ ] `anyOf`
  - [ ] `allOf`
  - [ ] `not`
  - [ ] `enum`
  - [ ] `required` fields
  - [ ] `additionalProperties`
- [ ] Generate complete OpenAPI document with:
  - [ ] Request body schemas
  - [ ] Response schemas
  - [ ] Parameters (query, path, header)
  - [ ] Operation descriptions
  - [ ] Tags
  - [ ] Examples

---

## 📋 Phase 4: Runtime Validation Layer - PENDING

### Status: ⏸️ NOT STARTED
**Dependencies:** Phase 3 must be completed first

### Tasks:
- [ ] Design validation layer architecture
- [ ] Evaluate Jesse for OpenAPI 3.0.x schema validation
- [ ] Implement `gm_opapi_schema_converter.erl` (OpenAPI → JSON Schema)
- [ ] Implement `gm_opapi_validator.erl`:
  - [ ] `validate_request_body/2`
  - [ ] `validate_response_body/3`
- [ ] Integrate validation into handlers:
  - [ ] Add validation calls in `handle_request/3`
  - [ ] Error handling and reporting
- [ ] Test runtime validation with sample requests

---

## 📋 Phase 5: Doc-to-Code Generation - PENDING

### Status: ⏸️ NOT STARTED
**Dependencies:** Phase 4 must be completed first

### Tasks:
- [ ] Parse OpenAPI spec (YAML/JSON)
- [ ] Generate handler module with routes
- [ ] Generate `-opapi_contract` attributes from spec
- [ ] Generate `-type` definitions from schemas
- [ ] Generate validation calls
- [ ] Integrate with existing handler patterns

---

## 🔧 Current Issues to Fix

### Known Issues:
1. **Method showing as "undefined" in YAML output**
   - Routes are extracted but method field is not correctly converted
   - Need to debug `extract_routes_from_methods/2` function
   - The method should be `post`, `get`, etc., not `undefined`

2. **No contracts extracted yet**
   - Expected: 0 (handler doesn't have `-opapi_contract` attributes yet)
   - Need to add sample contracts to test handler

3. **No types extracted yet**
   - Expected: 0 (handler doesn't have `-type` definitions yet)
   - Need to add sample types to test handler

---

## 🎯 Next Steps (Immediate)

1. **Create unit test infrastructure** (Phase 2)
   - Set up test directory
   - Create test fixtures
   - Write first test for `parse_file/2`
   - Run `rebar3 eunit` to verify test setup works

2. **Fix "undefined" method issue**
   - Debug route extraction
   - Ensure methods are correctly extracted from AST
   - Verify method conversion works for all HTTP methods

3. **Add sample contracts to test handler**
   - Add `-opapi_contract` attribute to one route
   - Test contract extraction
   - Verify contract appears in generated OpenAPI doc

---

## 📚 Lessons Learned

### Erlang Development Best Practices:
1. **ALWAYS verify function existence before using**
   - Check `erlang.org/doc/man/` documentation
   - Never assume function names or arities

2. **ALWAYS run `rebar3 xref` after changes**
   - Catches undefined function calls
   - Identifies unused exports

3. **Compilation workflow:**
   - For plugins: `rebar3 compile` → `rebar3 xref`
   - For projects: `make` → `make xref`
   - Fix all xref errors before proceeding

4. **Test incrementally:**
   - Small change → compile → xref → test → commit
   - Never accumulate multiple changes without testing

---

## 📁 File Structure

```
openapi-erl/
├── src/
│   ├── rebar3_opapi.erl              ✅ Main plugin module
│   ├── rebar3_opapi_prv_extract.erl  ✅ Extract provider
│   ├── rebar3_opapi_parser.erl       ✅ Parser module
│   ├── rebar3_opapi_builder.erl      ✅ OpenAPI builder
│   └── rebar3_opapi_yaml_writer.erl  ✅ YAML writer
├── test/                              ⏸️ NOT CREATED YET
│   ├── fixtures/                      ⏸️ Test fixtures
│   ├── rebar3_opapi_parser_tests.erl  ⏸️ Parser tests
│   ├── rebar3_opapi_builder_tests.erl ⏸️ Builder tests
│   └── rebar3_opapi_yaml_writer_tests.erl ⏸️ Writer tests
├── rebar.config                       ✅ Plugin configuration
└── README.md                          ⏸️ Documentation (pending)
```

---

## 🔗 References

- **Erlang Official Docs:** https://www.erlang.org/doc/man/
- **epp module:** https://www.erlang.org/doc/man/epp.html
- **erl_parse module:** https://www.erlang.org/doc/man/erl_parse.html
- **erl_scan module:** https://www.erlang.org/doc/man/erl_scan.html
- **OpenAPI 3.0.x Spec:** https://swagger.io/specification/
- **JSON Schema Draft 7:** https://json-schema.org/draft-07/schema
- **Jesse (JSON Schema validator):** https://github.com/for-GET/jesse

---

## ⚠️ Important Notes

- **DO NOT proceed to Phase 3 without completing Phase 2 (Unit Tests)**
- **DO NOT commit code with `rebar3 xref` errors**
- **ALWAYS verify functions exist in Erlang docs before using**
- **Test each small change incrementally**

