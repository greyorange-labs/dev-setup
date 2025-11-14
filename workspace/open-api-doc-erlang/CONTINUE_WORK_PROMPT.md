# Continue OpenAPI Erlang Plugin Development - Full Context Prompt

**Copy and paste this entire prompt to continue work in the openapi-erl workspace**

---

## 📍 Current Status

I'm working on a **rebar3 plugin for OpenAPI 3.0.x documentation generation** from Erlang handlers.

**Workspace:** `/Users/amar.c/workspace/openapi-erl` (GitHub: `https://github.com/greyorange-labs/openapi-erl`, branch: `develop`)

**Project Location:** `/Users/amar.c/workspace/gm_core/butler_server_release` (where the plugin is used)

---

## ✅ What's Completed (Phase 1)

### Plugin Structure - WORKING
- ✅ Plugin compiles with **0 undefined function errors** (verified with `rebar3 xref`)
- ✅ Accepts `--handler <file-path>` and `--output <file-path>` arguments
- ✅ Successfully generates OpenAPI 3.0.3 YAML documents
- ✅ Extracts routes from handlers (tested: 9 routes extracted successfully)
- ✅ All critical bugs fixed:
  - Fixed `erl_parse:parse_form_list/1` undefined → used `epp:parse_file/2` instead
  - Fixed `badarg` error in path handling
  - Fixed `function_clause` error in method type conversion

### Test Command That Works:
```bash
cd /Users/amar.c/workspace/gm_core/butler_server_release
rebar3 opapi extract \
  --handler apps/butler_shared/src/interfaces/in/gm_common_http_handler.erl \
  --output /tmp/test_openapi.yaml
```

**Output:**
```
Found 0 contract(s), 0 type(s), 9 route(s)
SUCCESS: OpenAPI documentation written to /tmp/test_openapi.yaml
```

---

## 🔧 Current Known Issues

1. **Method field shows "undefined" in YAML output**
   - Routes are extracted correctly (9 found)
   - But method in YAML shows as `undefined` instead of `get`, `post`, etc.
   - Likely issue in `extract_routes_from_methods/2` function
   - Location: `/Users/amar.c/workspace/openapi-erl/src/rebar3_opapi_parser.erl:145-160`

2. **No contracts extracted yet** (Expected - handler doesn't have contracts)

3. **No types extracted yet** (Expected - handler doesn't have types)

---

## 🎯 Next Task: Write Unit Tests (Phase 2)

**PRIORITY: HIGH - Must complete before proceeding to Phase 3**

### Task Breakdown:

#### Step 1: Test Infrastructure Setup
- [ ] Create `test/` directory structure
- [ ] Add test fixtures in `test/fixtures/`
- [ ] Configure EUnit in `rebar.config`
- [ ] Verify test setup with `rebar3 eunit`

#### Step 2: Parser Tests (test/rebar3_opapi_parser_tests.erl)
Write tests **ONE BY ONE**:
1. Test `parse_file/2` with valid Erlang file
2. Test `extract_routes/1` with simple routes
3. Test route extraction with multiple HTTP methods
4. Test contract extraction (when we add contracts)
5. Test type extraction (when we add types)
6. Test error handling (file not found, parse errors)

#### Step 3: Builder Tests (test/rebar3_opapi_builder_tests.erl)
7. Test `build/2` with empty operations
8. Test `build/2` with single operation
9. Test `method_to_lowercase/1` with different inputs
10. Test path grouping in `build_paths/1`

#### Step 4: YAML Writer Tests (test/rebar3_opapi_yaml_writer_tests.erl)
11. Test `write/1` with simple map
12. Test `write/1` with nested structures
13. Test YAML format correctness

---

## 📁 File Structure

```
/Users/amar.c/workspace/openapi-erl/
├── src/
│   ├── rebar3_opapi.erl                 # Main plugin module
│   ├── rebar3_opapi_prv_extract.erl     # Extract provider (orchestrates)
│   ├── rebar3_opapi_parser.erl          # Parses Erlang files, extracts routes/contracts/types
│   ├── rebar3_opapi_builder.erl         # Builds OpenAPI document from operations
│   └── rebar3_opapi_yaml_writer.erl     # Writes OpenAPI doc to YAML
├── test/                                 # TO CREATE
│   ├── fixtures/                         # Test Erlang files
│   ├── rebar3_opapi_parser_tests.erl    # Parser tests
│   ├── rebar3_opapi_builder_tests.erl   # Builder tests
│   └── rebar3_opapi_yaml_writer_tests.erl # YAML tests
├── rebar.config                          # Plugin config
└── README.md                             # Documentation
```

---

## 🔑 Key Implementation Details

### How Parsing Works:
1. **`rebar3_opapi_parser:parse_file/2`** uses `epp:parse_file/2` to parse Erlang source
2. **`extract_routes/1`** finds `routes/0` function and extracts route maps from AST
3. **`extract_contracts/1`** finds `-opapi_contract({OperationId, ContractMap})` attributes
4. **`extract_types/1`** finds `-type` definitions for schema generation

### How Building Works:
1. **`convert_contracts_to_operations/3`** matches contracts with routes by operation_id
2. **`rebar3_opapi_builder:build/2`** creates OpenAPI 3.0.3 document structure
3. **`rebar3_opapi_yaml_writer:write/1`** converts map to YAML string

### Route Structure Expected:
```erlang
#{
    path => "/api/endpoint",
    allowed_methods => #{<<"POST">> => #{operation_id => 'operationName'}}
}
```

### Contract Structure Expected (not yet in handler):
```erlang
-opapi_contract({'operationName', #{
    summary => <<"Operation summary">>,
    description => <<"Detailed description">>,
    request_body => #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{...}
    },
    responses => #{
        <<"200">> => #{<<"description">> => <<"Success">>}
    }
}}).
```

---

## ⚙️ Development Workflow

### CRITICAL: Always Follow This Workflow
```bash
# 1. Make a small change
# 2. Compile
cd /Users/amar.c/workspace/openapi-erl
rebar3 compile

# 3. Check for undefined functions
rebar3 xref

# 4. Fix any errors before proceeding
# 5. Test manually (if applicable)
# 6. Commit
git add -A
git commit -m "Description"
git push

# 7. Test in main project
cd /Users/amar.c/workspace/gm_core/butler_server_release
rm -rf _build/default/plugins/rebar3_opapi
rebar3 opapi extract --handler <path> --output /tmp/test.yaml
```

### For Main Project (butler_server_release):
```bash
cd /Users/amar.c/workspace/gm_core/butler_server_release
make           # Compile
make xref      # Check undefined functions
```

---

## 📚 Erlang Best Practices (CRITICAL - ALWAYS FOLLOW)

### 1. ALWAYS Verify Functions Before Using
- ❌ **DON'T** assume function names/arities exist
- ✅ **DO** check official Erlang docs at `erlang.org/doc/man/`
- ✅ **DO** run `rebar3 xref` after every change

### 2. Common Erlang Modules for This Project:
- **`epp`** - Erlang preprocessor (handles `-include`, macros)
  - `epp:parse_file/2` - Parses file with includes
- **`erl_scan`** - Tokenizes Erlang source
  - `erl_scan:string/1` - Tokenizes string
- **`erl_parse`** - Parses tokens into AST
  - NO `parse_form_list/1` function (doesn't exist!)
- **`file`** - File operations
  - `file:read_file/1` - Reads file
- **`maps`** - Map operations
  - `maps:get/2`, `maps:get/3`, `maps:to_list/1`, etc.

### 3. Testing Functions Exist:
```erlang
%% Check if function is exported
erlang:function_exported(Module, Function, Arity).
```

### 4. Workflow:
- Small change → `rebar3 compile` → `rebar3 xref` → fix → repeat
- **NEVER** commit code with xref errors
- **NEVER** assume functions exist without verification

---

## 🐛 Debugging Tips

### If Plugin Fails:
```bash
# Get detailed error
DEBUG=1 rebar3 opapi extract --handler <path> --output /tmp/test.yaml

# Check stack trace for:
# - undefined function calls (use rebar3 xref)
# - badarg errors (type mismatches)
# - function_clause errors (pattern matching failures)
```

### If Tests Fail:
```bash
# Run with verbose output
rebar3 eunit --verbose

# Run specific test module
rebar3 eunit --module=rebar3_opapi_parser_tests

# Run specific test
rebar3 eunit --module=rebar3_opapi_parser_tests --test=parse_file_test
```

---

## 📋 Test Fixtures to Create

### 1. `test/fixtures/simple_handler.erl`
```erlang
-module(simple_handler).
-export([routes/0]).

routes() ->
    [
        #{
            path => "/api/test",
            allowed_methods => #{<<"GET">> => #{operation_id => 'getTest'}}
        },
        #{
            path => "/api/users",
            allowed_methods => #{<<"POST">> => #{operation_id => 'createUser'}}
        }
    ].
```

### 2. `test/fixtures/handler_with_contracts.erl`
```erlang
-module(handler_with_contracts).
-export([routes/0]).

-opapi_contract({'createUser', #{
    summary => <<"Create a new user">>,
    request_body => #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"name">> => #{<<"type">> => <<"string">>}
        }
    },
    responses => #{
        <<"201">> => #{<<"description">> => <<"User created">>}
    }
}}).

routes() ->
    [
        #{
            path => "/api/users",
            allowed_methods => #{<<"POST">> => #{operation_id => 'createUser'}}
        }
    ].
```

### 3. `test/fixtures/handler_with_types.erl`
```erlang
-module(handler_with_types).
-export([routes/0]).

-type user() :: #{
    name => binary(),
    email => binary(),
    age => integer()
}.

-type response() :: #{
    status => ok | error,
    data => user()
}.

routes() -> [].
```

---

## 🎯 Immediate Next Steps

### Step 1: Create Test Infrastructure
```bash
cd /Users/amar.c/workspace/openapi-erl
mkdir -p test/fixtures
```

### Step 2: Update rebar.config
Add to `rebar.config`:
```erlang
{profiles, [
    {test, [
        {deps, [
            {eunit, ".*", {git, "git://github.com/richcarl/eunit.git", {branch, "master"}}}
        ]}
    ]}
]}.
```

### Step 3: Create First Test File
Create `test/rebar3_opapi_parser_tests.erl` with ONE test:
```erlang
-module(rebar3_opapi_parser_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test parse_file/2 with a simple valid Erlang file
parse_file_simple_test() ->
    FilePath = "test/fixtures/simple_handler.erl",
    {ok, {Contracts, Types}} = rebar3_opapi_parser:parse_file(FilePath, []),
    ?assertEqual([], Contracts),  % No contracts expected
    ?assertEqual([], Types).      % No types expected
```

### Step 4: Run Test
```bash
rebar3 eunit --module=rebar3_opapi_parser_tests
```

### Step 5: Fix Issues, Add Next Test
Iterate: write test → run → fix → commit → repeat

---

## 📖 Reference Documentation

- **Erlang epp:** https://www.erlang.org/doc/man/epp.html
- **Erlang erl_parse:** https://www.erlang.org/doc/man/erl_parse.html
- **Erlang erl_scan:** https://www.erlang.org/doc/man/erl_scan.html
- **EUnit Testing:** https://www.erlang.org/doc/apps/eunit/chapter.html
- **Rebar3 Testing:** https://rebar3.org/docs/testing/eunit/
- **OpenAPI 3.0.x:** https://swagger.io/specification/

---

## ⚠️ Important Reminders

1. **Write tests ONE BY ONE** - Don't write all tests at once
2. **Run `rebar3 xref` after every change** - Catches undefined functions
3. **Verify functions exist** in Erlang docs before using them
4. **Test incrementally** - Small change → test → fix → commit
5. **Follow .cursorrules** - Ask for approval before creating docs
6. **Don't proceed to Phase 3** without completing Phase 2 tests

---

## 🚀 Starting Prompt

**When you open the workspace, say:**

> "I'm continuing work on the OpenAPI Erlang plugin. I've read the CONTINUE_WORK_PROMPT.md for full context.
>
> Current status: Phase 1 (plugin structure) is complete and working. Plugin successfully extracts 9 routes.
>
> Next task: Implement Phase 2 (Unit Tests). I need to:
> 1. Set up test infrastructure
> 2. Create test fixtures
> 3. Write parser tests ONE BY ONE
> 4. Write builder tests ONE BY ONE
> 5. Write YAML writer tests ONE BY ONE
>
> Please help me start with Step 1: Create test infrastructure and the first test fixture (simple_handler.erl).
>
> Workspace: `/Users/amar.c/workspace/openapi-erl`
>
> Follow the Erlang best practices: compile → xref → test → commit after each change."

---

## 📄 Additional Context Files

- **Implementation Status:** `/Users/amar.c/workspace/dev-setup/workspace/open-api-doc-erlang/IMPLEMENTATION_STATUS.md`
- **Original Plan:** `/Users/amar.c/workspace/dev-setup/workspace/open-api-doc-erlang/IMPLEMENTATION_PLAN.md`

---

**End of Context Prompt - Copy everything above**

