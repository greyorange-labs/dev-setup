## Requirement:
I want to build a tool that can help me ->
1. Generate/Modify/Update Erlang code (Rest API code written using cowboy) if I provide an Open API doc (yaml file).
2. Generate/Modify/update Open API Doc from given erlang code (Rest API code written using cowboy).
3. This tool should be easy to integratable to my erlang source project /Users/amar.c/workspace/gm_core/butler_server_develop

## Details: My erlang project /Users/amar.c/workspace/gm_core/butler_server_develop
- It is a multi app project
- Most of the app's have `*.handler.erl` modules which implements `trails()` functions
    - bsh_http_handler -> /Users/amar.c/workspace/gm_core/butler_server_develop/apps/butler_shared/src/interfaces/in/bsh_http_handler.erl
    - butler_mhs_api_handler -> /Users/amar.c/workspace/gm_core/butler_server_develop/apps/mhs/src/interfaces/in/butler_mhs_api_handler.erl
    - butler_base_api_handler -> /Users/amar.c/workspace/gm_core/butler_server_develop/apps/butler_base/src/interfaces/in/butler_base_api_handler.erl
    - butler_web_api_handler -> /Users/amar.c/workspace/gm_core/butler_server_develop/src/web/butler_web_api_handler.erl
    - pick_api_handler -> /Users/amar.c/workspace/gm_core/butler_server_develop/apps/pick/src/interfaces/out/pick_api_handler.erl
    - butler_web_server -> /Users/amar.c/workspace/gm_core/butler_server_develop/apps/butler_base/src/critical/web/butler_web_server.erl
    - tagged_product_api_handler -> /Users/amar.c/workspace/gm_core/butler_server_develop/apps/storage/src/interface/tagged_product_api_handler.erl
    - storage_api_handler -> /Users/amar.c/workspace/gm_core/butler_server_develop/apps/storage/src/interface/storage_api_handler.erl
    - inventory_api_handler -> /Users/amar.c/workspace/gm_core/butler_server_develop/apps/storage/src/interface/inventory_api_handler.erl
    - test_storage_api_handler -> /Users/amar.c/workspace/gm_core/butler_server_develop/apps/storage/test/ct/test_storage_api_handler.erl
    - ims_microsim_api_handler -> /Users/amar.c/workspace/gm_core/butler_server_develop/apps/gmr_common/src/simulator/ims_microsim_api_handler.erl
    - gmr_layout_api_handler -> /Users/amar.c/workspace/gm_core/butler_server_develop/apps/gmr_common/src/interface/http/in/gmr_layout_api_handler.erl
    - butler_pps_api_handler -> /Users/amar.c/workspace/gm_core/butler_server_develop/apps/gmr_common/src/interface/http/in/butler_pps_api_handler.erl
    - pf_pps_api_handler -> /Users/amar.c/workspace/gm_core/butler_server_develop/apps/gmr_common/src/interface/http/in/pf_pps_api_handler.erl
    - ims_api_handler -> /Users/amar.c/workspace/gm_core/butler_server_develop/apps/gmr_common/src/interface/http/in/ims_api_handler.erl
    - station_http_handler -> /Users/amar.c/workspace/gm_core/butler_server_develop/apps/station/src/interface/in/station_http_handler.erl
    - audit_http_handler -> /Users/amar.c/workspace/gm_core/butler_server_develop/apps/audit/src/interface/in/audit_http_handler.erl
    - rlayout_map_api_handler -> /Users/amar.c/workspace/gm_core/butler_server_develop/apps/rlayout/src/rlayout_map_api_handler.erl
    - put_http_handler -> /Users/amar.c/workspace/gm_core/butler_server_develop/apps/put/src/interface/put_http_handler.erl
    - maop_api_handler -> /Users/amar.c/workspace/gm_core/butler_server_develop/apps/mhs/src/interfaces/in/maop_api_handler.erl
    - obts_web_api_handler -> /Users/amar.c/workspace/gm_core/butler_server_develop/apps/mhs/src/interfaces/in/obts_web_api_handler.erl
- Web server process:
    - module: Users/amar.c/workspace/gm_core/butler_server_develop/apps/butler_base/src/critical/web/cowboy_routes_manager.erl
    - It starts the web server with empty routes,
    - Then each application calls `start_handlers/1` function of this module to register its routes.
    - This process will update the dispatch based on the trails() of these handlers.
- Each handler is written based on cowboy_rest (you can refer the code of above handlers) for more details
- There is jesse validation also, imposed via decorator on lowest level api handle function (this is only for put/post requests and only for requestBody) but this is there for only few APIs only.

## My research:
There is a tool https://github.com/OpenAPITools/openapi-generator that has functionality to generate a erlang web server application based on given OpenAPI doc.
Limitation:
 - This tool can't be integrated directly with my project
   - As I only have a web server, so I just need functionality that this new tool modifies/updates the existing handler's code whenever someone wants to add/modify API.
   - add handler if a new handler needs to added (user can decide using the command line argument -> new handler / location of handler file  / existing handler etc)
   - router module is good to have, so we can have this.
 - this doesn't have functionlaity to generate erlang code from given API doc
   - I need this capabilty also.
   - both erlang -> open-api-doc  and open-api-doc -> erlang should have similar logic/understanding/implementation it should compliment each (there is cowboy_swagger: https://github.com/inaka/cowboy_swagger that generates doc from code, but it uses trails'metadata function, which is not used in open-api-generator, so can't use that)

## Additional requirement
- It should be easy to maintain code.
- It should be easy to refactor current handlers/code from my erlang project to make it compatible/align with this new tool
- Most of the existing library/tool's functionality to be used (like from open-api-generator / cowboy swagger or any relevant open-source repo)
- Easy to plgin in my erlang project.
- Proper guideline.
- bsh_http_handler to be refactored with this new tool's guide/framework

Please do share any other clarification that you need, before sharding me the initial plan to implement this tool. Once we have final requirement freezed, then you can implement this tool (may be rebar3 plugin | other command line tool (installed with some package) whichever is best).

## My erlang project's end goal is
- I want that there should be automated way to have documentation of all of my erlang APIs.
- Whenever a new API needs to added
  - One will write an open api doc, get it reviewed from PMs/Archetects/relevant teams
  - Once contract freezed and reviewed and accepted, dev will use this tool to generate all generic (except end business logic) code using this tool in existing handlers/router or new one.
- For existing APIs
  - Devs will slowely refactor code (with AI help --> a prompt is required) OR manualy based on instrctions/guide of this framework.
- There will be integrated CI check that will generate API docs from code and verify it with Human written docs (hosted in some other repo)
  - In this CI check, I will use the code to doc functionality.
  - ALos, dev can also use this functionality.

