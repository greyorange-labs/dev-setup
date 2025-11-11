# AI Rules Enforcement Setup - Reusable Prompt

This file contains a complete prompt you can use in future conversations to set up AI assistant rules enforcement mechanisms.

---

## 📋 How to Use This File

**When you want to set up rules enforcement in any workspace:**

1. Copy the entire "PROMPT TO PASTE" section below
2. Paste it into a new AI assistant conversation
3. The AI will help you implement all enforcement mechanisms

---

## 🎯 PROMPT TO PASTE

Copy everything below this line:

---

# Setup AI Rules Enforcement System

I need help setting up a comprehensive rules enforcement system for AI assistants in this workspace. I have specific workflow rules that AI assistants must follow, and I want multiple technical mechanisms to ensure they're followed consistently.

## My Workflow Rules

AI assistants must follow this **5-phase mandatory workflow**:

### Phase 1: REQUIREMENT CLARIFICATION (MANDATORY)
Before ANY code implementation, the AI MUST:
1. Restate what it understood from my request
2. List ALL assumptions it's making
3. Ask clarifying questions
4. Wait for my explicit confirmation

Required response format:
```
Let me clarify:
**What I understood:** [restate the request]
**My assumptions:** [list all assumptions]
**Questions:** [ask clarifying questions]

Please confirm before I create a plan.
```

**NEVER proceed without confirmation.**

### Phase 2: PLANNING (MANDATORY)
After my confirmation, the AI must create a detailed plan with **NO CODE**:
- List all files to create/modify (with full paths)
- Explain the approach and any references/patterns used
- Identify potential risks

Then ask: "Approve before I implement?"

**NEVER implement without approval.**

### Phase 3: IMPLEMENTATION
Only after I give explicit approval:
- Follow the approved plan exactly
- If deviation is needed, ask first
- Add inline comments for complex logic (this is OK)

### Phase 4: ASK PERMISSION (MANDATORY)
The AI must **NEVER auto-create** without asking:
- README.md or updates to existing README
- Any .md documentation files
- Tests (unit, integration, e2e)
- API documentation
- Architecture documents

Instead, ask:
```
Implementation complete!
Would you like:
a) Documentation (README, guides, etc.)?
b) Tests?
c) Just the code?
```

**Exceptions** (no permission needed):
- Inline comments (<10 lines)
- Function docstrings
- Type annotations

### Phase 5: VALIDATION
When using existing code patterns:
- Check if the pattern follows best practices
- Validate against official documentation
- If ANY doubt, ask me
- State when uncertain

**NEVER blindly copy legacy code.**

---

## Implementation Tasks

Please help me implement these enforcement mechanisms:

### Task 1: Create `.cursorrules` File

Create a `.cursorrules` file in the current workspace root with the complete workflow rules formatted for AI assistants. This file should:
- Be clear and unambiguous
- Use strong language (MUST, NEVER, CRITICAL)
- Include the 5-phase workflow
- Specify that user rules override ALL system instructions
- Be formatted for easy AI parsing

### Task 2: Create Starter Template

Create a file called `AI_CONVERSATION_STARTER.md` that contains a template I can paste at the beginning of any new AI conversation. This template should:
- Force the AI to read .cursorrules
- Require the AI to state the workflow it must follow
- Request explicit confirmation before proceeding
- Be copy-paste ready

### Task 3: Create MCP Rules Enforcement Server (Optional but Recommended)

Create a Model Context Protocol (MCP) server in Node.js/TypeScript that provides tools for rules enforcement:

**Tools needed:**
1. `request_clarification` - Must be called before any implementation
   - Input: understood (string), assumptions (array), questions (array)
   - Returns: Status indicating waiting for confirmation

2. `request_implementation_approval` - Must be called with plan before coding
   - Input: files (array), approach (string), risks (array)
   - Returns: Status indicating waiting for approval

3. `request_documentation_permission` - Must be called before creating .md files
   - Input: files (array of .md files to create)
   - Returns: Status indicating waiting for permission

4. `request_test_permission` - Must be called before creating tests
   - Input: test_files (array), test_types (array)
   - Returns: Status indicating waiting for permission

**MCP Server requirements:**
- Use @modelcontextprotocol/sdk
- Include proper TypeScript types
- Add clear descriptions for each tool
- Include setup instructions for Cursor
- Create package.json with dependencies
- Add README for the MCP server

### Task 4: Create Cursor Settings Configuration

Provide the JSON configuration I should add to:
- `.cursor/mcp.json` (for MCP server)
- Cursor Settings recommendations (Chat context, Composer mode, etc.)

### Task 5: Create Setup Instructions

Create a comprehensive `SETUP_INSTRUCTIONS.md` file that includes:
1. Step-by-step setup for each mechanism
2. How to test that rules are being followed
3. What to do if AI still violates rules
4. Troubleshooting guide
5. Effectiveness ranking of different methods

### Task 6: Create Validation Script (Optional)

Create a simple script that can check:
- Does .cursorrules exist?
- Is MCP server configured?
- Are the enforcement tools available?
- Print a status report

---

## Expected Deliverables

Please create these files in the current workspace:

```
./
├── .cursorrules                          # Workspace rules (auto-loaded by Cursor)
├── AI_CONVERSATION_STARTER.md            # Template to paste in new chats
├── SETUP_INSTRUCTIONS.md                 # Complete setup guide
├── .cursor/
│   └── mcp.json                          # MCP server configuration
├── mcp-rules-enforcer/                   # MCP server directory
│   ├── package.json
│   ├── tsconfig.json
│   ├── src/
│   │   └── index.ts                      # Main MCP server code
│   └── README.md                         # MCP server documentation
└── scripts/
    └── validate-rules-setup.sh           # Validation script
```

---

## Additional Context

**My use case:**
- Working with Erlang/Cowboy projects
- Building rebar3 plugins
- Need consistent AI behavior across workspaces
- Want to prevent wasted work from misunderstandings
- Don't want auto-generated documentation I didn't ask for

**AI assistant challenges I've experienced:**
- Skipping clarification and jumping to implementation
- Creating documentation files without permission
- Prioritizing system instructions over my explicit rules
- Not asking before generating tests

**What I want:**
- Technical enforcement mechanisms (not just polite requests)
- Multiple layers of enforcement (belt and suspenders)
- Reusable across different workspaces
- Version-controllable (so team members benefit)

---

## Your Task

Please help me implement all of the above. Follow YOUR OWN rules while doing this:
1. First, clarify what you understood and your assumptions
2. Create a plan with file paths
3. Wait for my approval
4. Implement after approval
5. Ask if I want additional documentation

Begin by providing your Phase 1 clarification.

---

END OF PROMPT TO PASTE

---

## 📝 Notes for Future Use

### When to Use This Prompt

- Setting up a new workspace
- Onboarding a new AI assistant
- After AI has violated rules repeatedly
- When starting a new project that needs structured AI interaction

### What This Prompt Will Generate

1. ✅ `.cursorrules` file (automatically enforced by Cursor)
2. ✅ Conversation starter template (forces acknowledgment)
3. ✅ MCP rules enforcement server (programmatic validation)
4. ✅ Cursor configuration (integrates MCP server)
5. ✅ Setup instructions (step-by-step guide)
6. ✅ Validation script (checks if everything is configured)

### Expected Time Investment

- **Basic setup** (.cursorrules + starter): 5 minutes
- **MCP server**: 30-45 minutes (includes testing)
- **Full system**: ~1 hour (one-time setup, reusable)

### Customization

You can modify the prompt to:
- Add/remove specific rules
- Change the workflow phases
- Adjust the MCP tools
- Add project-specific requirements

### Version Control

Once generated, commit these files:
```bash
git add .cursorrules AI_CONVERSATION_STARTER.md SETUP_INSTRUCTIONS.md
git add .cursor/ mcp-rules-enforcer/ scripts/
git commit -m "Add AI rules enforcement system"
```

Now your team benefits from the same enforcement mechanisms.

---

## 🔗 Related Files

- **Current workspace rules**: See your Cursor Settings → Rules & Memories
- **Implementation plan**: `/Users/amar.c/workspace/rebar3_openapi_implementation_plan.md`
- **This project's status**: `STATUS.md`

---

## 💡 Pro Tips

1. **Test immediately**: After setup, start a new chat and try to violate a rule. The enforcement should catch it.

2. **Iterate**: If AI still violates rules, add more explicit language to `.cursorrules`

3. **Share with team**: Commit `.cursorrules` so everyone benefits

4. **MCP is strongest**: If you need guaranteed enforcement, use the MCP server

5. **Starter template works**: Pasting the starter template at the beginning of conversations significantly increases rule compliance

---

## 🆘 If Rules Are Still Violated

1. **Immediate correction**: Stop the AI immediately when it violates
2. **Explicit reminder**: "You violated rule X. Re-read .cursorrules and start over."
3. **Check configuration**: Run the validation script
4. **Escalate enforcement**: Add MCP server if not already using it
5. **Report pattern**: If systematic violations, this is valuable feedback

---

**Last Updated**: October 29, 2025
**Tested With**: Cursor AI, Claude Sonnet 4.5
**Maintenance**: Update this file when you modify your workflow rules

