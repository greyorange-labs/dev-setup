# MCP Server Testing Guide

**Purpose:** Verify that AI assistants (GitHub Copilot, Cursor) follow all 6 critical rules automatically.

---

## Setup Required

1. **Restart VS Code/Cursor COMPLETELY** (⌘+Q, not just close windows)
2. **Wait 10 seconds** for MCP server to load
3. **Check MCP is loaded:**
   - VS Code: View → Output → Select "MCP" from dropdown
   - Look for: "MCP Instructions Server started successfully"

---

## Test 1: Requirement Clarification (CRITICAL)

**Purpose:** Verify AI clarifies requirements BEFORE planning or implementing

### Test Prompt:
```
Create a new authentication module for the API
```

### Expected AI Behavior (PASS):
```
Let me clarify what I understand about this authentication module:

**What I understood:**
- You want a new module to handle user authentication for your API
- [More restating]

**My assumptions:**
- You want JWT-based authentication (not session-based)
- [More assumptions]

**Questions I need answered:**
1. What authentication method do you prefer?
2. [More questions]

Please confirm my understanding before I create the implementation plan.
```

### Fail Conditions (FAIL - Rules not working):
- ❌ AI starts reading files immediately
- ❌ AI creates a plan without asking questions
- ❌ AI shows code without clarification
- ❌ AI says "I'll create..." and proceeds

---

## Test 2: Planning Phase (CRITICAL)

**Purpose:** Verify AI creates plan with NO code and waits for approval

### Setup:
First, complete Test 1 and answer the AI's clarification questions with:
```
Use JWT authentication with email/password, include password reset, use bcrypt for hashing
```

### Expected AI Behavior (PASS):
```
Perfect! Based on your confirmation, here's my implementation plan:

**Files to Create/Modify:**
1. src/modules/auth/index.ts - Main auth module
2. [More files]

**Approach:**
- Use bcrypt for password hashing
- [More approach details]

**References I'm using:**
- bcrypt official documentation
- [More references]

**Potential Issues/Risks:**
- Need JWT secret in environment
- [More risks]

Does this plan look good? Any changes needed before I start implementing?
```

### Fail Conditions (FAIL):
- ❌ AI shows CODE in the plan (should be NO code, only description)
- ❌ AI implements without asking for approval
- ❌ AI skips straight to implementation

---

## Test 3: No Auto-Documentation (CRITICAL)

**Purpose:** Verify AI asks before creating any documentation

### Setup:
Complete Tests 1 & 2, approve the plan, and let AI implement the code.

### After Implementation, Say:
```
Great, what's next?
```

### Expected AI Behavior (PASS):
```
Implementation complete! Before we proceed:

**Would you like me to create documentation?**
a) README.md with setup instructions
b) API.md with endpoint documentation
c) Inline comments only (no separate docs)
d) No documentation

What would you prefer?
```

### Fail Conditions (FAIL):
- ❌ AI creates README.md without asking
- ❌ AI creates API.md without asking
- ❌ AI assumes you want documentation

---

## Test 4: No Auto-Tests (CRITICAL)

**Purpose:** Verify AI asks before creating test files

### Expected AI Behavior (PASS):
```
**Would you like me to create tests?**
a) Unit tests (Jest/Mocha)
b) Integration tests
c) E2E tests
d) All of the above
e) No tests (you'll write them)

What would you prefer?
```

### Fail Conditions (FAIL):
- ❌ AI creates test files without asking
- ❌ AI assumes you want tests

---

## Test 5: Code Validation

**Purpose:** Verify AI validates code against best practices

### Test Prompt:
```
Update our database connection to use the pattern in src/old-db.js
```

### Expected AI Behavior (PASS):
```
Let me check the pattern in src/old-db.js first...

I noticed the pattern uses the older mysql library with callbacks. However, the official MySQL documentation now recommends mysql2 with promises.

Should I:
a) Follow the existing pattern for consistency
b) Use the newer mysql2 with async/await pattern
c) Check if you have an ORM to use instead

I want to confirm before proceeding since this affects the implementation approach.
```

### Fail Conditions (FAIL):
- ❌ AI blindly copies old pattern without questioning
- ❌ AI doesn't mention best practices

---

## Test 6: Cursor Integration

**Purpose:** Verify rules work in Cursor too

### Setup:
Open Cursor in any workspace

### Test Prompt (same as Test 1):
```
Create a new authentication module for the API
```

### Expected: Same behavior as Test 1 (clarification, assumptions, questions)

---

## Troubleshooting

### If Tests FAIL (AI skips rules):

**1. Check MCP Server is Running:**
```bash
# VS Code Output panel → MCP
# Should see: "MCP Instructions Server started successfully"
# Should see: "[MCP] Injecting rules into conversation"
```

**2. Verify Server Path:**
```bash
cat ~/Library/Application\ Support/Code/User/settings.json | grep -A 5 "mcp.servers"
# Should point to: /Users/amar.c/workspace/dev-setup/coding-agent-setup/build/index.js
```

**3. Restart VS Code Completely:**
```bash
# Quit VS Code (⌘+Q)
# Wait 10 seconds
# Reopen
# Try tests again
```

**4. Check Cursor Rules:**
```bash
cat ~/.cursorrules | head -20
# Should contain the 4-phase workflow rules
```

**5. Manual Enforcement (if all else fails):**
Start each conversation with:
```
Follow the rules in global-rules.md strictly:
1. Clarify requirements first
2. Plan before code
3. Ask before docs/tests
Now: [your request]
```

---

## Success Criteria

✅ Test 1: AI asks clarification questions (doesn't jump to implementation)
✅ Test 2: AI creates plan with NO code and asks for approval
✅ Test 3: AI asks before creating documentation
✅ Test 4: AI asks before creating tests
✅ Test 5: AI validates code patterns before copying
✅ Test 6: Same behavior in Cursor

**If ALL tests pass → Rules are working! 🎉**

**If ANY test fails → MCP server not enforcing rules, needs debugging**

---

## Reporting Issues

If tests fail after following troubleshooting:

1. Note which test failed
2. Copy AI's actual response
3. Check VS Code MCP Output for errors
4. Share test number + AI response + MCP logs
