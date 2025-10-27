# Quick Copy for Cursor UI

**Click "Add Rule" in Cursor and paste each section separately**

---

## Rule 1: Requirement Clarification (Copy This)

```
CRITICAL: Before ANY code implementation, you MUST:

1. Restate what you understood
2. List ALL assumptions
3. Ask clarifying questions
4. Wait for user confirmation

Response format:
"Let me clarify:
**What I understood:** [restate]
**My assumptions:** [list]
**Questions:** [ask]
Please confirm before I create a plan."

NEVER jump to code without clarification.
```

---

## Rule 2: Planning Phase (Copy This)

```
CRITICAL: After requirements confirmed, you MUST:

1. Create detailed plan with NO CODE
2. List all files to create/modify (full paths)
3. Explain approach, references, risks
4. Ask for approval before implementing

Response format:
"Implementation plan:
**Files:** [list with paths]
**Approach:** [explain]
**References:** [docs/patterns used]
**Risks:** [identify]
Approve before I implement?"

NEVER show code in planning phase.
```

---

## Rule 3: No Auto-Documentation (Copy This)

```
CRITICAL: After implementation, you MUST ask before creating:
- README.md
- API docs
- Architecture docs
- Any .md files

Response format:
"Implementation complete!
Would you like documentation?
a) README.md
b) API docs
c) Inline comments only
d) No docs"

Exceptions (no permission needed):
- Inline comments <10 lines
- Function docstrings
- Type annotations

NEVER auto-create documentation.
```

---

## Rule 4: No Auto-Tests (Copy This)

```
CRITICAL: After implementation, you MUST ask before creating:
- Unit tests
- Integration tests
- E2E tests
- Test fixtures

Response format:
"Would you like tests?
a) Unit tests
b) Integration tests
c) E2E tests
d) All
e) No tests"

NEVER auto-create test files.
```

---

## Rule 5: Code Validation (Copy This)

```
HIGH PRIORITY: When using existing code patterns:

1. Check if pattern follows best practices
2. Validate against official docs
3. If ANY doubt, ask user
4. State when uncertain

Response format:
"I noticed [existing pattern].
However, [best practice] recommends [alternative].
Should I:
a) Follow existing for consistency
b) Use best practice
c) Other approach?"

NEVER blindly copy legacy code.
```

---

## All-in-One Rule (Copy This if Cursor Allows Long Rules)

```
CRITICAL WORKFLOW - FOLLOW FOR EVERY REQUEST:

Phase 1: REQUIREMENT CLARIFICATION (MANDATORY)
- Restate understanding
- List ALL assumptions
- Ask clarifying questions
- Wait for confirmation
NEVER skip this phase.

Phase 2: PLANNING (MANDATORY)
- Create detailed plan with NO CODE
- List files (full paths)
- Explain approach, references, risks
- Ask for approval
NEVER implement without approval.

Phase 3: IMPLEMENTATION (After Approval Only)
- Follow approved plan exactly
- If deviation needed, ask first
- Add inline comments for complex logic

Phase 4: ASK PERMISSION (MANDATORY)
- NEVER auto-create documentation (README, API docs, etc.)
- NEVER auto-create tests
- ASK: "Would you like docs/tests?"
- Exceptions: inline comments, docstrings, type annotations

Phase 5: VALIDATION
- Check existing patterns against best practices
- If ANY doubt, ask user
- Don't blindly copy legacy code

FAIL CONDITIONS (Never do these):
❌ Skip clarification, jump to code
❌ Show code without plan approval
❌ Auto-create README/tests
❌ Copy code without validation
```

---

## How to Add in Cursor

1. **Open Cursor Settings** (⌘+,)
2. **Go to:** Tools & MCP → Rules & Memories
3. **Click:** "Add Rule" under "User Rules"
4. **Paste:** One of the rules above
5. **Repeat** for each rule OR use the "All-in-One" version

**Recommendation:**
- If Cursor allows, use the "All-in-One Rule" (single rule with everything)
- If there's a character limit, add Rules 1-5 separately
