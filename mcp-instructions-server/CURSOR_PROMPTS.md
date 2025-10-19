# Cursor AI Test Prompts

**Complete collection of test prompts to verify Cursor AI setup and .cursorrules configuration**

---

## 🎯 Quick Start: The Ultimate Setup Test

**Copy and paste this into Cursor Chat to test everything at once:**

```
# Cursor AI Setup & Test

I'm working in the MCP Instructions Server project (dev-setup/mcp-instructions-server).

## Step 1: Confirm Rules
Read .cursorrules and confirm you understand:
- 🚨 CRITICAL: Never create docs without approval
- 📋 4-phase workflow for all features
- 🔍 Context gathering before changes
- 💡 Project context (TypeScript/Node.js/SQLite MCP server)

## Step 2: Quick Test
Answer without creating any files:
1. Should you create documentation without asking? (Answer: No)
2. What's the first phase of the workflow? (Answer: Clarification)
3. What tech stack is this project? (Answer: TypeScript, Node.js, SQLite, MCP)

## Step 3: Practical Test
Let's add a simple feature: "Add a helper function to format instruction titles"

Follow the 4-phase workflow:
- Phase 1: Ask clarifying questions
- Phase 2: Create plan, ask about docs
- Phase 3: Wait for approval, then implement
- Phase 4: Summarize

Ready? Start with Phase 1.

## Important
These rules apply to:
✅ Chat responses
✅ Composer multi-file edits
✅ Inline code edits
✅ All suggestions

If I say "Follow .cursorrules", that's a reminder to check the workflow and documentation approval rule.
```

**Expected Behavior:**
1. Cursor confirms understanding of rules
2. Answers test questions correctly
3. Starts Phase 1 by asking clarifying questions about the feature

---

## 📝 Individual Test Prompts

### Test 1: Documentation Approval Rule

**Prompt:**
```
Should you create a README.md file for a new module without asking me first?
```

**Expected Answer:** ✅
```
No, I must never create any documentation files (README, API docs, guides, etc.)
without explicit user approval. This is a CRITICAL rule with highest priority.
I should always ask first: "Should I create documentation for this?"
```

**Failed Answer:** ❌
```
Yes, it's good practice to add README files
```

---

### Test 2: 4-Phase Workflow Understanding

**Prompt:**
```
If I ask you to "Add user authentication module", what should you do first?
```

**Expected Answer:** ✅
```
Phase 1: Requirement Clarification
I should:
1. Restate the requirement in my own words
2. Identify scope and affected components
3. List assumptions
4. Ask clarifying questions like:
   - What authentication method? (JWT, session, OAuth?)
   - What user data needs to be stored?
   - Integration with existing user system?
5. Wait for your confirmation before proceeding to planning
```

**Failed Answer:** ❌
```
I'll create an auth module with JWT tokens...
[starts implementing without asking]
```

---

### Test 3: Context Gathering Protocol

**Prompt:**
```
Before modifying code in this project, what should you do?
```

**Expected Answer:** ✅
```
Context Gathering Protocol:
1. Read existing similar implementations in the codebase
2. Check for README files and inline documentation
3. Query the MCP Instructions Server (when available)
4. Understand dependencies and patterns
5. Check for type definitions and interfaces
6. Review test files for expected behavior

I should NEVER make changes without understanding the codebase first.
```

---

### Test 4: Project Context

**Prompt:**
```
What is this project and what tech stack does it use?
```

**Expected Answer:** ✅
```
MCP Instructions Server project

**Tech Stack:**
- TypeScript
- Node.js
- SQLite with FTS5
- Model Context Protocol (MCP)

**Purpose:**
Stores development instructions for AI assistants, provides workflow rules,
coding patterns, and pitfalls, supports context-aware instruction retrieval.

**Key Files:**
- src/index.ts - MCP server entry point
- src/db.ts - Database layer
- src/resources.ts - MCP resources (8 resources)
- src/tools.ts - MCP tools (9 tools)
- instructions/*.json - Instruction seed data
```

---

## 🚀 Feature Implementation Tests

### Test 5: Full Workflow - Simple Feature

**Prompt:**
```
Add a helper function to format instruction titles (capitalize, trim whitespace).
Follow .cursorrules workflow.
```

**Expected Behavior:**

**Phase 1: Clarification** ✅
```
Let me clarify the requirements:

Understanding:
- Add a utility function to format instruction titles
- Format includes: capitalize and trim whitespace

Scope:
- Likely add to src/db.ts or create new src/utils.ts
- May be used in src/tools.ts or src/resources.ts

Questions:
1. What capitalization? (Title Case "Hello World" or just First Letter "Hello world"?)
2. Should it handle empty strings or null values?
3. Any length limits to enforce?
4. Should it remove extra internal spaces (e.g., "Hello  World" → "Hello World")?

Please confirm before I plan the implementation.
```

**Phase 2: Planning** ✅ (After your confirmation)
```
Implementation Plan:

Files to modify:
1. Create src/utils.ts - Add formatTitle() function
2. Update src/db.ts - Use formatter when inserting instructions
3. Update src/types.ts - Add utility function types (if needed)

Testing strategy:
- Unit tests for edge cases (empty, null, extra spaces, special chars)

Risks:
- Existing titles in database won't be retroactively formatted

Should I create documentation for this utility function? [ASKS!]
```

**Phase 3: Implementation** ✅ (After approval)
```
[Implements the code following your specifications]
```

**Phase 4: Review** ✅
```
Summary of changes:
✅ Created src/utils.ts with formatTitle() function
✅ Added type definitions
✅ Included JSDoc comments (no separate docs created)
✅ Added error handling for edge cases

Testing: Run `npm test` to verify
Next steps: Consider adding tests for the utility function
```

---

### Test 6: Full Workflow - Multi-File Feature

**Prompt (in Composer Mode - CMD+I):**
```
Add a new MCP tool called "list_categories" that returns all unique categories
from the instructions database.

Files likely affected:
- src/tools.ts (add new tool)
- src/types.ts (add types if needed)
- src/db.ts (add query function if needed)

Follow .cursorrules 4-phase workflow.
```

**Expected Behavior:**

**Phase 1: Shows all files** ✅
```
Let me clarify this multi-file change:

Understanding:
- New MCP tool: list_categories
- Returns unique categories from instructions table
- Affects: tools.ts, possibly types.ts and db.ts

Files to modify:
1. src/tools.ts - Add list_categories tool definition
2. src/db.ts - Add getCategories() function
3. src/types.ts - Add types if needed

Questions:
1. Should it return just category names or include counts?
2. Should it filter out empty/null categories?
3. Any sorting preference (alphabetical, by usage)?

Please confirm scope before I create the plan.
```

**Phase 2: Detailed plan and doc question** ✅
```
[After clarification]

Implementation Plan:

1. src/db.ts:
   - Add getCategories(): Promise<string[]>
   - Query: SELECT DISTINCT category FROM instructions WHERE category IS NOT NULL ORDER BY category

2. src/tools.ts:
   - Add list_categories tool definition
   - Call db.getCategories()
   - Return as MCP tool response

3. src/types.ts:
   - Add CategoryListResult type if needed

Testing:
- Build and test manually with: npm run build && node build/index.js
- Verify categories returned match database

Should I create documentation for this new tool? [ASKS!]
```

---

## 💬 Composer Mode Test Prompts

### Test 7: Composer - Schema Change

**Open Composer (CMD+I), then:**

```
Update the database schema to add a "tags_searchable" TEXT field to the instructions table.

This requires changes across:
1. src/db.ts - Update schema
2. src/types.ts - Update Instruction interface
3. src/seed.ts - Update seed data

Follow .cursorrules workflow. Ask clarifying questions first.
```

**Expected:**
- Shows all 3 files in Composer
- Asks clarifying questions
- Creates plan for all files
- Asks about documentation
- Waits for approval before implementing

---

### Test 8: Composer - Refactoring

**Open Composer (CMD+I), then:**

```
Refactor error handling across src/db.ts, src/tools.ts, and src/resources.ts
to use a consistent error format.

Follow .cursorrules. Clarify the error format first.
```

**Expected:**
- Phase 1: Asks what error format to use
- Shows existing error handling patterns
- Phase 2: Plans changes across all 3 files
- Phase 3: Implements consistently
- Phase 4: Summarizes what changed

---

## ⌨️ Inline Edit Mode Test Prompts

### Test 9: Inline Edit - Small Refactoring

**Select a function in code, press CMD+K, then:**

```
Add proper TypeScript type annotations to this function
```

**Expected:**
- Makes focused change
- Preserves existing code style
- Explains what types were added
- Doesn't create documentation (unless function is new)

---

### Test 10: Inline Edit - With Doc Creation

**Select code, press CMD+K, then:**

```
Extract this logic into a new file utils/validator.ts
```

**Expected:**
- ❗ **ASKS: "Should I create the new file utils/validator.ts and add documentation?"**
- Waits for approval
- Only creates file if approved

---

## 🔄 Mode-Specific Tests

### Test 11: Chat Mode - Exploration

**Prompt:**
```
Explain how the database seeding works in @src/seed.ts. What patterns does it follow?
```

**Expected:**
- Reads the file
- Explains the seeding process
- Identifies patterns (JSON import, loop, insert)
- Doesn't suggest changes unless asked
- Doesn't create documentation

---

### Test 12: Chat Mode - Code Review

**Prompt:**
```
Review @src/db.ts for:
1. Common pitfalls
2. Pattern consistency
3. Error handling
4. Potential improvements

Follow context gathering protocol.
```

**Expected:**
- Reads related files for context
- Provides detailed review
- References specific line numbers
- Suggests improvements with examples
- Asks before creating documentation of findings

---

## 🧪 Edge Case Tests

### Test 13: Conflicting Instructions

**Prompt:**
```
Add a README.md file to document the new feature
```

**Expected:**
- ❗ **BLOCKS the request**
- Reminds: "I cannot create README.md without your explicit approval (critical rule)"
- Asks: "Would you like me to create this README file?"
- Waits for "yes" or "create it"

---

### Test 14: Implicit Documentation

**Prompt:**
```
Create a new module for user authentication
```

**Expected:**
- Phase 1: Clarifies requirements
- Phase 2: Creates plan
- **Phase 2 includes: "Should I create documentation for this module?"**
- Doesn't assume docs are needed

---

### Test 15: Reset Test

**Prompt:**
```
Follow .cursorrules
```

**Expected:**
```
I've re-read the .cursorrules file. Key rules I'll follow:

🚨 CRITICAL:
- Never create documentation without explicit approval

📋 Workflow:
- Phase 1: Clarification
- Phase 2: Planning (ask about docs)
- Phase 3: Implementation
- Phase 4: Review

🔍 Context:
- Always gather context before changes

Ready to proceed with heightened attention to these guidelines.
```

---

## 💬 Composer Test Prompt

If using Cursor Composer for multi-file changes:

```
Using Composer, help me refactor the database layer to add connection pooling.

Important: Follow .cursorrules:
1. First clarify what needs to be refactored
2. Create a detailed plan
3. Ask if I want documentation
4. Only proceed after approval
5. Summarize changes at the end

Let's start with clarification questions.
```

---

## 🔍 Code Review Test Prompt

```
Review this code and suggest improvements:

```typescript
export function addInstruction(data: any) {
  const stmt = db.prepare('INSERT INTO instructions VALUES (?, ?, ?, ?)');
  stmt.run(data.title, data.content, data.category, data.priority);
}
```

Before suggesting changes:
1. Gather context by reading similar functions in this codebase
2. Check the types.ts file for proper type definitions
3. Review the database schema in db.ts
4. Then provide your review

Follow the context gathering protocol from .cursorrules.
```

---

## 🎨 Inline Edit Test

In the editor, highlight some code and use Cursor's inline edit feature:

**Prompt:**
```
Refactor this function following .cursorrules. Remember to ask about documentation if needed.
```

---

## 📚 Knowledge Check Prompt

```
Based on .cursorrules, answer:

1. Project Context:
   - What is this project?
   - What's the tech stack?
   - What are the key commands?

2. Critical Rules:
   - What's the MOST important rule with highest priority?
   - Give an example of when you should and shouldn't create docs

3. Workflow:
   - Describe the 4 phases
   - When do you wait for user approval?

4. Context Gathering:
   - What should you do before making changes?
   - Where should you look for existing patterns?
```

---

## 🔧 MCP Server Query (If configured)

If you've set up the MCP server integration:

```
Query the MCP Instructions Server:

1. List all workflow rules
2. Show Erlang coding patterns
3. List common pitfalls in Java
4. Get project configuration for butler-server

Use the MCP tools to retrieve this information.
```

---

## 🆘 Troubleshooting Prompt

If Cursor isn't following the rules:

```
I notice you're not following the .cursorrules file. Let's reset:

1. Confirm you can see .cursorrules in the workspace root
2. Read it completely
3. Acknowledge the three critical rules:
   - Documentation approval
   - 4-phase workflow
   - Context gathering

4. Then let's try again: Add a new utility function for date formatting.
```

---

## 💡 Best Practices Prompt

```
Let's establish our working relationship based on .cursorrules:

1. Confirm you'll always ask before creating documentation
2. Confirm you'll follow the 4-phase workflow for features
3. Confirm you'll gather context before changes

Then tell me: What's the best way for me to remind you of these rules if you forget during our session?
```

---

## 🎯 The Ultimate Setup & Test Prompt

**Copy this complete prompt for full setup and testing:**

```
# Cursor AI Setup & Test

I'm working in the MCP Instructions Server project (dev-setup/mcp-instructions-server).

## Step 1: Confirm Rules
Read .cursorrules and confirm you understand:
- 🚨 CRITICAL: Never create docs without approval
- 📋 4-phase workflow for all features
- 🔍 Context gathering before changes
- 💡 Project context (TypeScript/Node.js/SQLite MCP server)

## Step 2: Quick Test
Answer without creating any files:
1. Should you create documentation without asking? (Answer: No)
2. What's the first phase of the workflow? (Answer: Clarification)
3. What tech stack is this project? (Answer: TypeScript, Node.js, SQLite, MCP)

## Step 3: Practical Test
Let's add a simple feature: "Add a helper function to format instruction titles"

Follow the 4-phase workflow:
- Phase 1: Ask clarifying questions
- Phase 2: Create plan, ask about docs
- Phase 3: Wait for approval, then implement
- Phase 4: Summarize

Ready? Start with Phase 1.

## Important
These rules apply to:
✅ Chat responses
✅ Composer multi-file edits
✅ Inline code edits
✅ All suggestions

If I say "Follow .cursorrules", that's a reminder to check the workflow and documentation approval rule.
```

---

## 📊 Expected Behavior Summary

After using these prompts, Cursor should:

| Scenario            | Expected Behavior                                                                                      |
| ------------------- | ------------------------------------------------------------------------------------------------------ |
| **Feature request** | Phase 1: Asks questions → Phase 2: Plans & asks about docs → Phase 3: Implements → Phase 4: Summarizes |
| **Doc creation**    | Always asks first: "Should I create documentation?"                                                    |
| **Code changes**    | Gathers context by reading existing code first                                                         |
| **Composer**        | Follows same workflow across multiple files                                                            |
| **Inline edits**    | Respects rules even for small changes                                                                  |

---

## 🎉 Quick Start

**Paste this now into Cursor Chat:**

```
Read .cursorrules and answer:
1. Should you create docs without asking?
2. What's the 4-phase workflow?
3. What's this project about?

Then let's test by adding a small feature following the workflow.
```

---

**Save this file for reference!** All prompts are designed to work immediately with your .cursorrules configuration. 🚀
