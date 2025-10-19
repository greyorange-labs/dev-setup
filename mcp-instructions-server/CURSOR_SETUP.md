# Cursor AI Setup Guide

**Complete guide to using the MCP Instructions Server with Cursor AI for ~100% accuracy**

---

## 🎯 Overview

This guide configures Cursor AI to match your GitHub Copilot setup, providing:
- ✅ **~100% accuracy** through structured workflows
- ✅ **Documentation approval** - Never creates docs without asking
- ✅ **4-phase development** - Consistent, error-free implementations
- ✅ **Context-aware** - Understands your codebase patterns
- ✅ **Team-ready** - Share setup across your company

---

## 🏗️ Architecture: Cursor vs Copilot

**[Both Tools]** Use the same MCP Instructions Server database (34 instructions)

**[Cursor Specific]:**
- `.cursorrules` file at workspace root (primary method)
- Built-in multi-tab context management
- Composer mode for multi-file edits
- Native inline editing (CMD+K)

**[Copilot Specific]:**
- VS Code settings JSON
- `.github/copilot-instructions.md`
- `@github` prefix to load MCP
- Global instructions in settings

**Result:** Both tools follow the same rules, different integration methods

---

## ✅ What's Already Working

### `.cursorrules` File Created

**Location:** `/Users/amar.c/workspace/dev-setup/.cursorrules`

**Contains:**
- 🚨 Documentation Approval Required (CRITICAL)
- 📋 4-Phase Workflow (Clarification → Planning → Implementation → Review)
- 🔍 Context Gathering Protocol
- 🎯 Cursor-Specific Best Practices (Multi-tab, Composer, Inline edits)
- 💡 Project-specific context (MCP Instructions Server)
- 🔄 Workflow variations by mode (Chat, Composer, Inline)

**Status:** ✅ **Active immediately in Cursor!**

Cursor automatically loads `.cursorrules` from workspace root.

---

## 🚀 Quick Setup (3 Steps)

### Step 1: Verify .cursorrules is Active

The `.cursorrules` file is already created and active!

**Verify:**
```bash
cat /Users/amar.c/workspace/dev-setup/.cursorrules
```

**You should see:** Rules for documentation approval, 4-phase workflow, and Cursor-specific practices

### Step 2: Configure Cursor Settings (Recommended)

**A. Open Cursor Settings:**
- Press `CMD+Shift+P` → "Preferences: Open User Settings (JSON)"
- Or edit: `~/Library/Application Support/Cursor/User/settings.json`

**B. Add these settings:**
```json
{
  // Enable .cursorrules file support
  "cursor.general.enableRulesFile": true,

  // Increase context window
  "cursor.chat.maxContextTokens": 100000,

  // Enable Composer mode
  "cursor.composer.enableComposer": true,

  // Prioritize context over web search
  "cursor.chat.alwaysSearchWeb": false
}
```

**C. Save** (`CMD+S`)

**📝 See:** `cursor-settings-to-add.json` for copy-paste configuration

### Step 3: Restart Cursor

Close **ALL** Cursor windows and reopen.

---

## 🧪 Verify Your Setup

### Test 1: Documentation Approval Rule

**Prompt:**
```
Should you create documentation without asking me first?
```

**Expected:** "No, I must always ask for explicit approval first"

✅ **Pass:** Rule is active
❌ **Fail:** See [Troubleshooting](#troubleshooting)

### Test 2: 4-Phase Workflow

**Prompt:**
```
What workflow should you follow when implementing a feature?
```

**Expected:** Description of 4 phases (Clarification → Planning → Implementation → Review)

✅ **Pass:** Workflow loaded
❌ **Fail:** Cursor may not have loaded .cursorrules

### Test 3: Practical Implementation

**Prompt:**
```
Add a helper function to format instruction titles. Follow .cursorrules.
```

**Expected behavior:**
1. **Phase 1:** Asks clarifying questions (capitalize? trim? length limits?)
2. **Phase 2:** Creates plan, asks "Should I create documentation?"
3. **Phase 3:** Implements after approval
4. **Phase 4:** Summarizes changes and validation results

✅ **Pass:** Full workflow executed
❌ **Fail:** Remind Cursor to "Follow .cursorrules strictly"

### Test 4: Composer Mode (Multi-File)

**Open Composer:** `CMD+I`

**Prompt:**
```
Update the database schema to add a priority field across db.ts, types.ts, and seed.ts. Follow .cursorrules.
```

**Expected:**
- Shows all 3 files it will modify
- Follows 4-phase workflow
- Asks about documentation
- Waits for approval before implementing

### Test 5: Inline Edit Mode

**Select code** → Press `CMD+K`

**Prompt:**
```
Refactor this function to be more concise
```

**Expected:**
- Makes focused change
- Preserves existing style
- Explains what changed

---

## 📊 Setup Verification Checklist

**Complete this checklist:**

- [ ] `.cursorrules` file exists at `/Users/amar.c/workspace/dev-setup/.cursorrules`
- [ ] Cursor settings updated with `enableRulesFile: true`
- [ ] Cursor restarted (ALL windows closed)
- [ ] Test 1 passed: Documentation approval works
- [ ] Test 2 passed: Workflow description correct
- [ ] Test 3 passed: Practical implementation follows workflow
- [ ] Test 4 passed: Composer respects rules
- [ ] Test 5 passed: Inline edits respect rules

**All checks pass?** ✅ **You're ready to go!**

**Some checks fail?** See [Troubleshooting](#troubleshooting) below

---

## 🎯 Cursor-Specific Best Practices

### 1. Multi-Tab Context Management

**Feature:** Cursor tracks context across all open tabs automatically

**How to use:**
```
I have User.ts, UserService.ts, and UserController.ts open.
Update the user creation flow across all three files. Follow .cursorrules.
```

**Benefit:** Cursor maintains awareness of related files without explicit @file references

### 2. Composer Mode (CMD+I) - For Multi-File Changes

**When to use:**
- New features spanning multiple files
- Refactoring across components
- Schema changes affecting multiple layers

**Best practice:**
```
[CMD+I to open Composer]

Add user authentication:
1. Auth middleware in middleware/auth.ts
2. Auth routes in routes/auth.ts
3. Auth service in services/auth.ts
4. Auth types in types/auth.ts

Follow .cursorrules workflow.
```

**Still follows 4-phase workflow!** Composer is just the interface mode.

### 3. Inline Edits (CMD+K) - For Focused Changes

**When to use:**
- Renaming variables/functions
- Adding type annotations
- Quick refactorings
- Style improvements

**Best practice:**
```
[Select code] → [CMD+K]

Add proper error handling with try-catch
```

**Tip:** Even for small edits, the documentation approval rule still applies if creating new files.

### 4. Chat Mode (CMD+L) - For Exploration

**When to use:**
- Understanding existing code
- Planning before implementation
- Asking about patterns
- Code review and suggestions

**Best practice:**
```
Explain how the database seeding works in src/seed.ts.
What patterns does it follow?
```

### 5. Reference Files with @ Notation

**In any mode:**
```
@src/db.ts - How does this handle database connections?
@types.ts - What's the Instruction type structure?
```

**Benefit:** Cursor loads specific files into context

---

## 💡 Top Developer Practices for Cursor

Based on research from top companies and developers:

### 1. Break Large Tasks into Chunks

**❌ Don't:**
```
Build entire authentication system
```

**✅ Do:**
```
Phase 1: Add user model and types
Phase 2: Add auth middleware
Phase 3: Add login/logout routes
Phase 4: Add session management
```

**Why:** Cursor handles smaller, focused tasks more accurately

### 2. Use Explicit References

**❌ Vague:**
```
Update the auth code
```

**✅ Specific:**
```
In @src/middleware/auth.ts, update the validateToken function to check for token expiry. Follow existing error handling patterns.
```

### 3. Leverage Context, Not Web Search

**Setting:**
```json
"cursor.chat.alwaysSearchWeb": false
```

**Why:** Your codebase context is more accurate than generic web results

**When you need web:** Explicitly ask "search web for [topic]"

### 4. Review Before Accepting

Even with ~100% accuracy:
- Read the proposed changes
- Verify against .cursorrules
- Check for documentation creation
- Ensure tests are updated

### 5. Use "Follow .cursorrules" as Reset

If Cursor starts deviating:
```
Before continuing, read .cursorrules and confirm you understand the critical rules.
```

---

## 📚 Quick Reference

### Keyboard Shortcuts

| Action | macOS | Purpose |
|--------|-------|---------|
| Chat | `CMD+L` | Open chat panel |
| Composer | `CMD+I` | Multi-file edits |
| Inline Edit | `CMD+K` | Quick focused changes |
| Settings | `CMD+,` | Open settings |
| Command Palette | `CMD+Shift+P` | All commands |

### Common Prompts

**Understanding:**
```
Explain how @filename works
What patterns does @filename follow?
What are the dependencies of @filename?
```

**Implementation:**
```
Implement [feature] in @filename. Follow .cursorrules.
Add [function] following the existing patterns in this file.
```

**Refactoring:**
```
Refactor @filename to use [pattern]. Maintain backward compatibility.
Extract common logic from @file1 and @file2 into shared utility.
```

**Code Review:**
```
Review @filename for:
1. Common pitfalls
2. Pattern consistency
3. Error handling
4. Test coverage
```

---

## ⚙️ Advanced: MCP Server Integration (Future)

**Current Status:** Cursor may not fully support MCP protocol yet.

**Primary Method:** `.cursorrules` file (already working!)

**If Cursor adds MCP support:**

Add to Cursor settings:
```json
{
  "cursor.mcp.servers": {
    "instructions-server": {
      "command": "node",
      "args": [
        "/Users/amar.c/workspace/dev-setup/mcp-instructions-server/build/index.js"
      ],
      "env": {
        "CURSOR_CURRENT_FILE": "${file}"
      }
    }
  }
}
```

**For now:** Query MCP database manually if needed:

```bash
# Open database
sqlite3 /Users/amar.c/workspace/dev-setup/mcp-instructions-server/instructions.db

# Example queries
SELECT title, category FROM instructions;
SELECT content FROM instructions WHERE language='erlang';
SELECT title FROM instructions WHERE category='pitfalls';
```

---

## 📊 Comparison: .cursorrules vs MCP Server

| Feature      | `.cursorrules`       | MCP Server           |
| ------------ | -------------------- | -------------------- |
| **Setup**    | ✅ Automatic          | Manual config needed |
| **Scope**    | This workspace only  | Can be global        |
| **Content**  | Static rules         | Dynamic, queryable   |
| **Search**   | No search            | Full-text search     |
| **Updates**  | Edit file            | Update database      |
| **Best for** | Core rules, workflow | Large knowledge base |

**Recommendation:** Use **both**!
- `.cursorrules` for critical rules (documentation approval, workflow)
- MCP Server for searchable knowledge base (patterns, pitfalls)

---

## 🎓 Usage Examples

### Example 1: Simple Feature (Uses .cursorrules)

**You:**
```
Add a function to calculate user age from birthdate
```

**Cursor:**
```
Phase 1: Let me clarify...
- Input: birthdate (Date or string?)
- Output: age in years?
- Handle edge cases (future dates, leap years)?

Please confirm before I plan the implementation.
```

**You:** "Confirmed. Input is Date object, output is number."

**Cursor:**
```
Phase 2: Implementation Plan
- Create calculateAge() function in utils/date.ts
- Add unit tests
- Handle edge cases

Should I create documentation for this function?
```

**You:** "No docs, just JSDoc comments"

**Cursor:** [Implements with JSDoc comments only]

### Example 2: Code Review (Uses .cursorrules)

**You:**
```
Review this Erlang code:

handle_call({create_user, Name}, _From, State) ->
    UserId = list_to_atom(Name),
    {reply, {ok, UserId}, State}.
```

**Cursor (after gathering context):**
```
⚠️ Issue Found: Atom Exhaustion Risk

Problem: Using list_to_atom/1 with user input can exhaust the atom table.

Suggested fix:
```erlang
handle_call({create_user, Name}, _From, State) ->
    case validate_name(Name) of
        {ok, ValidName} ->
            UserId = list_to_binary(ValidName),
            {reply, {ok, UserId}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.
```
```

### Example 3: With MCP Server (Advanced)

**You:**
```
@mcp What are the Erlang gen_server patterns I should follow?
```

**Cursor (queries MCP):**
```
Based on the MCP Instructions Server, here are the gen_server patterns:

1. Always implement all callbacks
2. Use proper return tuples: {reply, Reply, State} | {noreply, State} | {stop, Reason, State}
3. Add type specifications with -spec
4. Handle timeout for cleanup
5. Use supervisor for fault tolerance

[Shows full pattern from database]
```

---

## 🔄 Updating Rules

### Update .cursorrules

```bash
# Edit the file
code /Users/amar.c/workspace/dev-setup/.cursorrules

# Changes apply immediately in Cursor (no restart needed)
```

### Update MCP Instructions

```bash
cd /Users/amar.c/workspace/dev-setup/mcp-instructions-server

# Edit instructions/*.json files
nano instructions/workflow-rules.json

# Rebuild and reseed
rm instructions.db
npm run seed

# Restart Cursor to reload
```

---

## 🆘 Troubleshooting

### Issue 1: Cursor Not Following .cursorrules

**Symptoms:**
- Creates documentation without asking
- Skips phases in workflow
- Doesn't gather context

**Solutions:**

**A. Verify rules file exists and is correct:**
```bash
cat /Users/amar.c/workspace/dev-setup/.cursorrules
# Should show complete rules
```

**B. Check Cursor settings:**
```json
{
  "cursor.general.enableRulesFile": true  // Must be true
}
```

**C. Verify workspace:**
```bash
pwd
# Should be: /Users/amar.c/workspace/dev-setup or subdirectory
```

**D. Restart Cursor:**
Close ALL windows and reopen

**E. Explicit reminder:**
```
Before we continue, read .cursorrules and confirm you understand the critical rules, especially documentation approval.
```

### Issue 2: Cursor Creates Docs Without Asking

**This means the documentation approval rule isn't active.**

**Fix:**

1. **Verify rule exists in .cursorrules:**
   ```bash
   grep -A 5 "Documentation Approval" /Users/amar.c/workspace/dev-setup/.cursorrules
   ```

2. **Check enableRulesFile setting** (see above)

3. **Be explicit in requests:**
   ```
   Add authentication module. REMEMBER: Ask before creating ANY documentation.
   ```

4. **Test the rule specifically:**
   ```
   Should you create a README file without asking me first?
   ```
   Expected answer: "No"

### Issue 3: Composer Mode Not Following Workflow

**Symptoms:**
- Composer makes changes without asking questions
- Skips approval steps

**Solution:**

**A. Always include workflow reminder:**
```
[CMD+I for Composer]

Add user service across multiple files.
Follow .cursorrules 4-phase workflow:
1. Clarify requirements first
2. Create plan and ask about docs
3. Wait for approval
4. Then implement
```

**B. Break into phases manually:**
```
Phase 1: Tell me what files you'll need to modify and ask clarifying questions.
Don't implement yet.
```

### Issue 4: Context Window Too Small

**Symptoms:**
- Cursor "forgets" earlier context
- Doesn't maintain file awareness

**Solution:**

**A. Increase token limit:**
```json
{
  "cursor.chat.maxContextTokens": 100000
}
```

**B. Reference files explicitly:**
```
Referring to @src/db.ts, @src/types.ts, and @src/seed.ts...
```

### Issue 5: Cursor Searches Web Instead of Using Context

**Solution:**

**A. Disable automatic web search:**
```json
{
  "cursor.chat.alwaysSearchWeb": false
}
```

**B. Be explicit:**
```
Use only the code in this workspace. Don't search the web.
```

### Issue 6: Rules Work in Chat but Not in Composer/Inline

**This is expected if rules aren't properly configured.**

**Solution:**

**A. Verify rules are comprehensive:**
Check `.cursorrules` has section: "🔄 Workflow Variations by Mode"

**B. Always reference rules:**
```
[In Composer] Follow .cursorrules for this multi-file change
[In Inline Edit] Follow .cursorrules even for this small change
```

### Issue 7: MCP Server Not Loading (If Configured)

**Symptoms:**
- Can't query MCP database
- Pattern instructions not available

**Solution:**

**A. Check if Cursor supports MCP:**
Cursor may not support MCP protocol yet. Primary method is `.cursorrules`.

**B. Manual database query:**
```bash
sqlite3 /Users/amar.c/workspace/dev-setup/mcp-instructions-server/instructions.db "SELECT title FROM instructions LIMIT 10;"
```

**C. Alternative:**
Copy patterns from MCP database into `.cursorrules` if needed.

### Issue 8: Different Behavior Than Copilot

**This is normal!** Cursor and Copilot are different tools.

**Common differences:**
- Copilot: Uses `@github` prefix, VS Code settings
- Cursor: Uses `.cursorrules`, different UI

**Both follow same core rules:**
- Documentation approval
- 4-phase workflow
- Context gathering

**Solution:**
See [SETUP_COMPARISON.md](SETUP_COMPARISON.md) for detailed differences.

---

## 🔧 Debug Checklist

If things aren't working, verify each:

- [ ] `.cursorrules` file exists at workspace root
- [ ] `cursor.general.enableRulesFile: true` in settings
- [ ] Working directory is correct (`pwd`)
- [ ] Cursor restarted after settings changes
- [ ] Test prompt returns correct answer
- [ ] Reminding Cursor explicitly about rules
- [ ] Using correct mode for task (Chat/Composer/Inline)
- [ ] Breaking large tasks into smaller chunks

**Still not working?**

See [CURSOR_LOCAL_SETUP_INSTRUCTIONS.md](CURSOR_LOCAL_SETUP_INSTRUCTIONS.md) for detailed local setup.

---

## 💡 Pro Tips for Cursor

### 1. Reference .cursorrules Explicitly

```
Implement this feature following the .cursorrules workflow
```

### 2. Ask About Rules

```
What rules are defined in .cursorrules for this project?
```

### 3. Test Rules First

```
Before we start, confirm you've read .cursorrules and will follow:
1. Documentation approval rule
2. 4-phase workflow
3. Context gathering protocol
```

### 4. Use Composer for Multi-File Changes

When using Cursor Composer:
```
Use Composer to implement user authentication module.
Follow .cursorrules for workflow and documentation approval.
```

---

## 📝 Quick Reference

### Cursor Settings Locations

**macOS:**
```
~/.config/Cursor/User/settings.json
```

**Windows:**
```
%APPDATA%\Cursor\User\settings.json
```

**Linux:**
```
~/.config/Cursor/User/settings.json
```

### Project Files

**Rules:**
- `/Users/amar.c/workspace/dev-setup/.cursorrules` (workspace)

**MCP Server:**
- `/Users/amar.c/workspace/dev-setup/mcp-instructions-server/` (optional)

---

## 🎯 Summary

**For Cursor, you have two options:**

### Option 1: Simple Setup (Recommended) ✅

**Use `.cursorrules` file** (already created!)

- ✅ No additional setup needed
- ✅ Rules apply immediately
- ✅ Works in all Cursor features (chat, compose, inline)
- ✅ Easy to update

**Test it now:**
```
Should you create documentation without asking me?
```

### Option 2: Advanced Setup (Optional)

**Add MCP Server integration**

- Requires manual configuration
- Provides searchable knowledge base
- More powerful but more complex

---

## 🚀 Get Started Now

Paste this in Cursor Chat:

```
I have a .cursorrules file in my workspace with:
1. CRITICAL rule: Never create docs without approval
2. 4-phase workflow for features
3. Context gathering requirements

Test these rules by answering:
- Should you create documentation without asking?
- What workflow should you follow for features?
- Should you gather context before making changes?

Then ask: "Should we add a new feature to this project?"
and follow the workflow.
```

---

**Your Cursor AI is now configured with the same rules as VS Code/Copilot!** 🎉
