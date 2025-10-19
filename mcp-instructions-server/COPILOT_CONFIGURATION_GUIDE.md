# Configuring GitHub Copilot to Always Follow Your Rules

## 🎯 Goal

Make Copilot follow your rules in **ALL conversations**, not just when using `@github`.

---

## 📍 Three Levels of Instructions

### 1. **Workspace Instructions** (Project-specific)
**Location:** `.github/copilot-instructions.md` in your workspace

✅ **Already created:** `/Users/amar.c/workspace/dev-setup/.github/copilot-instructions.md`

**How it works:**
- Applies to **this workspace only** (dev-setup)
- Works with and without `@github`
- Highest priority for workspace-related tasks

---

### 2. **Global User Instructions** (All workspaces)
**Location:** Set in VS Code User Settings

**Option A: Via Settings UI**

1. Open VS Code Settings: `Cmd+,` (Mac) or `Ctrl+,` (Windows/Linux)
2. Search for: `github.copilot.chat.codeGeneration.instructions`
3. Click "Edit in settings.json"
4. Add your global rules:

```json
{
  "github.copilot.chat.codeGeneration.instructions": [
    {
      "text": "CRITICAL: Never create documentation files (README, API docs, guides, etc.) without explicit user approval. Always ask first: 'Should I create documentation for this?'"
    },
    {
      "text": "Follow the 4-phase workflow: 1) Clarify requirements, 2) Create plan, 3) Implement, 4) Review. Wait for approval between phases."
    },
    {
      "text": "Always gather context before making changes. Read existing code, check for patterns, understand dependencies."
    }
  ]
}
```

**Option B: Via File** (More flexible)

1. Create global instructions file:
   ```bash
   mkdir -p ~/.config/github-copilot
   nano ~/.config/github-copilot/instructions.md
   ```

2. Add your rules (see `.github/copilot-instructions-global.md` for template)

3. Reference in VS Code settings:
   ```json
   {
     "github.copilot.chat.codeGeneration.instructionsFile": "~/.config/github-copilot/instructions.md"
   }
   ```

---

### 3. **MCP Server Instructions** (Dynamic, context-aware)
**Location:** Your MCP Instructions Server database

✅ **Already configured:** 34 instructions in `instructions.db`

**How it works:**
- Loaded when using `@github` in Copilot Chat
- Context-aware (auto-detects project, language)
- Queryable via MCP resources and tools

---

## 🔄 Priority Order (Highest to Lowest)

```
1. Workspace Instructions (.github/copilot-instructions.md)
   ↓
2. User Global Instructions (VS Code settings)
   ↓
3. MCP Server Instructions (when using @github)
   ↓
4. Default Copilot Behavior
```

**If rules conflict, higher priority wins.**

---

## ✅ Recommended Setup

### For Maximum Coverage:

1. **Workspace Instructions** (✅ Done)
   - `/Users/amar.c/workspace/dev-setup/.github/copilot-instructions.md`
   - Contains critical rules for this project

2. **Global Instructions** (📝 Action Required)
   - Add to VS Code User Settings
   - Copy content from `.github/copilot-instructions-global.md`

3. **MCP Server** (✅ Done)
   - Already configured with 34 instructions
   - Auto-loads with `@github`

---

## 🚀 Quick Setup: Global Instructions

### Step 1: Open VS Code Settings JSON

```bash
# Mac
code ~/.config/Code/User/settings.json

# Windows
code %APPDATA%\Code\User\settings.json

# Linux
code ~/.config/Code/User/settings.json
```

### Step 2: Add Global Instructions

Add this to your `settings.json`:

```json
{
  "github.copilot.chat.codeGeneration.instructions": [
    {
      "text": "🚨 CRITICAL RULE: NEVER create any documentation files (README.md, API docs, architecture docs, guides, etc.) without explicit user approval. Always ask first: 'Should I create documentation for this?' Exceptions: inline comments (<10 lines), docstrings, type annotations."
    },
    {
      "text": "Follow 4-phase workflow for features: 1) Requirement Clarification (restate, ask questions, wait for confirmation), 2) Planning (gather context, create plan, get approval), 3) Implementation (follow plan, validate continuously), 4) Review (summarize changes, show validation results)."
    },
    {
      "text": "ALWAYS gather context before making changes: read existing code, check for similar implementations, understand patterns and dependencies, review documentation."
    },
    {
      "text": "When in doubt about creating files (especially documentation), ASK FIRST. User decides what gets created, not the AI."
    }
  ]
}
```

### Step 3: Restart VS Code

Close all windows and reopen for changes to take effect.

### Step 4: Test

**Test without @github:**
```
"Add a new authentication module"
```

**Expected behavior:**
- Copilot follows 4-phase workflow
- Asks: "Should I create documentation for this module?"
- Waits for your approval

**Test with @github:**
```
@github Add a new authentication module
```

**Expected behavior:**
- Copilot loads MCP instructions + global instructions
- Same behavior as above + additional context from MCP server

---

## 🔍 Verification Checklist

After setup, verify:

- [ ] Workspace instructions file exists: `.github/copilot-instructions.md`
- [ ] Global instructions added to VS Code settings
- [ ] VS Code restarted
- [ ] Test without `@github`: Copilot asks before creating docs
- [ ] Test with `@github`: Same behavior + MCP context
- [ ] MCP server loads automatically when using `@github`

---

## 📊 Coverage Matrix

| Scenario                    | Workspace Instructions | Global Instructions | MCP Server |
| --------------------------- | :--------------------: | :-----------------: | :--------: |
| Chat in editor (no @github) |           ✅            |          ✅          |     ❌      |
| Inline suggestions          |           ❌            |          ✅          |     ❌      |
| @github in Copilot Chat     |           ✅            |          ✅          |     ✅      |
| Different workspace         |           ❌            |          ✅          |     ✅*     |

*If that workspace has MCP server configured

---

## 🎯 Best Practice: Three-Layer Strategy

### Layer 1: Critical Rules → Global Settings
**What:** Security, permissions, critical workflow rules
**Where:** `github.copilot.chat.codeGeneration.instructions` in User settings
**Why:** Applies everywhere, always

**Example:**
- Never create docs without approval
- Never log sensitive data
- Always validate user input

### Layer 2: Project Rules → Workspace Instructions
**What:** Project-specific conventions, team standards
**Where:** `.github/copilot-instructions.md` in each project
**Why:** Only applies to that project

**Example:**
- Project structure conventions
- Team coding standards
- Project-specific workflows

### Layer 3: Dynamic Context → MCP Server
**What:** Language patterns, coding examples, pitfalls
**Where:** MCP Instructions Server database
**Why:** Context-aware, queryable, easily updated

**Example:**
- Erlang OTP patterns
- Java Spring Boot patterns
- Common pitfalls by language

---

## 🔧 Troubleshooting

### Issue: Copilot still creates docs without asking

**Solution:**
1. Check global settings exist:
   ```bash
   grep "copilot.chat.codeGeneration.instructions" ~/.config/Code/User/settings.json
   ```
2. Verify workspace instructions exist:
   ```bash
   cat .github/copilot-instructions.md
   ```
3. Restart VS Code (close ALL windows)
4. Test again with explicit reminder:
   ```
   "Add a feature. Remember to follow the documentation approval rule."
   ```

### Issue: Global instructions not loading

**Solution:**
1. Verify JSON syntax in settings.json
2. Check for conflicting settings
3. Try Settings UI instead of JSON
4. Restart VS Code

### Issue: Different behavior with/without @github

**Expected!**
- Without `@github`: Uses workspace + global instructions only
- With `@github`: Also loads MCP server instructions

---

## 📝 Next Steps

1. ✅ Workspace instructions created (`.github/copilot-instructions.md`)
2. 📝 **You need to:** Add global instructions to VS Code settings
3. ✅ MCP server already configured
4. 🧪 Test both scenarios (with/without `@github`)

---

## 🎓 Summary

**To make Copilot follow rules everywhere:**

1. **Workspace level:** `.github/copilot-instructions.md` ✅ Done
2. **Global level:** Add to VS Code User settings ⏳ Action needed
3. **MCP level:** Already configured ✅ Done

After adding global settings, Copilot will **always** ask before creating documentation, whether you use `@github` or not!

---

**Ready to add global settings?** Open VS Code settings and add the configuration from Step 2 above! 🚀
