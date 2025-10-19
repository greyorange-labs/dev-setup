# Cursor AI Local Setup Instructions

**Setup guide for configuring Cursor AI on your macOS system**

---

## ✅ What's Already Done

- ✅ `.cursorrules` file created and enhanced with best practices
- ✅ MCP Instructions Server built and seeded (34 instructions)
- ✅ Documentation files created

---

## 🚀 Local Cursor Configuration

### Step 1: Verify .cursorrules is Active

The `.cursorrules` file at the workspace root is **automatically loaded by Cursor**.

Verify it exists:
```bash
cat /Users/amar.c/workspace/dev-setup/.cursorrules
```

**Expected:** You should see the Cursor AI Rules with:
- 🚨 Documentation Approval Required
- 📋 4-Phase Workflow
- 🔍 Context Gathering
- 🎯 Cursor-Specific Best Practices

### Step 2: Add Cursor-Specific Settings

1. **Open Cursor Settings:**
   - Press `CMD+Shift+P`
   - Type: "Preferences: Open User Settings (JSON)"
   - Or directly edit: `~/Library/Application Support/Cursor/User/settings.json`

2. **Add these settings** (from `cursor-settings-to-add.json`):

```json
{
  // ... your existing settings ...

  // Enable .cursorrules file support
  "cursor.general.enableRulesFile": true,

  // Increase context window for better understanding
  "cursor.chat.maxContextTokens": 100000,

  // Enable Composer mode for multi-file edits
  "cursor.composer.enableComposer": true,

  // Disable automatic web search (use context first)
  "cursor.chat.alwaysSearchWeb": false
}
```

3. **Save the file** (`CMD+S`)

### Step 3: Restart Cursor

Close all Cursor windows and reopen for settings to take effect.

---

## 🧪 Test Your Setup

### Test 1: Rules File is Active

Open Cursor Chat and ask:

```
Should you create documentation without asking me first?
```

**Expected answer:** "No, I must always ask for explicit approval first"

### Test 2: 4-Phase Workflow

Ask:

```
What workflow should you follow when I ask you to implement a feature?
```

**Expected answer:** Description of the 4-phase workflow (Clarification → Planning → Implementation → Review)

### Test 3: Practical Implementation

```
Add a helper function to format instruction titles in this project. Follow .cursorrules.
```

**Expected behavior:**
1. **Phase 1:** Asks clarifying questions about formatting requirements
2. **Phase 2:** Creates plan and asks: "Should I create documentation for this?"
3. **Phase 3:** Implements after your approval
4. **Phase 4:** Summarizes changes

### Test 4: Composer Mode

1. Open Cursor Composer (`CMD+I`)
2. Request a multi-file change:

```
Update the database schema to add a new field across db.ts, types.ts, and seed.ts
```

**Expected:** Cursor should:
- Ask clarifying questions
- Show all files it will modify
- Ask about documentation
- Wait for approval

### Test 5: Inline Edit Mode

1. Select some code in a file
2. Press `CMD+K`
3. Ask for a refactoring

**Expected:** Cursor should:
- Make focused change
- Preserve existing style
- Explain the change

---

## 📊 Setup Verification Checklist

After completing setup, verify:

- [ ] `.cursorrules` file exists at workspace root
- [ ] Cursor settings updated with `enableRulesFile: true`
- [ ] Cursor restarted
- [ ] Test 1 passed: Documentation approval rule works
- [ ] Test 2 passed: Workflow description correct
- [ ] Test 3 passed: Practical implementation follows workflow
- [ ] Test 4 passed: Composer mode respects rules
- [ ] Test 5 passed: Inline edits respect rules

---

## 🎯 Cursor-Specific Features to Leverage

### 1. Multi-Tab Context
Cursor automatically tracks context across multiple open files.

**Best practice:**
- Open related files in tabs
- Reference them with `@filename.ts` in chat
- Cursor maintains awareness of all open files

### 2. Composer Mode (CMD+I)
For complex multi-file changes.

**When to use:**
- Adding new features across multiple files
- Refactoring that affects many components
- Updating patterns across the codebase

**Still follows 4-phase workflow!**

### 3. Inline Edits (CMD+K)
For quick, focused changes.

**When to use:**
- Small refactorings
- Renaming
- Quick fixes
- Type annotation additions

### 4. Chat Mode (CMD+L)
For exploration and understanding.

**When to use:**
- Understanding existing code
- Planning implementations
- Asking about patterns
- Code review

---

## 🔄 Integration with MCP Server (Optional)

**Note:** Cursor may not fully support MCP protocol yet. The primary integration method is `.cursorrules`.

If Cursor adds MCP support in the future, you can add this to settings:

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

For now, `.cursorrules` provides all the workflow rules, and you can manually reference the MCP database for patterns:

```bash
# Query the MCP database
sqlite3 /Users/amar.c/workspace/dev-setup/mcp-instructions-server/instructions.db

# Example queries
SELECT title FROM instructions WHERE category='workflow-rules';
SELECT title FROM instructions WHERE language='erlang';
SELECT content FROM instructions WHERE title LIKE '%Documentation%';
```

---

## 🛠️ Troubleshooting

### Issue: Cursor Not Following .cursorrules

**Solutions:**

1. **Verify rules file exists:**
   ```bash
   ls -la /Users/amar.c/workspace/dev-setup/.cursorrules
   ```

2. **Check setting is enabled:**
   Open Cursor settings and verify:
   ```json
   "cursor.general.enableRulesFile": true
   ```

3. **Restart Cursor:**
   Close ALL windows and reopen

4. **Explicitly remind Cursor:**
   ```
   Follow the .cursorrules file. Read it before proceeding.
   ```

5. **Check workspace:**
   Make sure you're in the `/Users/amar.c/workspace/dev-setup` directory

### Issue: Cursor Creates Docs Without Asking

**This means the rule isn't being applied correctly.**

**Fix:**

1. Re-read the .cursorrules file:
   ```
   Before we continue, please read the .cursorrules file and confirm you understand the documentation approval rule.
   ```

2. Be explicit in requests:
   ```
   Add a feature. Remember: ASK before creating any documentation.
   ```

3. Check if `.cursorrules` is properly formatted (no JSON errors)

### Issue: Composer Mode Not Working

**Check:**

1. Setting enabled:
   ```json
   "cursor.composer.enableComposer": true
   ```

2. Try keyboard shortcut: `CMD+I`

3. Or use menu: View → Command Palette → "Composer"

### Issue: Context Window Too Small

**Increase it:**

```json
"cursor.chat.maxContextTokens": 100000
```

Then restart Cursor.

---

## 📚 Quick Reference

### Keyboard Shortcuts

| Action | Shortcut |
|--------|----------|
| Open Chat | `CMD+L` |
| Open Composer | `CMD+I` |
| Inline Edit | `CMD+K` |
| Command Palette | `CMD+Shift+P` |
| Settings | `CMD+,` |

### Common Prompts

**Remind about rules:**
```
Follow .cursorrules strictly
```

**Check understanding:**
```
What are the critical rules from .cursorrules?
```

**Feature implementation:**
```
Implement [feature] following the 4-phase workflow from .cursorrules
```

**Code review:**
```
Review this code for patterns and pitfalls. Follow context gathering protocol.
```

---

## 🎉 You're All Set!

Your Cursor AI is now configured with:

✅ **Documentation approval rule** - Never creates docs without asking
✅ **4-phase workflow** - Structured approach to all features
✅ **Context gathering** - Understands codebase before changes
✅ **Cursor-specific optimizations** - Multi-tab, Composer, inline edits
✅ **Project context** - Knows about MCP Instructions Server

**Start using Cursor with confidence!**

---

## 📖 Related Documentation

- **[CURSOR_SETUP.md](CURSOR_SETUP.md)** - General Cursor setup guide
- **[CURSOR_PROMPTS.md](CURSOR_PROMPTS.md)** - Test prompts and examples
- **[SETUP_COMPARISON.md](SETUP_COMPARISON.md)** - Cursor vs Copilot comparison
- **[.cursorrules](../.cursorrules)** - The rules file itself

---

**Questions?** Test your setup with the prompts above, and if Cursor doesn't follow the rules, use the troubleshooting section!

