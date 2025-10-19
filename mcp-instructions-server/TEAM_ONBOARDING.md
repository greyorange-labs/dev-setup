# Team Onboarding Guide

**Quick setup guide for new team members to configure AI assistants with ~100% accuracy**

---

## 🎯 Overview

This repository provides standardized AI assistant configuration for the entire team:
- ✅ **~100% accuracy** through structured workflows
- ✅ **Consistent** development patterns across team
- ✅ **Error prevention** via stored rules and patterns
- ✅ **Documentation control** - AI always asks permission
- ✅ **Works with** both Cursor AI and GitHub Copilot

**Setup time:** 5-15 minutes per developer

---

## 🚀 Quick Start (Choose Your Tool)

### Option 1: Cursor AI (Recommended - Easiest!)

**Time: ~5 minutes**

1. **Clone repository:**
   ```bash
   git clone <your-repo-url>
   cd dev-setup
   ```

2. **Verify `.cursorrules` exists:**
   ```bash
   cat .cursorrules
   # Should show Cursor AI Rules
   ```

3. **Configure Cursor settings:**
   - Open Cursor → `CMD+Shift+P` → "Preferences: Open User Settings (JSON)"
   - Add these settings:
   ```json
   {
     "cursor.general.enableRulesFile": true,
     "cursor.chat.maxContextTokens": 100000,
     "cursor.composer.enableComposer": true,
     "cursor.chat.alwaysSearchWeb": false
   }
   ```

4. **Restart Cursor**

5. **Test:**
   ```
   Should you create documentation without asking me?
   ```
   Expected: "No, I must ask for approval first"

✅ **Done!** See [CURSOR_SETUP.md](CURSOR_SETUP.md) for details

---

### Option 2: GitHub Copilot

**Time: ~15 minutes**

1. **Clone repository:**
   ```bash
   git clone <your-repo-url>
   cd dev-setup/mcp-instructions-server
   ```

2. **Build MCP server:**
   ```bash
   npm install
   npm run build
   npm run seed
   ```

3. **Configure VS Code settings:**
   - Open VS Code → `CMD+Shift+P` → "Preferences: Open User Settings (JSON)"
   - Add this configuration:
   ```json
   {
     "github.copilot.chat.mcp.servers": {
       "instructions-server": {
         "command": "node",
         "args": [
           "/ABSOLUTE/PATH/TO/dev-setup/mcp-instructions-server/build/index.js"
         ],
         "env": {
           "VSCODE_CURRENT_FILE": "${file}"
         }
       }
     },

     "github.copilot.chat.codeGeneration.instructions": [
       {
         "text": "🚨 CRITICAL: NEVER create documentation without explicit approval. Always ask first."
       },
       {
         "text": "Follow 4-phase workflow: 1) Clarification, 2) Planning, 3) Implementation, 4) Review."
       },
       {
         "text": "ALWAYS gather context before changes: read existing code, understand patterns."
       }
     ]
   }
   ```

4. **Get absolute path:**
   ```bash
   cd /path/to/dev-setup/mcp-instructions-server
   pwd
   # Copy output and replace /ABSOLUTE/PATH/TO/ in settings
   ```

5. **Restart VS Code**

6. **Test:**
   ```
   @github Should you create documentation without asking me?
   ```
   Expected: "No, I must ask for approval first"

✅ **Done!** See [SETUP_FOR_NEW_USERS.md](SETUP_FOR_NEW_USERS.md) for details

---

### Option 3: Both Tools (Recommended for Flexibility!)

**Time: ~20 minutes**

Follow both guides above. You'll have:
- Cursor for quick, automatic rule loading
- Copilot for powerful MCP pattern queries
- Maximum flexibility across editors

---

## ✅ Verification Checklist

After setup, verify all these work:

### Test 1: Documentation Approval Rule ⭐ CRITICAL

**Prompt:**
```
Should you create a README file without asking me?
```

**Expected:** "No, I must always ask for explicit approval first"

- [ ] ✅ Cursor test passed
- [ ] ✅ Copilot test passed

---

### Test 2: 4-Phase Workflow

**Prompt:**
```
What workflow should you follow when implementing a feature?
```

**Expected:** Description of 4 phases (Clarification → Planning → Implementation → Review)

- [ ] ✅ Cursor test passed
- [ ] ✅ Copilot test passed

---

### Test 3: Practical Implementation

**Prompt:**
```
Add a helper function to format titles (capitalize and trim). Follow the workflow.
```

**Expected Behavior:**
1. Phase 1: Asks clarifying questions
2. Phase 2: Creates plan, asks "Should I create documentation?"
3. Phase 3: Implements after approval
4. Phase 4: Summarizes changes

- [ ] ✅ Cursor test passed
- [ ] ✅ Copilot test passed

---

### Test 4: Context Gathering

**Prompt:**
```
Before making changes, what should you do?
```

**Expected:** Describes context gathering protocol (read existing code, understand patterns, etc.)

- [ ] ✅ Cursor test passed
- [ ] ✅ Copilot test passed

---

## 🎯 What You Get

After setup, your AI assistant will:

### 1. **Always Ask Before Creating Documentation** 🚨
```
You: "Add authentication module"
AI:  [Creates code]
AI:  "Should I create documentation for this module?"
You: "No" → AI doesn't create docs ✅
```

### 2. **Follow 4-Phase Workflow** 📋
```
Phase 1: Clarification     → Asks questions, confirms understanding
Phase 2: Planning          → Creates detailed plan, asks about docs
Phase 3: Implementation    → Follows plan, applies patterns
Phase 4: Review            → Summarizes changes, provides evidence
```

### 3. **Gather Context First** 🔍
```
AI: [Reads existing similar code]
AI: [Understands project patterns]
AI: [Checks for dependencies]
AI: [Then suggests implementation following your patterns]
```

### 4. **Follow Language Patterns** 💡
- Erlang: OTP patterns, gen_server, error tuples
- Java: Spring Boot, JPA, DTOs, transaction management
- TypeScript: Type safety, async/await, error handling

### 5. **Avoid Common Pitfalls** ⚠️
- Erlang: Atom exhaustion, process leaks
- Java: N+1 queries, LazyInitializationException
- General: Premature optimization

**Result: ~100% accuracy, consistent code quality across team!**

---

## 📚 Team Resources

### Quick References

**For Cursor Users:**
- [CURSOR_SETUP.md](CURSOR_SETUP.md) - Complete setup guide
- [CURSOR_PROMPTS.md](CURSOR_PROMPTS.md) - Test prompts
- [CURSOR_LOCAL_SETUP_INSTRUCTIONS.md](CURSOR_LOCAL_SETUP_INSTRUCTIONS.md) - Detailed config

**For Copilot Users:**
- [SETUP_FOR_NEW_USERS.md](SETUP_FOR_NEW_USERS.md) - Complete setup guide
- [COPILOT_SETUP_COMMANDS.md](COPILOT_SETUP_COMMANDS.md) - Copy-paste commands
- [COPILOT_CONFIGURATION_GUIDE.md](COPILOT_CONFIGURATION_GUIDE.md) - Advanced config

**For Both:**
- [SETUP_COMPARISON.md](SETUP_COMPARISON.md) - Side-by-side comparison
- [HOW_TO_DEFINE_RULES.md](HOW_TO_DEFINE_RULES.md) - Adding new rules
- [README.md](README.md) - Technical reference

---

## 🎓 Common Use Cases

### Use Case 1: New Feature Implementation

**Request:**
```
Add user registration endpoint
```

**AI Response:**
```
Phase 1: Let me clarify...
- What fields? (email, password, username?)
- Validation rules?
- Email verification needed?
- Database: which tables?

[Waits for your confirmation]
```

**You provide details**

```
Phase 2: Implementation Plan
1. Create User entity (src/entities/User.java)
2. Add UserRepository
3. Create UserService with validation
4. Add POST /api/users endpoint
5. Write tests

Should I create documentation? [ASKS!]
```

**You say "No docs"**

```
Phase 3: [Implements following Java/Spring Boot patterns]

Phase 4: Summary
✅ User entity created with proper annotations
✅ Repository with custom queries
✅ Service with validation and error handling
✅ Controller with DTOs (not entities!)
✅ Tests passing

Next: Run `mvn test` to verify
```

---

### Use Case 2: Code Review

**Request:**
```
Review this code for issues:

[paste code]
```

**AI Response:**
```
[Reads similar code in project for context]
[Checks patterns and pitfalls database]

Review findings:

⚠️ Issue 1: [Specific problem]
Problem: [Why it's bad]
Solution: [How to fix]

✅ Good: [What's well done]

Suggested improvements:
1. [Improvement with code example]
2. [Improvement with code example]

[No documentation created, just inline suggestions]
```

---

### Use Case 3: Multi-File Refactoring

**[Cursor - Composer Mode]**
```
[CMD+I]

Refactor error handling across src/services/*.ts to use consistent format.
Follow .cursorrules workflow.
```

**AI Response:**
```
Phase 1: Clarification
- Standard format: { success: boolean, data?: T, error?: string }?
- Affect all 12 service files?
- Update tests too?

[Shows list of files to modify]
[Asks for confirmation]
```

**[Copilot - With @github]**
```
@github Refactor error handling across src/services/*.ts
```

**AI Response:**
```
[Loads TypeScript patterns from MCP]
[Checks error handling pitfalls]

Phase 1: [Same clarification as above]
```

---

## 🔧 Troubleshooting

### Issue: AI Not Following Rules

**Solution:**

**[Cursor]**
```
Follow .cursorrules strictly. Read the documentation approval rule before proceeding.
```

**[Copilot]**
```
@github Follow the workflow rules. Check documentation approval before proceeding.
```

---

### Issue: AI Creates Docs Without Asking

**This means setup isn't complete.**

**[Cursor]**
1. Check `.cursorrules` exists in workspace root
2. Verify `cursor.general.enableRulesFile: true` in settings
3. Restart Cursor
4. Test again

**[Copilot]**
1. Check global instructions in VS Code settings
2. Verify MCP server path is correct
3. Use `@github` prefix
4. Restart VS Code

---

### Issue: Different Behavior Than Expected

**Check:**
- [ ] Correct workspace directory
- [ ] Settings file updated
- [ ] Editor restarted
- [ ] Test prompts used correctly
- [ ] Using correct prefix (none for Cursor, `@github` for Copilot with MCP)

**Still not working?**

See tool-specific troubleshooting:
- [CURSOR_SETUP.md#troubleshooting](CURSOR_SETUP.md#🆘-troubleshooting)
- [GETTING_STARTED.md#troubleshooting](GETTING_STARTED.md#🆘-troubleshooting)

---

## 💡 Best Practices for Team

### 1. Always Test After Setup
Run all 4 verification tests before starting work

### 2. Remind AI When Needed
If AI deviates from rules:
- **Cursor**: "Follow .cursorrules"
- **Copilot**: "Follow the workflow rules"

### 3. Share Patterns
Found a useful pattern? Add it to the MCP database:
- Edit `instructions/*.json` files
- Run `npm run seed` to update
- Share with team via git

### 4. Review AI Suggestions
Even with ~100% accuracy:
- Read the proposed changes
- Verify tests pass
- Check for documentation creation
- Ensure patterns are followed

### 5. Update Rules Together
Team decisions on new patterns:
1. Discuss pattern with team
2. Add to appropriate `.json` file
3. Reseed database
4. Update `.cursorrules` if needed
5. Commit and push

---

## 📊 Team Setup Status

Track your team's setup progress:

| Team Member | Tool | Setup Complete | Tests Passed | Ready |
|-------------|------|:--------------:|:------------:|:-----:|
| Example     | Cursor | ✅ | ✅ | ✅ |
| You         | ?    | ⏳ | ⏳ | ⏳ |

---

## 🎉 Welcome to High-Accuracy Development!

You're now set up with:
- ✅ Standardized AI assistant configuration
- ✅ ~100% accuracy workflows
- ✅ Consistent development patterns
- ✅ Error prevention built-in
- ✅ Documentation control

**Questions?**
- Check tool-specific docs (links above)
- Ask in team chat
- Open an issue on the repository

**Happy coding with AI assistance that actually helps!** 🚀

---

## 📝 Quick Command Reference

### Cursor
```bash
# View rules
cat /path/to/.cursorrules

# Test
[Open Cursor Chat]
"Should you create docs without asking?"
```

### Copilot
```bash
# Check server
cd /path/to/mcp-instructions-server
node build/index.js

# Update patterns
npm run seed

# Test
[Open Copilot Chat]
"@github Should you create docs without asking?"
```

### Both
```bash
# Clone repo
git clone <repo-url>

# Check git status
git status

# Pull latest rules
git pull origin main
```

---

**Setup complete? Welcome to the team!** 👋

