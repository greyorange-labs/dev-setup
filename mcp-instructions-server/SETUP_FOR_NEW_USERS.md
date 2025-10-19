# Quick Setup Guide for New Users

**Get started with the MCP Instructions Server in 5 minutes!**

---

## 🎯 What This Is

An **MCP (Model Context Protocol) server** that teaches GitHub Copilot your development workflow, coding patterns, and best practices. After setup, Copilot will:

- ✅ Follow your 4-phase workflow (clarify → plan → implement → review)
- ✅ Apply language-specific patterns (Erlang, Java, etc.)
- ✅ Warn about common pitfalls
- ✅ **Always ask before creating documentation**

---

## 🚀 Quick Setup (5 Steps)

### Step 1: Clone This Repository

```bash
git clone https://github.com/amarBitMan/dev-setup.git
cd dev-setup/mcp-instructions-server
```

### Step 2: Install Dependencies

**Requirements:**
- Node.js v18+ (or v20+)
- npm v9+

**Install Node.js (if needed):**

```bash
# macOS (Homebrew)
brew install node

# Ubuntu/Debian
sudo apt install nodejs npm

# Windows (Chocolatey)
choco install nodejs
```

**Install project dependencies:**

```bash
npm install
```

### Step 3: Build and Seed Database

```bash
# Build TypeScript
npm run build

# Seed instruction database
npm run seed
```

**Expected output:**
```
✨ Database seeding completed successfully!
📊 Total instructions: 34
```

### Step 4: Configure VS Code

#### A. Add MCP Server to VS Code (Required)

Open VS Code settings and add the MCP server configuration:

**macOS/Linux:**
```bash
code ~/.config/Code/User/settings.json
# or for macOS:
code ~/Library/Application\ Support/Code/User/settings.json
```

**Windows:**
```bash
code %APPDATA%\Code\User\settings.json
```

**Add this configuration:**

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
  }
}
```

**⚠️ IMPORTANT:** Replace `/ABSOLUTE/PATH/TO/` with your actual path!

**To get your absolute path:**
```bash
# Run this in the mcp-instructions-server directory:
pwd
# Copy the output and use it in the config above
```

#### B. Add Global Copilot Instructions (Recommended)

Add these to your VS Code settings to make Copilot follow rules **everywhere** (not just with `@github`):

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

**💡 Tip:** You can copy-paste the entire JSON block into Copilot Chat and ask:

```
Add these settings to my VS Code global configuration
```

### Step 5: Restart VS Code

Close **all VS Code windows** and reopen for changes to take effect.

---

## ✅ Verify Installation

### Test 1: MCP Server is Loaded

Open Copilot Chat and ask:

```
@github What are the workflow rules?
```

**Expected:** Copilot describes the 4-phase workflow (Clarification, Planning, Implementation, Review)

### Test 2: Documentation Approval Rule

Ask Copilot:

```
@github Should you create documentation without asking me first?
```

**Expected:** Copilot says **"No, I must always ask for approval first"**

### Test 3: Without @github (Global Instructions)

Ask without `@github`:

```
Add a new REST API endpoint for user management
```

**Expected:**
- Copilot asks clarifying questions (Phase 1)
- Copilot asks: **"Should I create documentation for this?"**

---

## 📊 What's Included

After setup, the MCP server contains **34 instructions**:

### Workflow Rules (7)
- ⭐ **Documentation Approval Required** (CRITICAL)
- Phase 1: Requirement Clarification
- Phase 2: Planning
- Phase 3: Implementation
- Phase 4: Review and Summary
- Context Gathering Protocol
- Error Prevention Checklist

### Coding Patterns (15)
- **Erlang/OTP (8):** Gen Server, error handling, type specs, testing
- **Java/Spring Boot (7):** REST controllers, JPA, DTOs, repositories

### Common Pitfalls (8)
- Erlang: Atom exhaustion, process leaks, ETS leaks
- Java: N+1 queries, LazyInitializationException, transaction issues

### Project Configs (4)
- Butler Server (Erlang)
- GreyMatter Platform (Java)

---

## 🎓 Usage Examples

### Example 1: Feature Implementation

**You:**
```
@github Add a gen_server for inventory tracking in the Butler Server project
```

**Copilot Phase 1 (Clarification):**
```
Let me clarify: You want to add a new gen_server behavior module
for tracking inventory in the Butler Server project.

Affected: apps/inventory/src/, supervisor tree
Assumptions: Using OTP patterns, PostgreSQL backend
Questions: What state should it maintain? Event-driven or polling?

Please confirm before planning.
```

**You:** "Confirmed. Event-driven."

**Copilot Phase 2 (Planning):**
```
Implementation Plan:

Files to Create:
1. apps/inventory/src/inventory_server.erl - gen_server implementation
2. apps/inventory/src/inventory_sup.erl - supervisor
3. test/inventory_server_tests.erl - EUnit tests

Should I create documentation for this module?  ⭐ ASKS PERMISSION
```

**You:** "No docs needed"

**Copilot Phase 3 (Implementation):** [Creates code following Erlang patterns]

**Copilot Phase 4 (Review):** [Summarizes changes and validation results]

### Example 2: Code Review

**You:**
```
@github Review this code for common pitfalls

handle_call({create_user, Name}, _From, State) ->
    UserId = list_to_atom(Name),
    {reply, {ok, UserId}, State}.
```

**Copilot:**
```
⚠️ WARNING: Atom Exhaustion Risk Detected!

Problem: You're using list_to_atom/1 with user input (Name).
This can exhaust the atom table and crash the VM.

Solution:
```erlang
% ✅ GOOD - Use existing atoms only
handle_call({create_user, Name}, _From, State) ->
    try
        UserId = list_to_existing_atom(Name),
        {reply, {ok, UserId}, State}
    catch
        error:badarg -> {reply, {error, invalid_name}, State}
    end.

% OR use binaries instead:
handle_call({create_user, Name}, _From, State) ->
    UserId = list_to_binary(Name),
    {reply, {ok, UserId}, State}.
```
```

---

## 🛠️ Customization

### Add Your Own Instructions

1. **Edit JSON files** in `instructions/`:
   - `workflow-rules.json` - Process rules
   - `erlang-patterns.json` - Erlang patterns
   - `java-patterns.json` - Java patterns
   - `common-pitfalls.json` - Anti-patterns
   - `project-configs.json` - Project configs

2. **Reload database:**
   ```bash
   cd /path/to/mcp-instructions-server
   rm instructions.db
   npm run seed
   ```

3. **Restart VS Code**

**See [HOW_TO_DEFINE_RULES.md](HOW_TO_DEFINE_RULES.md) for detailed guide.**

### Add New Languages

Example: Adding Python patterns

1. Create `instructions/python-patterns.json`:
   ```json
   [
     {
       "title": "Python Type Hints",
       "content": "## Pattern\n\n```python\ndef process(items: List[str]) -> dict[str, int]:\n    ...\n```",
       "category": "coding-patterns",
       "language": "python",
       "priority": 85,
       "tags": ["python", "types"]
     }
   ]
   ```

2. Update `src/seed.ts` to include the new file

3. Rebuild and reseed:
   ```bash
   npm run build
   rm instructions.db
   npm run seed
   ```

---

## 🆘 Troubleshooting

### Issue: MCP Server Not Loading

**Symptoms:** `@github` doesn't use instructions

**Solution:**
1. Verify MCP config in VS Code settings:
   ```bash
   # Check if configuration exists
   cat ~/Library/Application\ Support/Code/User/settings.json | grep -A 10 "mcp.servers"
   ```

2. Verify absolute path is correct:
   ```bash
   cd /path/to/mcp-instructions-server
   pwd
   # Use this exact path in VS Code settings
   ```

3. Test server manually:
   ```bash
   node build/index.js
   # Should start without errors (Ctrl+C to stop)
   ```

4. Restart VS Code (close ALL windows)

### Issue: Copilot Creates Docs Without Asking

**Solution:**

1. **Verify global instructions exist:**
   ```bash
   cat ~/Library/Application\ Support/Code/User/settings.json | grep -A 5 "codeGeneration.instructions"
   ```

2. **If missing, add them** (see Step 4B above)

3. **Restart VS Code**

4. **Remind Copilot:**
   ```
   Remember to follow the documentation approval rule
   ```

### Issue: Instructions Seem Outdated

**Solution:**
```bash
cd /path/to/mcp-instructions-server
rm instructions.db
npm run seed
# Restart VS Code
```

### Issue: Build Fails

**Check Node.js version:**
```bash
node --version  # Should be v18+ or v20+
npm --version   # Should be v9+
```

**Reinstall dependencies:**
```bash
rm -rf node_modules package-lock.json
npm install
npm run build
```

---

## 📖 Documentation

- **[GETTING_STARTED.md](GETTING_STARTED.md)** - Detailed user guide
- **[HOW_TO_DEFINE_RULES.md](HOW_TO_DEFINE_RULES.md)** - How to create instructions
- **[OPERATIONS.md](OPERATIONS.md)** - Database operations
- **[COPILOT_CONFIGURATION_GUIDE.md](COPILOT_CONFIGURATION_GUIDE.md)** - Advanced configuration
- **[README.md](README.md)** - Technical reference

---

## 🎯 Next Steps

After setup:

1. ✅ Test with `@github` in Copilot Chat
2. ✅ Test without `@github` to verify global instructions
3. 📝 Add your own project-specific instructions
4. 🔧 Customize patterns for your tech stack
5. 📚 Share with your team!

---

## 🤝 Contributing

Found a bug? Want to add patterns for a new language?

1. Fork this repository
2. Add your instructions/patterns
3. Submit a pull request

---

## 📄 License

MIT License - See [LICENSE](../LICENSE) for details.

---

## 💬 Support

- **Issues:** [GitHub Issues](https://github.com/amarBitMan/dev-setup/issues)
- **Documentation:** See docs linked above
- **Questions:** Open a discussion on GitHub

---

**You're all set! 🎉**

Start using Copilot with `@github` and watch it follow your workflow perfectly!
