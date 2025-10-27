# MCP Instructions Server

A production-ready Model Context Protocol (MCP) server that enforces disciplined AI coding workflows by serving global instruction rules to GitHub Copilot, Cursor AI, and other AI assistants. Reduces ~90% of common AI coding mistakes by requiring proper requirement clarification, planning, and validation before implementation.

---

## 📚 Documentation Structure

**Not sure which file to read? See [DOCS_INDEX.md](DOCS_INDEX.md) for a quick guide.**

**This repository has FOUR documentation files:**

1. **[DOCS_INDEX.md](DOCS_INDEX.md)** - Which doc to read for what purpose (start here!)
2. **README.md** (this file) - Complete setup guide, configuration, troubleshooting, FAQ
3. **[global-rules.md](global-rules.md)** - Detailed AI assistant rules with examples
4. **[PROJECT_STRUCTURE.md](PROJECT_STRUCTURE.md)** - File organization and maintenance guide

**That's it! No duplicate info. Each file has a clear purpose.**

---

## ⚡ Quick Start (Already Installed)

**Your system is already configured!** The MCP server is installed at:
```
/Users/amar.c/workspace/dev-setup/coding-agent-setup
```

**Next step: Restart VS Code**
1. Quit VS Code completely (⌘+Q on Mac)
2. Wait 5 seconds
3. Reopen VS Code

**Test it works:**
Open GitHub Copilot Chat and try: `"Create a new authentication module for the API"`

The AI should:
- ✓ Restate understanding → List assumptions → Ask questions → Wait for confirmation
- ✓ Create detailed plan (no code) → Ask for approval
- ✓ Only then implement → Ask permission for docs/tests

**If AI jumps straight to code:** See [Troubleshooting](#troubleshooting) section below.

---

## What This Is

This MCP server loads and serves a comprehensive set of coding rules (`global-rules.md`) that force AI assistants to follow a disciplined 6-phase workflow before writing code. Instead of jumping straight to implementation, AI assistants must clarify requirements, create detailed plans, validate existing patterns, and ask permission before creating documentation or tests.

The server provides these rules through both file-based loading (`.github/copilot-instructions.md`, `.cursorrules`) and MCP resource serving (`instructions://global-rules`) for maximum compatibility and reliability.

---

## Quick Setup for New Developers

### Prerequisites
- **Node.js 18+** - Check with `node --version`
- **VS Code or Cursor** - For AI assistant integration
- **GitHub Copilot subscription** - Required for Copilot features

### Installation Steps

1. **Clone the repository**
   ```bash
   git clone <repository-url>
   cd mcp-instructions-server
   ```

2. **Install dependencies**
   ```bash
   npm install
   ```

3. **Build the server**
   ```bash
   npm run build
   ```

4. **Configure VS Code settings**

   Add this to your VS Code `settings.json` file (`~/Library/Application Support/Code/User/settings.json` on macOS):

   ```json
   {
     "github.copilot.chat.codeGeneration.useInstructionFiles": true,
     "github.copilot.chat.mcp.servers": {
       "instructions": {
         "command": "node",
         "args": [
           "/ABSOLUTE/PATH/TO/mcp-instructions-server/build/index.js"
         ],
         "env": {
           "VSCODE_CURRENT_FILE": "${file}"
         }
       }
     }
   }
   ```

   **⚠️ IMPORTANT:** Replace `/ABSOLUTE/PATH/TO/mcp-instructions-server` with your actual full path to this project.

5. **Optional: Global setup**

   To apply rules to ALL workspaces (not just this project):
   ```bash
   # Copy rules to global locations
   cp .github/copilot-instructions.md ~/.github/copilot-instructions.md
   cp .cursorrules ~/.cursorrules
   ```

6. **Restart your editor**

   **⚠️ CRITICAL:** Close ALL VS Code/Cursor windows completely (⌘+Q on Mac), then reopen. Simply reloading windows is not enough.

---

## Verify It's Working

Test with this prompt in GitHub Copilot Chat or Cursor:

**"Create a new authentication module for the API"**

The AI should respond with these **8 steps in order**:

1. ✅ **Restate understanding** - "Let me clarify what I understand about this authentication module..."
2. ✅ **List assumptions** - "My assumptions: JWT-based auth, email/password, etc."
3. ✅ **Ask clarifying questions** - "Questions: What auth method? Required fields? Password reset?"
4. ✅ **Wait for confirmation** - "Please confirm before I create the implementation plan"
5. ✅ **Create detailed plan** - After you confirm, shows plan with no code
6. ✅ **Ask for plan approval** - "Does this plan look good?"
7. ✅ **Implement only after approval** - Code appears only after you approve the plan
8. ✅ **Ask about docs/tests** - "Would you like documentation? Would you like tests?"

**🚨 If AI skips ANY of these steps → Rules not loading, see Troubleshooting section**

---

## Making Changes to Rules

### ⚠️ DO RULES AUTO-RELOAD? **NO - YOU MUST RESTART EDITOR**

When you edit rules, they **DO NOT** automatically reload. You must restart your editor to see changes.

### Steps to Update Rules

1. **Edit the rules file**
   ```bash
   # Edit the main rules file
   nano global-rules.md

   # OR edit the GitHub Copilot version
   nano .github/copilot-instructions.md
   ```

2. **Close ALL editor windows**
   ```bash
   # On macOS: ⌘+Q (Quit entirely, don't just close windows)
   # On Windows/Linux: Alt+F4 or File → Exit
   ```

3. **Reopen your editor**

   Your updated rules are now active.

### For MCP Server Code Changes

If you modify TypeScript files in `src/`:

1. **Edit the code**
2. **Rebuild the server**
   ```bash
   npm run build
   ```
3. **Restart your editor** (⌘+Q then reopen)

The MCP server code changes require both rebuilding AND editor restart.

---

## Team Workflow - Sharing Rule Changes

### How Team Members Get Latest Rules

```bash
# Simple: Pull changes and restart editor
git pull
# Close all editor windows (⌘+Q)
# Reopen editor
# Rules are now updated
```

### How YOU Share Rule Changes

```bash
# 1. Edit rules
nano global-rules.md

# 2. Test locally (restart editor, test with AI)

# 3. Commit and push
git add global-rules.md .github/copilot-instructions.md
git commit -m "Add new rule for API validation"
git push

# 4. Tell team
echo "New rules pushed - everyone please 'git pull' and restart your editor"
```

### Organization-Wide Setup

For entire engineering organizations:

1. **Admin forks this repo** to your organization account
2. **Team members clone** from the org repo (not this original)
3. **Everyone's VS Code** points to their local clone path in settings.json
4. **When rules update:** Everyone does `git pull` + restart editor
5. **Consistent rules** across all developers and projects

---

## Repository Structure

For complete file-by-file breakdown, see [PROJECT_STRUCTURE.md](PROJECT_STRUCTURE.md).

**Quick overview:**
```
mcp-instructions-server/
├── README.md                    ⭐ This file - setup, config, troubleshooting
├── global-rules.md              ⭐ AI assistant rules (what AI follows)
├── PROJECT_STRUCTURE.md         → Detailed file guide
├── .github/copilot-instructions.md  → Auto-loads in GitHub Copilot
├── .cursorrules                 → Auto-loads in Cursor AI
├── src/                         → MCP server source code
├── build/                       → Compiled JavaScript (auto-generated)
└── instructions/                → Optional JSON templates
```

**Key principle:** Two main docs (README.md + global-rules.md), everything else is implementation.

---

## Contributing Rules

### Adding New Rules

1. **Edit global-rules.md**
   ```bash
   nano global-rules.md
   ```

2. **Update companion files**
   ```bash
   # Keep GitHub Copilot version in sync
   nano .github/copilot-instructions.md

   # Update Cursor version if needed
   nano .cursorrules
   ```

3. **Test your changes**
   ```bash
   # Restart editor (⌘+Q then reopen)
   # Test with AI prompts
   # Verify AI follows new rules
   ```

4. **Commit with clear message**
   ```bash
   git add global-rules.md .github/copilot-instructions.md
   git commit -m "Add Rule 7: No hardcoded secrets in code

   - AI must ask about environment variables
   - Must validate against security best practices
   - Include examples of secure vs insecure patterns"
   ```

### Rule Writing Guidelines

**Good Rule (Specific, Actionable):**
```markdown
## Rule X: Database Query Validation (MANDATORY)

Before generating database queries, you MUST:
1. Ask about SQL injection prevention measures
2. Validate against existing ORM patterns in codebase
3. Use parameterized queries, never string concatenation

Example: "I see you need a user lookup query. Should I use your existing ORM (detected: Prisma) or write raw SQL? For raw SQL, I'll use parameterized queries to prevent injection."
```

**Bad Rule (Vague, Unenforceable):**
```markdown
## Rule X: Write Good Database Code

Make sure database code is secure and follows best practices.
```

---

## Troubleshooting

### Rules Not Loading (GitHub Copilot)

**Problem:** AI jumps straight to code without clarification

**Solutions:**

1. **Check useInstructionFiles setting**
   ```bash
   # Verify this is in your settings.json:
   grep -A 2 "useInstructionFiles" ~/Library/Application\ Support/Code/User/settings.json
   # Should show: "github.copilot.chat.codeGeneration.useInstructionFiles": true
   ```

2. **Verify MCP server path**
   ```bash
   # Check if your absolute path is correct
   ls -la /your/absolute/path/mcp-instructions-server/build/index.js
   # Should exist and be executable
   ```

3. **Test MCP server manually**
   ```bash
   cd /your/absolute/path/mcp-instructions-server
   timeout 2 node build/index.js
   # Should show: "MCP Instructions Server started successfully"
   ```

4. **Check VS Code MCP logs**
   ```bash
   # Open VS Code Command Palette (⌘+Shift+P)
   # Type: "Developer: Show Logs"
   # Select "Model Context Protocol"
   # Look for connection errors
   ```

5. **Complete reset**
   ```bash
   # Quit VS Code entirely (⌘+Q)
   # Wait 5 seconds
   # Reopen VS Code
   # Try test prompt again
   ```

### Rules Not Loading (Cursor)

**Problem:** Cursor AI ignores the 6-phase workflow

**Solutions:**

1. **Verify .cursorrules exists**
   ```bash
   ls -la .cursorrules
   # Should show the file in project root
   ```

2. **Check global .cursorrules**
   ```bash
   ls -la ~/.cursorrules
   # Should exist if you want global rules
   ```

3. **Restart Cursor completely**
   ```bash
   # Quit Cursor entirely
   # Wait 5 seconds
   # Reopen Cursor in project directory
   ```

4. **Test with simple prompt**
   ```
   "Add logging to this function"
   # Should ask for clarification first
   ```

### MCP Server Won't Build

**Problem:** `npm run build` fails

**Solutions:**

1. **Check Node.js version**
   ```bash
   node --version
   # Must be 18.0.0 or higher
   ```

2. **Clear dependencies and reinstall**
   ```bash
   rm -rf node_modules package-lock.json
   npm install
   npm run build
   ```

3. **Check TypeScript errors**
   ```bash
   npx tsc --noEmit
   # Shows compilation errors without building
   ```

### AI Still Ignoring Rules

**Problem:** Rules load but AI doesn't follow them consistently

**Workaround for unstable `useInstructionFiles`:**

The `useInstructionFiles` feature is very new (October 2025) and may not be fully stable. If GitHub Copilot ignores rules:

1. **Manual rule reminder**
   ```
   Before you write any code:
   1. Restate what you understand
   2. List your assumptions
   3. Ask clarifying questions
   4. Wait for my confirmation
   ```

2. **Use MCP directly**
   ```
   @github Use instructions://global-rules and follow all 6 phases
   ```

3. **Reference the rules explicitly**
   ```
   Follow the rules in global-rules.md - start with requirement clarification
   ```

---

## FAQ

### Do I need to install the MCP server in every project?
**No.** Install once globally. The MCP server path in your VS Code settings.json points to one installation, and it works for all projects. Optionally copy `.github/copilot-instructions.md` and `.cursorrules` to individual projects for project-specific rules.

### Can I have project-specific rules?
**Yes.** Add `.github/copilot-instructions.md` or `.cursorrules` to any project. These files are automatically loaded and can contain project-specific rules that supplement the global rules from this MCP server.

### **Do rules auto-reload when I edit them?**
**NO.** This is the most important FAQ answer. Rules do NOT automatically reload. You MUST completely restart your editor (⌘+Q then reopen) after editing any rule files. Simply reloading a window is not enough.

### How do I share rule updates with my team?
**Simple workflow:**
1. You: Edit rules → `git commit` → `git push` → Tell team "new rules, please git pull and restart"
2. Team: `git pull` → Restart editor (⌘+Q then reopen) → Rules updated

### Can I use this with Claude Desktop or other AI tools?
**Yes.** MCP is an open standard. Claude Desktop and other MCP-compatible AI tools can connect to this server. Add the server configuration to their MCP settings with the same command/args as VS Code.

### Does this work for Cursor Composer?
**Yes.** Cursor Composer loads `.cursorrules` automatically. The rules will apply to both Cursor Chat and Composer. Make sure to restart Cursor after editing rules.

### What if I don't want the MCP server complexity?
**For Cursor users:** You can skip the MCP server entirely. Cursor works perfectly with just the `.cursorrules` file. Copy rules from `global-rules.md` into `.cursorrules` and restart Cursor.

**For Copilot users:** The MCP server provides more reliability, but you can try with just `.github/copilot-instructions.md` if the `useInstructionFiles` feature works consistently for you.

### How do I update to newer versions of this repo?
```bash
# Add original repo as upstream (one-time setup)
git remote add upstream https://github.com/original-repo/mcp-instructions-server.git

# Get latest changes
git fetch upstream
git merge upstream/main

# Or rebase to keep clean history
git rebase upstream/main
```

### Can I modify the rules for my organization's specific needs?
**Absolutely.** Fork this repository, modify `global-rules.md` for your needs, and have your team clone from your fork. Common customizations:
- Add language-specific rules (Python, TypeScript, etc.)
- Include security/compliance requirements
- Add code review standards
- Include architecture patterns

### What happens if the MCP server crashes?
The file-based rules (`.github/copilot-instructions.md`, `.cursorrules`) continue working. The MCP server is an additional layer of reliability, not a single point of failure.

### Do the rules work with VS Code extensions other than Copilot?
The `.github/copilot-instructions.md` file is specific to GitHub Copilot. However, any VS Code extension that supports MCP can connect to this server. The rules themselves are general enough to apply to any AI coding assistant.

---

**Version:** 1.0.0
**Last Updated:** October 27, 2025
**License:** MIT