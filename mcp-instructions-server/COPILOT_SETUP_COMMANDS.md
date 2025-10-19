# Copilot Chat Setup Commands

**Quick commands to paste into Copilot Chat for easy setup**

---

## 🚀 One-Click Setup Command

After cloning this repo, paste this into GitHub Copilot Chat:

```
Help me set up the MCP Instructions Server. I've cloned the repo to my local machine. Please:

1. Guide me through installing Node.js and npm if needed
2. Help me run npm install, npm run build, and npm run seed
3. Add the MCP server configuration to my VS Code settings (get the absolute path from the current workspace)
4. Add the global GitHub Copilot instructions for documentation approval and 4-phase workflow
5. Remind me to restart VS Code
6. Help me verify the setup works

Use the SETUP_FOR_NEW_USERS.md guide as reference.
```

---

## 📝 Individual Setup Commands

If you prefer step-by-step, paste these one at a time:

### 1. Check Prerequisites

```
Check if I have Node.js v18+ and npm v9+ installed. If not, guide me through installation for my OS.
```

### 2. Install Dependencies

```
I'm in the mcp-instructions-server directory. Help me run npm install, then npm run build, then npm run seed. Explain what each command does.
```

### 3. Configure VS Code MCP Server

```
Add the MCP server configuration to my VS Code global settings. The absolute path to this workspace is: [paste your path here]. Use the instructions from SETUP_FOR_NEW_USERS.md Step 4A.
```

**To get your path:**
```bash
pwd  # Run this in mcp-instructions-server directory
```

### 4. Add Global Copilot Instructions

```
Add these global GitHub Copilot instructions to my VS Code settings:

1. CRITICAL: Never create documentation without asking
2. Follow 4-phase workflow (clarify, plan, implement, review)
3. Always gather context before changes
4. Ask first when in doubt about creating files

Use the exact JSON format from SETUP_FOR_NEW_USERS.md Step 4B.
```

### 5. Verify Setup

```
@github What are the workflow rules? (This should describe the 4-phase workflow)
```

```
@github Should you create documentation without asking me? (This should say "No")
```

---

## 🎯 Quick Customization Commands

After setup, use these to customize:

### Add New Language Instructions

```
@github Help me add Python coding patterns to the MCP instructions database. Create a python-patterns.json file with common patterns like type hints, async/await, and dataclasses. Then update src/seed.ts to include it.
```

### Add Project-Specific Rules

```
@github Add my project "[Project Name]" to the MCP server. It's located at [path] and uses [tech stack]. Create appropriate instructions in project-configs.json and update src/index.ts to detect it.
```

### View Current Instructions

```
@github Show me all instructions in the database grouped by category
```

### Search Instructions

```
@github Search the instructions database for patterns related to [topic]
```

---

## 🔧 Troubleshooting Commands

### MCP Server Not Loading

```
@github The MCP server isn't loading. Help me:
1. Verify the configuration in my VS Code settings
2. Check if the absolute path is correct
3. Test the server manually by running: node build/index.js
4. Debug any errors
```

### Copilot Not Following Rules

```
@github Copilot isn't following the rules (like asking before creating docs). Help me:
1. Verify global instructions are in VS Code settings
2. Check if workspace instructions exist in .github/copilot-instructions.md
3. Verify the MCP server database has the rules
4. Suggest what might be wrong
```

### Database Issues

```
@github The instruction database seems corrupted or outdated. Help me:
1. Backup the current database
2. Delete and reseed with: rm instructions.db && npm run seed
3. Verify the instructions loaded correctly
```

---

## 📚 Learning Commands

### Understand the System

```
@github Explain how the MCP Instructions Server works. What's the difference between workspace instructions, global instructions, and MCP server instructions? When is each used?
```

### Learn to Create Rules

```
@github I want to create a new instruction rule. Walk me through:
1. Understanding the rule structure (title, content, category, priority, etc.)
2. Deciding the right priority level
3. Choosing the right category
4. Writing effective content with examples
Use HOW_TO_DEFINE_RULES.md as reference.
```

---

## 🎓 Example Usage Commands

### Request Feature Implementation

```
@github Add a new REST API endpoint for user authentication in my [language/framework] project. Follow the workflow rules and ask about documentation.
```

### Code Review

```
@github Review this code for common pitfalls and anti-patterns:

[paste your code]
```

### Get Pattern Suggestions

```
@github What are the recommended patterns for [specific task] in [language]?
```

---

## 💡 Pro Tips

### Always Use @github for MCP Features

When you want Copilot to use the MCP instructions, **always start with `@github`**:

✅ `@github Add a feature...`
❌ `Add a feature...` (won't use MCP)

### Remind Copilot of Specific Rules

If Copilot forgets a rule, remind it:

```
@github Remember to follow the documentation approval rule - always ask before creating docs
```

### Check What Copilot Knows

```
@github What instructions do you have about [topic]?
```

### Update Instructions Live

You can ask Copilot to update instructions:

```
@github Add a new instruction to the database about [topic] with priority 90
```

---

## 🚀 Quick Start Checklist

Copy this checklist and paste into a text file to track your progress:

```
MCP Instructions Server Setup Checklist

Prerequisites:
[ ] Node.js v18+ installed
[ ] npm v9+ installed
[ ] Git installed
[ ] VS Code installed with GitHub Copilot extension

Setup Steps:
[ ] Cloned repository
[ ] Ran npm install
[ ] Ran npm run build
[ ] Ran npm run seed (34 instructions loaded)
[ ] Added MCP server config to VS Code settings (with absolute path)
[ ] Added global Copilot instructions to VS Code settings
[ ] Restarted VS Code (closed ALL windows)

Verification:
[ ] Test 1: @github What are the workflow rules? (Shows 4 phases)
[ ] Test 2: @github Should you create docs without asking? (Says no)
[ ] Test 3: Ask for a feature without @github (Asks about documentation)

Customization (Optional):
[ ] Added my project to project-configs.json
[ ] Added my preferred language patterns
[ ] Customized workflow rules for my team

Done! 🎉
```

---

## 📞 Getting Help

If you get stuck, paste this:

```
@github I'm having trouble setting up the MCP Instructions Server. Here's my situation:

- Step I'm on: [describe]
- What I tried: [describe]
- Error/issue: [paste error or describe issue]
- My OS: [macOS/Windows/Linux]
- Node version: [run: node --version]
- npm version: [run: npm --version]

Please help me troubleshoot using SETUP_FOR_NEW_USERS.md and COPILOT_CONFIGURATION_GUIDE.md as reference.
```

---

**Save this file for quick reference!** 📌

All these commands are designed to work with GitHub Copilot Chat for the easiest setup experience.
