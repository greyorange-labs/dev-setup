# GitHub Copilot Instructions for dev-setup

## 🚨 CRITICAL RULES - ALWAYS FOLLOW

### Documentation Approval Required (PRIORITY: CRITICAL)

**NEVER create any documentation files without explicit user approval.**

This applies to:
- README files
- Architecture documentation
- API documentation
- Design documents
- How-to guides
- Migration guides
- Any markdown (.md) files
- Wiki pages

#### Rules:

1. **Always Ask First**
   ```
   "Should I create documentation for this?"
   "Do you want me to add a README?"
   "Would you like documentation for this feature?"
   ```

2. **Wait for Explicit Approval**
   - Don't assume documentation is needed
   - Don't create docs "just in case"
   - User must explicitly say "yes" or "create it"

3. **Exceptions (Still Add Value)**
   You MAY add without asking:
   - Inline code comments (< 10 lines)
   - Function/method docstrings
   - Type annotations
   - Brief commit messages

#### Example Interactions:

**❌ WRONG:**
```
User: "Create a new utility module"
Copilot: [Creates code + README.md without asking]
```

**✅ CORRECT:**
```
User: "Create a new utility module"
Copilot: "I'll create the utility module. Should I also create documentation for it?"
User: "No, just the code"
Copilot: [Creates only code]
```

---

## 📋 4-Phase Workflow (PRIORITY: HIGH)

When implementing features, always follow this workflow:

### Phase 1: Requirement Clarification
- Restate the requirement in your own words
- Identify scope and affected components
- List assumptions
- Ask clarifying questions
- **Wait for confirmation before proceeding**

### Phase 2: Planning
- Gather context from existing code
- Create detailed implementation plan
- List files to create/modify
- Define testing strategy
- Identify risks
- **Get approval before implementation**

### Phase 3: Implementation
- Follow the approved plan
- Apply language-specific patterns
- Validate continuously (format, type-check, test)
- Document as you go (with approval for docs!)

### Phase 4: Review and Summary
- Summarize all changes
- Report validation results
- Provide testing evidence
- List next steps

---

## 🔍 Context Gathering (PRIORITY: HIGH)

**NEVER make changes without understanding the codebase.**

Before implementing:
1. Read existing similar implementations
2. Check for README files and inline documentation
3. Query the MCP Instructions Server (when using @github)
4. Understand dependencies and patterns

---

## 🎯 When Responding

- **Always check:** Does this response create documentation?
- **If yes:** Ask for approval first
- **If no:** Proceed normally

This applies to **all interactions**, whether using @github or not.

---

## ✅ Command and Code Validation (PRIORITY: CRITICAL)

**NEVER suggest commands, functions, or code without validating against official documentation.**

### The Rule:
Every command, function, API, or code snippet MUST be validated against official documentation before suggesting it.

### Validation Process:

1. **Identify Version**: Check the specific version being used (e.g., asdf 0.18.0)
2. **Find Official Source**: Official docs, GitHub repo, man pages, or `--help` output
3. **Verify Syntax**: Confirm the exact command/function exists and syntax is correct
4. **Check Version Compatibility**: Ensure feature exists in the specified version

### What to Validate:

- CLI commands (asdf, git, docker, npm, etc.)
- API methods and functions
- Configuration syntax
- Library/framework usage
- Language-specific features
- Package manager commands
- Version-specific features

### Examples:

**❌ WRONG:**
```
User: "Use asdf 0.18.0 to install Erlang"
Copilot: [Adds `asdf global erlang 27.1.2` without verification]
```

**✅ CORRECT:**
```
User: "Use asdf 0.18.0 to install Erlang"
Copilot: "Let me verify the asdf 0.18.0 commands first."
[Checks official docs or runs asdf --help]
Copilot: "Based on asdf 0.18.0 documentation, here are the commands..."
```

### When to Validate:

- User mentions specific versions
- User says "make sure you use the latest..."
- You're about to suggest a command you haven't verified
- You're copying patterns from other similar tools
- User questions your suggestion
- **ALWAYS, if you have ANY doubt**

### Validation Checklist:

Before suggesting ANY command/code:
- [ ] Read official documentation?
- [ ] Verified it works in specified version?
- [ ] Syntax is correct?
- [ ] Checked for deprecated alternatives?
- [ ] Can cite the source?

**If any answer is NO, validate first.**

### Remember:

**NEVER ASSUME. ALWAYS VALIDATE. BE 100% ACCURATE.**

If you don't know, admit it and look it up. Better to verify than give wrong information.
