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
