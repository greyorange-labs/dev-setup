# How to Define Rules for the MCP Instructions Server

**A comprehensive guide to creating effective instructions for GitHub Copilot**

---

## 📋 Table of Contents

1. [Rule Anatomy](#rule-anatomy)
2. [Rule Fields Explained](#rule-fields-explained)
3. [Priority System](#priority-system)
4. [Categories](#categories)
5. [Content Structure](#content-structure)
6. [Examples by Type](#examples-by-type)
7. [Best Practices](#best-practices)
8. [Testing Your Rules](#testing-your-rules)

---

## 🧬 Rule Anatomy

Every rule is a JSON object with this structure:

```json
{
  "title": "Short Descriptive Title",
  "content": "## Detailed Markdown Content\n\nYour instructions here...",
  "category": "workflow-rules",
  "language": "erlang",
  "project": "butler-server",
  "tags": ["tag1", "tag2"],
  "priority": 95
}
```

**Example - The Documentation Approval Rule:**

```json
{
  "title": "Documentation Approval Required",
  "content": "## CRITICAL: Always Ask Before Creating Documentation\n\n**NEVER create any documentation files without explicit user approval.**\n\nThis applies to:\n- README files\n- Architecture documentation\n...",
  "category": "workflow-rules",
  "priority": 100,
  "tags": ["documentation", "approval", "critical", "permissions"]
}
```

---

## 📝 Rule Fields Explained

### 1. **`title`** (Required)
- **Type:** String
- **Purpose:** Short, searchable name for the rule
- **Length:** 3-10 words
- **Format:** Title Case preferred
- **Examples:**
  - ✅ "Documentation Approval Required"
  - ✅ "Phase 1: Requirement Clarification"
  - ✅ "Gen Server Pattern"
  - ❌ "This is a really long title that explains everything" (too long)
  - ❌ "rule" (too vague)

### 2. **`content`** (Required)
- **Type:** String (Markdown format)
- **Purpose:** Full instruction details
- **Format:** Markdown with `\n` for newlines
- **Structure:** See [Content Structure](#content-structure)
- **Length:** As long as needed (typically 100-500 lines)

### 3. **`category`** (Required)
- **Type:** String
- **Purpose:** Group related instructions
- **Options:**
  - `workflow-rules` - Process and methodology rules
  - `coding-patterns` - Language-specific patterns
  - `component-guidelines` - Component-specific rules
  - `pitfalls` - Anti-patterns and mistakes to avoid
  - `project-configs` - Project-specific configurations
  - `testing-strategies` - Testing approaches
  - `architecture-patterns` - High-level design patterns

### 4. **`language`** (Optional)
- **Type:** String or `null`
- **Purpose:** Language-specific instructions
- **Format:** Lowercase
- **Examples:** `"erlang"`, `"java"`, `"python"`, `"typescript"`, `null`
- **When to use:**
  - Use for coding patterns: `"language": "erlang"`
  - Omit for workflow rules: `"language": null` or omit field

### 5. **`project`** (Optional)
- **Type:** String or `null`
- **Purpose:** Project-specific instructions
- **Format:** Kebab-case
- **Examples:** `"butler-server"`, `"greymatter-platform"`, `null`
- **When to use:**
  - Use for project configs: `"project": "butler-server"`
  - Omit for general rules: `"project": null` or omit field

### 6. **`tags`** (Optional)
- **Type:** Array of strings
- **Purpose:** Additional searchability and filtering
- **Format:** Lowercase, kebab-case
- **Examples:** `["documentation", "approval", "critical"]`
- **Best practices:**
  - 3-5 tags per instruction
  - Use for cross-cutting concerns
  - Include priority indicators: `"critical"`, `"important"`

### 7. **`priority`** (Required)
- **Type:** Integer (0-100)
- **Purpose:** Determines which rules Copilot prioritizes
- **Scale:**
  - **100**: CRITICAL - Must always follow (e.g., security, permissions)
  - **95-99**: High priority - Core workflow rules
  - **90-94**: Important - Key patterns and guidelines
  - **80-89**: Standard - Regular coding patterns
  - **70-79**: Helpful - Best practices
  - **< 70**: Nice to have - Suggestions

---

## 🎯 Priority System

### How Priority Works

**Higher priority = Copilot follows it more strictly**

When conflicts occur, higher priority wins:

```json
// Priority 100 - ALWAYS enforced
{
  "title": "Documentation Approval Required",
  "priority": 100,
  "content": "NEVER create docs without asking"
}

// Priority 80 - Usually followed, but overridden by priority 100
{
  "title": "Always Add README to Modules",
  "priority": 80,
  "content": "Every module should have a README"
}
```

**Result:** Copilot won't auto-create README (priority 100 blocks it), but will ASK if you want one (respects both rules).

### Priority Guidelines

**Priority 100 (CRITICAL):**
- Security rules
- Permission rules
- Data protection
- Critical business logic

**Examples:**
```json
{
  "title": "Documentation Approval Required",
  "priority": 100,
  "content": "NEVER create docs without explicit approval"
}

{
  "title": "Never Expose User Passwords",
  "priority": 100,
  "content": "Always hash passwords, never log them"
}
```

**Priority 95-99 (Core Workflow):**
- 4-phase workflow rules
- Required processes
- Mandatory validation steps

**Priority 90-94 (Important Patterns):**
- Language-specific patterns
- Critical anti-patterns
- Testing requirements

**Priority 80-89 (Standard Practices):**
- Coding conventions
- Best practices
- Performance optimizations

**Priority < 80 (Suggestions):**
- Style preferences
- Optional improvements

---

## 📂 Categories

### 1. **`workflow-rules`**

**Purpose:** How Copilot should work with you

**Examples:**
- 4-phase workflow (clarification, planning, implementation, review)
- Context gathering protocol
- Documentation approval rules
- Error prevention checklists

**When to use:**
- Process requirements
- Interaction patterns
- Validation procedures
- Permission rules

**Template:**
```json
{
  "title": "Phase X: [Phase Name]",
  "content": "## [Phase Name]\n\nYou MUST:\n1. [Step]\n2. [Step]...",
  "category": "workflow-rules",
  "priority": 95,
  "tags": ["phase-x", "workflow"]
}
```

### 2. **`coding-patterns`**

**Purpose:** Language-specific best practices

**Examples:**
- Erlang: Gen Server pattern
- Java: Spring Boot REST controller pattern
- Python: Async/await pattern

**When to use:**
- Language syntax patterns
- Framework conventions
- Idiomatic code structure

**Template:**
```json
{
  "title": "[Pattern Name]",
  "content": "## Overview\n\n[Description]\n\n## Pattern\n```[lang]\n[code]\n```\n\n## Benefits\n- [Benefit]...",
  "category": "coding-patterns",
  "language": "erlang",
  "priority": 85,
  "tags": ["erlang", "otp", "pattern"]
}
```

### 3. **`pitfalls`**

**Purpose:** Common mistakes to avoid

**Examples:**
- Erlang: Atom exhaustion
- Java: N+1 query problem
- JavaScript: Memory leaks with closures

**When to use:**
- Anti-patterns
- Common bugs
- Performance traps
- Security vulnerabilities

**Template:**
```json
{
  "title": "[Language]: [Pitfall Name]",
  "content": "## ⚠️ Warning: [Pitfall]\n\n**Problem:**\n[Description]\n\n**Why It's Bad:**\n- [Reason]\n\n**Solution:**\n[Fix]\n\n**Example:**\n```[lang]\n// ❌ BAD\n[bad code]\n\n// ✅ GOOD\n[good code]\n```",
  "category": "pitfalls",
  "language": "java",
  "priority": 90,
  "tags": ["java", "performance", "warning"]
}
```

### 4. **`project-configs`**

**Purpose:** Project-specific setup and conventions

**Examples:**
- Butler Server overview
- GreyMatter Platform conventions
- Monorepo structure

**When to use:**
- Project structure
- Build commands
- Deployment procedures
- Team conventions

**Template:**
```json
{
  "title": "[Project Name] Overview",
  "content": "## Project: [Name]\n\n**Structure:**\n[Directories]\n\n**Conventions:**\n- [Convention]\n\n**Commands:**\n```bash\nmake build\nmake test\n```",
  "category": "project-configs",
  "project": "my-project",
  "priority": 90,
  "tags": ["project", "config"]
}
```

### 5. **`testing-strategies`**

**Purpose:** How to test code

**Examples:**
- EUnit testing in Erlang
- JUnit + Mockito in Java
- Integration testing strategies

**Template:**
```json
{
  "title": "[Language] Testing Pattern",
  "content": "## Testing Strategy\n\n**Unit Tests:**\n[Pattern]\n\n**Integration Tests:**\n[Pattern]\n\n**Example:**\n```[lang]\n[test code]\n```",
  "category": "testing-strategies",
  "language": "erlang",
  "priority": 85,
  "tags": ["testing", "erlang", "eunit"]
}
```

---

## 📖 Content Structure

### Best Practices for `content` Field

#### 1. **Use Clear Hierarchy**

```markdown
## Main Section

### Subsection

**Key Point:**
- Detail 1
- Detail 2
```

#### 2. **Include Examples**

```markdown
## Pattern

**Example:**
```erlang
handle_call({get_user, UserId}, _From, State) ->
    case user_db:lookup(UserId) of
        {ok, User} -> {reply, {ok, User}, State};
        {error, Reason} -> {reply, {error, Reason}, State}
    end.
```
```

#### 3. **Use Visual Indicators**

```markdown
✅ DO THIS
❌ DON'T DO THIS
⚠️ WARNING
💡 TIP
🔒 SECURITY
```

#### 4. **Provide Before/After**

```markdown
// ❌ BAD - Atom exhaustion risk
list_to_atom(UserInput)

// ✅ GOOD - Use existing atoms
list_to_existing_atom(UserInput)
```

#### 5. **Format for Scanning**

- **Bold** for emphasis
- Lists for steps
- Code blocks for examples
- Short paragraphs (2-3 lines)

### Complete Content Template

```markdown
## Overview

Brief description of the rule or pattern.

## Why This Matters

- Reason 1
- Reason 2

## The Rule/Pattern

**Do this:**
```[language]
[code example]
```

**Not this:**
```[language]
[anti-pattern]
```

## When to Apply

- Use case 1
- Use case 2

## Exceptions

When you can break this rule:
- Exception 1
- Exception 2

## Example Interactions

**User:** "Add feature X"
**Copilot:** [Expected behavior]

## Related Rules

- See also: [Other Rule Name]
```

---

## 🎨 Examples by Type

### Example 1: Critical Workflow Rule

```json
{
  "title": "Documentation Approval Required",
  "content": "## CRITICAL: Always Ask Before Creating Documentation\n\n**NEVER create any documentation files without explicit user approval.**\n\nThis applies to:\n- README files\n- Architecture documentation\n- API documentation\n\n### Rules:\n\n1. **Always Ask First**\n   ```\n   \"Should I create documentation for this feature?\"\n   ```\n\n2. **Wait for Explicit Approval**\n   - Don't assume documentation is needed\n   - User must explicitly say \"yes\"\n\n3. **Exceptions (Still Add Value)**\n   You MAY add without asking:\n   - Inline code comments (< 10 lines)\n   - Function/method docstrings\n\n### Example Interactions:\n\n**❌ WRONG:**\n```\nUser: \"Add auth module\"\nCopilot: [Creates code + README]\n```\n\n**✅ CORRECT:**\n```\nUser: \"Add auth module\"\nCopilot: \"Should I create documentation?\"\nUser: \"No\"\nCopilot: [Creates only code]\n```",
  "category": "workflow-rules",
  "priority": 100,
  "tags": ["documentation", "approval", "critical", "permissions"]
}
```

### Example 2: Coding Pattern

```json
{
  "title": "Erlang Gen Server Pattern",
  "content": "## Gen Server Pattern\n\n### Structure\n\nEvery gen_server module should have:\n\n```erlang\n-module(my_server).\n-behaviour(gen_server).\n\n%% API\n-export([start_link/0, get_state/0]).\n\n%% Callbacks\n-export([init/1, handle_call/3, handle_cast/2, \n         handle_info/2, terminate/2, code_change/3]).\n\n%% API Functions\nstart_link() ->\n    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).\n\nget_state() ->\n    gen_server:call(?MODULE, get_state).\n\n%% Callbacks\ninit([]) ->\n    {ok, #{}}.\n\nhandle_call(get_state, _From, State) ->\n    {reply, State, State}.\n```\n\n### Best Practices\n\n1. **Always return proper tuples**\n   - `{reply, Reply, NewState}`\n   - `{noreply, NewState}`\n   - `{stop, Reason, Reply, NewState}`\n\n2. **Handle all callbacks**\n   - Even if just returning `{noreply, State}`\n\n3. **Use timeout for cleanup**\n   ```erlang\n   init([]) ->\n       {ok, #{}, 5000}. % Timeout after 5 seconds\n   ```",
  "category": "coding-patterns",
  "language": "erlang",
  "priority": 90,
  "tags": ["erlang", "otp", "gen_server", "pattern"]
}
```

### Example 3: Common Pitfall

```json
{
  "title": "Erlang: Atom Exhaustion",
  "content": "## ⚠️ Warning: Atom Exhaustion\n\n**Problem:**\nCreating atoms dynamically from user input can exhaust the atom table (max ~1 million atoms).\n\n**Why It's Bad:**\n- Atoms are never garbage collected\n- Exhausting atom table crashes the VM\n- Common attack vector\n\n**Bad Example:**\n```erlang\n% ❌ DON'T DO THIS\nhandle_request(UserInput) ->\n    Key = list_to_atom(UserInput),  % DANGEROUS!\n    process_key(Key).\n```\n\n**Good Examples:**\n```erlang\n% ✅ Option 1: Use existing atoms only\nhandle_request(UserInput) ->\n    try\n        Key = list_to_existing_atom(UserInput),\n        process_key(Key)\n    catch\n        error:badarg -> {error, invalid_key}\n    end.\n\n% ✅ Option 2: Use strings/binaries\nhandle_request(UserInput) ->\n    Key = list_to_binary(UserInput),\n    process_key(Key).\n```\n\n**Detection:**\n- Dialyzer warning: \"Call to list_to_atom/1\"\n- Code review: Look for user input → atoms\n\n**Related:**\n- See also: \"String vs Binary vs Atom\" pattern",
  "category": "pitfalls",
  "language": "erlang",
  "priority": 95,
  "tags": ["erlang", "security", "atoms", "warning"]
}
```

### Example 4: Project Configuration

```json
{
  "title": "Butler Server Development Workflow",
  "content": "## Butler Server Project\n\n**Location:** `/Users/amar.c/workspace/gm_core/butler_server_develop`\n\n**Stack:**\n- Erlang/OTP 26\n- Rebar3\n- PostgreSQL\n- Cowboy (HTTP)\n\n### Directory Structure\n\n```\nbutler_server_develop/\n├── apps/\n│   ├── butler_core/    # Core business logic\n│   ├── butler_api/     # REST API\n│   └── butler_db/      # Database access\n├── config/\n│   └── sys.config      # Runtime config\n├── test/\n└── rebar.config\n```\n\n### Development Commands\n\n```bash\n# Build\nrebar3 compile\n\n# Run tests\nrebar3 eunit\nrebar3 ct\n\n# Type check\nrebar3 dialyzer\n\n# Start shell\nrebar3 shell\n```\n\n### Conventions\n\n1. **Module Naming**\n   - Core modules: `butler_core_*`\n   - API modules: `butler_api_*`\n   - DB modules: `butler_db_*`\n\n2. **Database Migrations**\n   - Files: `priv/migrations/YYYYMMDD_description.sql`\n   - Must be sequential\n   - Never modify existing migrations\n\n3. **Testing**\n   - Unit tests in same app as module\n   - Integration tests in `test/` directory\n   - Mock external services\n\n4. **Error Handling**\n   - Always return `{ok, Result} | {error, Reason}`\n   - Use tagged tuples for errors: `{error, {not_found, UserId}}`",
  "category": "project-configs",
  "project": "butler-server",
  "language": "erlang",
  "priority": 90,
  "tags": ["butler-server", "erlang", "project", "workflow"]
}
```

---

## ✅ Best Practices

### DO's

✅ **Be Specific and Actionable**
```json
// Good: Clear action
"NEVER create docs without asking"

// Bad: Vague
"Be careful with documentation"
```

✅ **Use Examples**
```json
"Example:\n```\nUser: 'Add feature'\nCopilot: 'Should I create docs?'\n```"
```

✅ **Show Before/After**
```markdown
// ❌ BAD
list_to_atom(Input)

// ✅ GOOD
list_to_existing_atom(Input)
```

✅ **Set Appropriate Priority**
```json
// Critical rule = 100
{"title": "Never expose passwords", "priority": 100}

// Nice-to-have = 70
{"title": "Prefer pattern matching", "priority": 70}
```

✅ **Use Tags for Discoverability**
```json
{"tags": ["critical", "security", "permissions"]}
```

### DON'Ts

❌ **Don't Be Vague**
```json
// Bad
{"content": "Write good code"}

// Good
{"content": "Always handle error tuples: {ok, Result} | {error, Reason}"}
```

❌ **Don't Overuse Priority 100**
```json
// Only for CRITICAL rules
{"title": "Security rule", "priority": 100} // ✅ OK
{"title": "Use tabs not spaces", "priority": 100} // ❌ Too high
```

❌ **Don't Mix Concerns**
```json
// Bad: Combines workflow + coding pattern
{"title": "Planning and Gen Servers", ...}

// Good: Separate rules
{"title": "Phase 2: Planning", ...}
{"title": "Gen Server Pattern", ...}
```

❌ **Don't Duplicate Rules**
```json
// Check if similar rule exists first
// If exists, update it rather than create duplicate
```

---

## 🧪 Testing Your Rules

### Step 1: Add the Rule

```bash
cd /Users/amar.c/workspace/dev-setup/mcp-instructions-server/instructions

# Edit existing file or create new one
nano workflow-rules.json
```

### Step 2: Validate JSON

```bash
# Check JSON syntax
node -e "JSON.parse(require('fs').readFileSync('workflow-rules.json'))"
```

### Step 3: Reload Database

```bash
cd /Users/amar.c/workspace/dev-setup/mcp-instructions-server
rm instructions.db
npm run seed
```

**Expected output:**
```
✨ Database seeding completed successfully!
```

### Step 4: Verify in Database

```bash
# Check if rule was added
sqlite3 instructions.db "SELECT title, priority FROM instructions WHERE title LIKE '%Your Rule%';"
```

### Step 5: Restart VS Code

Close all VS Code windows and reopen.

### Step 6: Test with Copilot

Ask questions to verify Copilot follows the rule:

**For Documentation Approval Rule:**
```
@github Add a new API endpoint. Should you create docs?
```

**Expected:** Copilot should ask for permission first.

**For Coding Pattern:**
```
@github Show me how to create a gen_server
```

**Expected:** Copilot should follow your pattern.

### Step 7: Test Priority

Create conflicting low-priority rule to ensure higher priority wins:

```json
// Priority 100 (should win)
{"title": "Never create docs", "priority": 100}

// Priority 80 (should be overridden)
{"title": "Always create README", "priority": 80}
```

**Expected:** Copilot asks before creating docs (priority 100 wins).

---

## 📊 Rule Effectiveness Checklist

Before committing your rule, verify:

- [ ] **Clear Title**: Is it searchable and descriptive?
- [ ] **Appropriate Category**: Does it fit the category?
- [ ] **Right Priority**: Is priority justified?
- [ ] **Actionable Content**: Can Copilot execute it?
- [ ] **Examples Included**: Are there code examples?
- [ ] **No Ambiguity**: Is it clear and specific?
- [ ] **Tested**: Have you tested it with Copilot?
- [ ] **No Duplicates**: Does a similar rule exist?
- [ ] **Proper JSON**: Is the JSON valid?
- [ ] **Tags Added**: Are tags relevant and searchable?

---

## 🚀 Quick Templates

### Critical Workflow Rule Template

```json
{
  "title": "[Action] Required",
  "content": "## CRITICAL: [Rule]\n\n**NEVER [action] without [condition].**\n\nThis applies to:\n- [Item 1]\n- [Item 2]\n\n### Rules:\n\n1. **Always [Action]**\n   ```\n   [Example prompt]\n   ```\n\n2. **Exceptions**\n   - [Exception]\n\n### Example:\n\n**❌ WRONG:**\n```\n[Bad behavior]\n```\n\n**✅ CORRECT:**\n```\n[Good behavior]\n```",
  "category": "workflow-rules",
  "priority": 100,
  "tags": ["critical", "workflow"]
}
```

### Coding Pattern Template

```json
{
  "title": "[Language]: [Pattern Name]",
  "content": "## [Pattern Name]\n\n### Overview\n[Description]\n\n### Pattern\n```[lang]\n[code]\n```\n\n### When to Use\n- [Use case]\n\n### Benefits\n- [Benefit]",
  "category": "coding-patterns",
  "language": "[language]",
  "priority": 85,
  "tags": ["[language]", "[framework]", "pattern"]
}
```

### Pitfall Template

```json
{
  "title": "[Language]: [Pitfall Name]",
  "content": "## ⚠️ Warning: [Pitfall]\n\n**Problem:**\n[Description]\n\n**Why It's Bad:**\n- [Reason]\n\n**Solution:**\n```[lang]\n// ❌ BAD\n[bad code]\n\n// ✅ GOOD\n[good code]\n```",
  "category": "pitfalls",
  "language": "[language]",
  "priority": 90,
  "tags": ["[language]", "warning", "[topic]"]
}
```

---

## 🎓 Real-World Scenarios

### Scenario 1: "Copilot keeps creating unwanted docs"

**Solution:** Add high-priority rule

```json
{
  "title": "Documentation Approval Required",
  "category": "workflow-rules",
  "priority": 100,
  "content": "NEVER create docs without asking"
}
```

### Scenario 2: "Copilot doesn't follow our error handling pattern"

**Solution:** Add coding pattern

```json
{
  "title": "Java: Standard Error Response",
  "category": "coding-patterns",
  "language": "java",
  "priority": 90,
  "content": "Always return ErrorResponse DTO with status, message, timestamp"
}
```

### Scenario 3: "Copilot uses deprecated APIs"

**Solution:** Add pitfall

```json
{
  "title": "Java: Deprecated Date API",
  "category": "pitfalls",
  "language": "java",
  "priority": 95,
  "content": "❌ Never use java.util.Date. ✅ Use java.time.LocalDateTime"
}
```

---

## 📚 Related Documentation

- **[OPERATIONS.md](OPERATIONS.md)** - How to add/update rules in the database
- **[GETTING_STARTED.md](GETTING_STARTED.md)** - Quick start guide
- **[USAGE.md](USAGE.md)** - Examples of using instructions

---

**Ready to create your own rules?** Start with a simple workflow rule at priority 90, test it, then add more complex patterns! 🎉
