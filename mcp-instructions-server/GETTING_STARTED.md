# MCP Instructions Server - Getting Started

**Quick setup guide for the Model Context Protocol Instructions Server**

---

## ✅ Installation Status

If you're reading this, installation is complete:

- ✅ Node.js v24.10.0 & npm v11.6.0 installed
- ✅ MCP server built (`build/` directory)
- ✅ Database seeded with **34 instructions**
- ✅ VS Code configured globally
- ✅ Ready to use!

---

## 🚀 How the MCP Server Works

### Important: The Server Starts Automatically

**You don't need to manually start the MCP server!**

VS Code automatically starts the MCP server when:
- ✅ You open VS Code
- ✅ You use GitHub Copilot Chat
- ✅ You switch between projects

**The server runs in the background** as a child process managed by VS Code.

### What Happens When...

#### 1. **Laptop Restart → Open VS Code**
- ✅ VS Code reads your global settings
- ✅ Automatically starts MCP server when Copilot is used
- ✅ No action needed from you

#### 2. **Restart VS Code**
- ✅ MCP server automatically restarts
- ✅ Loads latest instructions from database
- ✅ No action needed from you

#### 3. **Open Erlang Project (Butler Server)**
- ✅ MCP server detects project path
- ✅ Automatically loads Erlang patterns
- ✅ Applies Butler Server configuration
- ✅ No action needed from you

#### 4. **Open Java Project (GreyMatter Platform)**
- ✅ MCP server detects project path
- ✅ Automatically loads Java/Spring Boot patterns
- ✅ Applies GreyMatter Platform configuration
- ✅ No action needed from you

### When You MIGHT Need to Restart

Only restart VS Code if:
- ❌ Copilot isn't following instructions (rare)
- ❌ You changed VS Code settings
- ❌ You updated the MCP server code

**How to restart:** Close all VS Code windows and reopen

---

## 📝 What Instructions Are Loaded

### 34 Instructions Total:

**Workflow Rules (7)** - How to work
- ⭐ **Documentation Approval Required** (NEW!)
- Phase 1: Requirement Clarification
- Phase 2: Planning
- Phase 3: Implementation
- Phase 4: Review and Summary
- Context Gathering Protocol
- Error Prevention Checklist

**Erlang/OTP Patterns (8)**
- OTP Application Structure
- Error Handling with Tuples `{ok, Result} | {error, Reason}`
- Gen Server Pattern
- Type Specifications with `-spec`
- Database Migration Ordering ⚠️ CRITICAL
- Testing Patterns (EUnit, Common Test)
- Logging Best Practices
- Maps vs Records

**Java/Spring Boot Patterns (7)**
- REST Controller Pattern
- Service Layer Pattern
- JPA Entity Best Practices
- Repository Pattern
- DTO and Mapper Pattern
- Testing Patterns (JUnit, Mockito)
- Configuration Management

**Common Pitfalls (8)** - What to avoid
- Erlang: Atom Exhaustion ⚠️
- Erlang: Process Leaks
- Erlang: ETS Table Leaks
- Java: N+1 Query Problem ⚠️
- Java: LazyInitializationException
- Java: Entity as DTO Anti-Pattern
- Java: Transaction Boundary Issues
- General: Premature Optimization

**Project Configurations (4)**
- Butler Server Overview & Workflow
- GreyMatter Platform Overview & Workflow

---

## 🧪 Quick Test

Open GitHub Copilot Chat and try:

```
"What is the 4-phase workflow?"
```

**Expected:** Copilot describes Clarification → Planning → Implementation → Review

```
"Should you create documentation without asking me?"
```

**Expected:** Copilot says NO, must always ask for approval first

---

## 🔄 How to Add/Reload Instructions

### Option 1: Add to JSON Files (Recommended)

1. **Edit the JSON file:**
   ```bash
   cd /Users/amar.c/workspace/dev-setup/mcp-instructions-server/instructions

   # Edit existing files:
   # - workflow-rules.json
   # - erlang-patterns.json
   # - java-patterns.json
   # - common-pitfalls.json
   # - project-configs.json

   # Or create new files like python-patterns.json
   ```

2. **Reload the database:**
   ```bash
   cd /Users/amar.c/workspace/dev-setup/mcp-instructions-server
   rm instructions.db
   npm run seed
   ```

3. **Restart VS Code** (close all windows, reopen)

### Option 2: Add via Copilot (Advanced)

Ask Copilot to add instructions using the MCP tools:

```
"Add a new instruction to the database:
Title: 'Python Type Hints'
Category: 'coding-patterns'
Language: 'python'
Content: 'Always use type hints in Python functions...'
Tags: ['python', 'types']"
```

Copilot will use the `add_instruction` tool.

**Note:** Changes via tools are live, no restart needed!

### JSON Format for New Instructions

```json
{
  "title": "Your Pattern Name",
  "content": "## Description\n\nYour detailed content in markdown...",
  "category": "coding-patterns",
  "language": "python",
  "project": "my-project",
  "tags": ["python", "async", "performance"],
  "priority": 90
}
```

**Categories:**
- `workflow-rules` - Process and methodology
- `coding-patterns` - Language-specific patterns
- `component-guidelines` - Component-specific rules
- `pitfalls` - Anti-patterns
- `project-configs` - Project setup
- `testing-strategies` - Testing approaches
- `architecture-patterns` - Design patterns

---

## 🎯 How to Use with Copilot

### Simple Questions
```
"What patterns should I follow in this project?"
"What are common Erlang pitfalls?"
"Show me the Spring Boot REST pattern"
```

### Implementing Features (Follows 4-Phase Workflow)
```
"Add a new user registration endpoint. Follow the workflow."
```

Copilot will automatically:
1. **Ask clarifying questions** (Phase 1)
2. **Create detailed plan** (Phase 2)
3. **Implement with patterns** (Phase 3)
4. **Summarize results** (Phase 4)
5. **Ask before creating docs** ⭐ NEW!

### Code Review
```
"Review this code for common pitfalls"
"Check if this follows our Erlang patterns"
```

---

## 📍 Project Auto-Detection

The MCP server automatically knows your project:

**Butler Server**
- Path: `/Users/amar.c/workspace/gm_core/butler_server_develop`
- Language: Erlang/OTP
- Applies: Erlang patterns, OTP structure, migration rules

**GreyMatter Platform**
- Path: `/Users/amar.c/workspace/greymatter-platform`
- Language: Java 21 / Spring Boot
- Applies: Spring Boot patterns, JPA rules, DTO patterns

**Other Projects**
- Detects language by file extension
- Applies general patterns for that language

---

## 🛠️ Useful Commands

```bash
# Navigate to MCP server
cd /Users/amar.c/workspace/dev-setup/mcp-instructions-server

# Check instructions count
sqlite3 instructions.db "SELECT COUNT(*) FROM instructions;"

# List all instruction titles
sqlite3 instructions.db "SELECT title, category FROM instructions ORDER BY priority DESC;"

# Search for specific topics
sqlite3 instructions.db "SELECT title FROM instructions WHERE content LIKE '%gen_server%';"

# View instructions by category
sqlite3 instructions.db "SELECT title FROM instructions WHERE category='workflow-rules';"

# Check if documentation approval rule exists
sqlite3 instructions.db "SELECT title FROM instructions WHERE title LIKE '%Documentation%';"
```

### After Adding New Instructions

```bash
# Rebuild database
cd /Users/amar.c/workspace/dev-setup/mcp-instructions-server
rm instructions.db
npm run seed

# Then restart VS Code (close all windows, reopen)
```

### Verify MCP Server Can Start

```bash
cd /Users/amar.c/workspace/dev-setup/mcp-instructions-server
node build/index.js
# Should start without errors (Press Ctrl+C to stop)
```

---

## 🎓 Example Workflows

### Example 1: Erlang Feature in Butler Server

**You:** "Add a gen_server for inventory tracking"

**Copilot Phase 1:**
```
Let me clarify: You want to add a new gen_server behavior module
for tracking inventory in the Butler Server project.

Affected: apps/inventory/src/, supervisor tree
Assumptions: Using OTP patterns, PostgreSQL backend
Questions: What state should it maintain? Event-driven or polling?

Please confirm before planning.
```

**You:** "Confirmed. Event-driven."

**Copilot Phase 2:**
```
Implementation Plan:

Files to Create:
1. apps/inventory/src/inventory_server.erl - gen_server implementation
2. apps/inventory/src/inventory_sup.erl - supervisor
3. test/inventory_server_tests.erl - EUnit tests

Order:
1. Create gen_server with proper init/handle_call/handle_cast
2. Add to supervisor tree
3. Add type specs with -spec
4. Write tests

Should I create documentation for this module?  ⭐ ASKS PERMISSION
```

**You:** "No docs needed, just code"

**Copilot Phase 3:** [Implements following Erlang patterns]

**Copilot Phase 4:**
```
Summary:
✅ Created inventory_server.erl with gen_server behavior
✅ Added to supervisor tree with restart strategy
✅ Type specs added with -spec declarations
✅ Tests written and passing

Next: Run make dialyzer and make test
```

### Example 2: Java Feature in GreyMatter Platform

**You:** "Add product inventory REST API"

**Copilot Phase 1:** [Clarifies requirements]

**Copilot Phase 2:**
```
Implementation Plan:

Files:
1. V001__create_products_table.sql (Flyway migration)
2. Product.java (JPA entity)
3. ProductDto.java (response DTO, NOT entity!)
4. ProductService.java (@Transactional)
5. ProductController.java (REST endpoints)
6. Tests

Testing: Unit tests with Mockito, integration tests with @SpringBootTest
Risks: N+1 queries → Use @EntityGraph

Should I create API documentation (README/OpenAPI)?  ⭐ ASKS PERMISSION
```

**You:** "Yes, create OpenAPI annotations only"

**Copilot Phase 3:** [Implements with Spring Boot patterns + Swagger annotations]

**Copilot Phase 4:** [Summarizes with validation results]

---

## 🆘 Troubleshooting

### Copilot Not Using Instructions

**Try:**
1. Explicitly ask: "Use the MCP instructions to help me"
2. Restart VS Code (close all windows)
3. Check settings: Cmd+Shift+P → "Preferences: Open User Settings (JSON)"
4. Verify the `github.copilot.chat.mcp.servers` section exists

### Copilot Still Creating Docs Without Asking

**The documentation approval rule has Priority 100 (highest)**

If Copilot creates docs without asking:
1. Verify the instruction exists:
   ```bash
   sqlite3 instructions.db "SELECT * FROM instructions WHERE title LIKE '%Documentation%';"
   ```
2. If not found, reload database:
   ```bash
   rm instructions.db && npm run seed
   ```
3. Restart VS Code
4. Remind Copilot: "Remember to ask before creating any documentation"

### Instructions Seem Outdated

```bash
# Reload database
cd /Users/amar.c/workspace/dev-setup/mcp-instructions-server
rm instructions.db
npm run seed
# Restart VS Code
```

### Server Not Starting

```bash
# Test manually
cd /Users/amar.c/workspace/dev-setup/mcp-instructions-server
node build/index.js
# If errors, rebuild:
npm run build
```

---

## 📚 Additional Documentation

- **USAGE.md** - Detailed examples and advanced usage
- **README.md** - Full API reference and technical details
- **OPERATIONS.md** - Database operations and maintenance

---

## 🎉 You're Ready!

The MCP server is running automatically in the background.

**Just start using GitHub Copilot!**

- It will follow the 4-phase workflow
- It will apply language-specific patterns
- It will warn about pitfalls
- **It will ask before creating documentation** ⭐

Your development workflow is now supercharged! 🚀

---

**Quick test right now:** Ask Copilot "Should you create documentation without my approval?"
