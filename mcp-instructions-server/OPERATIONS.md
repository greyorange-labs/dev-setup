# MCP Instructions Server - Operations Guide

**Database operations, maintenance, and instruction management**

---

## 📊 Database Operations

### Database Location

```
/Users/amar.c/workspace/dev-setup/mcp-instructions-server/instructions.db
```

**SQLite database with Full-Text Search (FTS5)** enabled for fast searching.

---

## 🔍 Inspecting the Database

### Count Total Instructions

```bash
cd /Users/amar.c/workspace/dev-setup/mcp-instructions-server
sqlite3 instructions.db "SELECT COUNT(*) FROM instructions;"
```

**Expected:** `34` (as of latest seed)

### View All Instructions

```bash
sqlite3 instructions.db "SELECT id, title, category, language FROM instructions ORDER BY priority DESC;"
```

### Search by Category

```bash
# Workflow rules
sqlite3 instructions.db "SELECT title FROM instructions WHERE category='workflow-rules';"

# Coding patterns
sqlite3 instructions.db "SELECT title, language FROM instructions WHERE category='coding-patterns';"

# Pitfalls
sqlite3 instructions.db "SELECT title FROM instructions WHERE category='pitfalls';"
```

### Search by Language

```bash
# Erlang instructions
sqlite3 instructions.db "SELECT title FROM instructions WHERE language='erlang';"

# Java instructions
sqlite3 instructions.db "SELECT title FROM instructions WHERE language='java';"
```

### Search by Project

```bash
# Butler Server instructions
sqlite3 instructions.db "SELECT title FROM instructions WHERE project='butler-server';"

# GreyMatter Platform instructions
sqlite3 instructions.db "SELECT title FROM instructions WHERE project='greymatter-platform';"
```

### Full-Text Search

```bash
# Search in content
sqlite3 instructions.db "SELECT title FROM instructions_fts WHERE instructions_fts MATCH 'gen_server';"

# Search for documentation-related instructions
sqlite3 instructions.db "SELECT title FROM instructions_fts WHERE instructions_fts MATCH 'documentation';"
```

### View Single Instruction

```bash
sqlite3 instructions.db "SELECT * FROM instructions WHERE title='Documentation Approval Required';"
```

---

## 📝 Adding Instructions

### Method 1: Add to JSON Files (Recommended)

**Step 1:** Navigate to instructions directory

```bash
cd /Users/amar.c/workspace/dev-setup/mcp-instructions-server/instructions
```

**Step 2:** Edit existing JSON or create new file

Existing files:
- `workflow-rules.json` - Process rules
- `erlang-patterns.json` - Erlang/OTP patterns
- `java-patterns.json` - Java/Spring Boot patterns
- `common-pitfalls.json` - Anti-patterns
- `project-configs.json` - Project-specific configs

**Step 3:** Add your instruction in JSON format

```json
{
  "title": "Python Async/Await Pattern",
  "content": "## Overview\n\nUse async/await for I/O-bound operations...",
  "category": "coding-patterns",
  "language": "python",
  "project": null,
  "tags": ["python", "async", "concurrency"],
  "priority": 90
}
```

**Step 4:** Reload database

```bash
cd /Users/amar.c/workspace/dev-setup/mcp-instructions-server
rm instructions.db
npm run seed
```

**Step 5:** Restart VS Code (close all windows, reopen)

### Method 2: Use Copilot (Advanced)

Ask Copilot to add instructions using the `add_instruction` tool:

```
@github Add a new instruction:

Title: "Python Type Hints Best Practices"
Category: "coding-patterns"
Language: "python"
Priority: 90
Tags: ["python", "types", "mypy"]

Content:
## Overview
Always use type hints in Python 3.10+ for better IDE support and runtime validation.

## Pattern
```python
from typing import Optional, List

def process_items(items: List[str], limit: Optional[int] = None) -> dict[str, int]:
    """Process items and return counts."""
    return {item: len(item) for item in items[:limit]}
```

## Benefits
- Better IDE autocomplete
- Catch errors early with mypy
- Self-documenting code
```

**Copilot will execute the `add_instruction` tool automatically!**

Changes are immediate, no restart needed.

---

## 🔄 Updating Instructions

### Method 1: Edit JSON + Reseed

```bash
# 1. Edit the JSON file
cd /Users/amar.c/workspace/dev-setup/mcp-instructions-server/instructions
nano erlang-patterns.json  # or use VS Code

# 2. Delete database and reseed
cd /Users/amar.c/workspace/dev-setup/mcp-instructions-server
rm instructions.db
npm run seed

# 3. Restart VS Code
```

### Method 2: Use Copilot

```
@github Update the instruction "OTP Application Structure" with id 42:
Add a section about umbrella applications.
```

Copilot will use the `update_instruction` tool.

### Method 3: Direct SQL (Advanced)

```bash
sqlite3 instructions.db

UPDATE instructions
SET content = 'New content here...',
    priority = 95
WHERE id = 42;

.quit
```

Restart VS Code to reload.

---

## 🗑️ Deleting Instructions

### Method 1: Remove from JSON + Reseed

```bash
# 1. Edit JSON file and remove the instruction
nano instructions/erlang-patterns.json

# 2. Reseed database
rm instructions.db
npm run seed

# 3. Restart VS Code
```

### Method 2: Use Copilot

```
@github Delete the instruction with id 42
```

### Method 3: Direct SQL

```bash
sqlite3 instructions.db "DELETE FROM instructions WHERE id = 42;"
```

Restart VS Code.

---

## 🔎 Searching Instructions

### Using Copilot

Ask Copilot to search:

```
"Search for instructions about gen_server"
"Find all Java transaction-related instructions"
"Show me all high-priority workflow rules"
```

Copilot will use the `search_instructions` tool.

### Using CLI

```bash
# Full-text search
sqlite3 instructions.db "SELECT title FROM instructions_fts WHERE instructions_fts MATCH 'transaction';"

# Filter by category and language
sqlite3 instructions.db "SELECT title FROM instructions WHERE category='coding-patterns' AND language='java';"

# High-priority only (90+)
sqlite3 instructions.db "SELECT title, priority FROM instructions WHERE priority >= 90 ORDER BY priority DESC;"
```

---

## 📋 Listing Categories, Languages, Projects

### Via Copilot

```
"List all categories in the instructions database"
"What languages do we have instructions for?"
"Show all project-specific instructions"
```

### Via CLI

```bash
# All categories
sqlite3 instructions.db "SELECT DISTINCT category FROM instructions;"

# All languages (excluding NULL)
sqlite3 instructions.db "SELECT DISTINCT language FROM instructions WHERE language IS NOT NULL;"

# All projects
sqlite3 instructions.db "SELECT DISTINCT project FROM instructions WHERE project IS NOT NULL;"

# All tags
sqlite3 instructions.db "SELECT DISTINCT tags FROM instructions;"
```

---

## 🧹 Database Maintenance

### Backup Database

```bash
cd /Users/amar.c/workspace/dev-setup/mcp-instructions-server
cp instructions.db instructions.db.backup
```

### Restore from Backup

```bash
cd /Users/amar.c/workspace/dev-setup/mcp-instructions-server
cp instructions.db.backup instructions.db
# Restart VS Code
```

### Reset to Fresh State

```bash
cd /Users/amar.c/workspace/dev-setup/mcp-instructions-server
rm instructions.db
npm run seed
# Restart VS Code
```

### Vacuum Database (Optimize)

```bash
sqlite3 instructions.db "VACUUM;"
```

Reduces file size and improves performance.

---

## 📈 Statistics

### Instruction Count by Category

```bash
sqlite3 instructions.db "
SELECT category, COUNT(*) as count
FROM instructions
GROUP BY category
ORDER BY count DESC;
"
```

### Instruction Count by Language

```bash
sqlite3 instructions.db "
SELECT language, COUNT(*) as count
FROM instructions
WHERE language IS NOT NULL
GROUP BY language
ORDER BY count DESC;
"
```

### Priority Distribution

```bash
sqlite3 instructions.db "
SELECT priority, COUNT(*) as count
FROM instructions
GROUP BY priority
ORDER BY priority DESC;
"
```

### Most Used Tags

```bash
sqlite3 instructions.db "
SELECT tags, COUNT(*) as count
FROM instructions
GROUP BY tags
ORDER BY count DESC
LIMIT 10;
"
```

---

## 🔧 Advanced Operations

### Export Instructions to JSON

```bash
sqlite3 instructions.db <<EOF
.mode json
.output export.json
SELECT * FROM instructions;
.quit
EOF
```

Creates `export.json` with all instructions.

### Import from Custom JSON

```bash
# Create a Node.js script to import
node -e "
const Database = require('better-sqlite3');
const db = new Database('instructions.db');
const data = require('./export.json');

const insert = db.prepare(\`
  INSERT INTO instructions (title, content, category, language, project, tags, priority)
  VALUES (@title, @content, @category, @language, @project, @tags, @priority)
\`);

data.forEach(item => insert.run(item));
console.log('Imported', data.length, 'instructions');
"
```

### Verify Database Integrity

```bash
sqlite3 instructions.db "PRAGMA integrity_check;"
```

Should output: `ok`

### Check Database Size

```bash
du -h instructions.db
```

Typical size: ~100-200 KB for 34 instructions.

---

## 🎯 Common Workflows

### Adding a New Language (e.g., Python)

**Step 1:** Create `instructions/python-patterns.json`

```json
[
  {
    "title": "Python Type Hints",
    "content": "...",
    "category": "coding-patterns",
    "language": "python",
    "tags": ["python", "types"],
    "priority": 90
  },
  {
    "title": "Python Async/Await",
    "content": "...",
    "category": "coding-patterns",
    "language": "python",
    "tags": ["python", "async"],
    "priority": 85
  }
]
```

**Step 2:** Update `src/seed.ts` to include new file

```typescript
const pythonPatterns = JSON.parse(
  readFileSync(join(__dirname, '..', 'instructions', 'python-patterns.json'), 'utf-8')
);

// In the seeding loop
for (const instruction of pythonPatterns) {
  const id = db.addInstruction(instruction);
  console.log(`  ✅ Added Python pattern: ${instruction.title} (ID: ${id})`);
}
```

**Step 3:** Rebuild and reseed

```bash
npm run build
rm instructions.db
npm run seed
```

**Step 4:** Restart VS Code

### Adding Project-Specific Instructions

**Example:** Add instructions for a new Python project

```json
{
  "title": "Analytics Service Overview",
  "content": "## Project Structure\n\n...",
  "category": "project-configs",
  "language": "python",
  "project": "analytics-service",
  "tags": ["python", "fastapi", "analytics"],
  "priority": 95
}
```

**Update `src/index.ts`** to detect the project:

```typescript
function detectProjectContext(filePath?: string): ProjectContext | undefined {
  if (!filePath) return undefined;

  // Existing projects...

  if (filePath.includes('/analytics-service')) {
    return {
      name: 'analytics-service',
      language: 'python',
      framework: 'fastapi',
    };
  }

  return undefined;
}
```

Rebuild, reseed, restart VS Code.

---

## 🆘 Troubleshooting

### Database Locked Error

```
Error: database is locked
```

**Solution:** Close all connections

```bash
# Kill any running node processes
pkill -f "node.*index.js"

# Delete and recreate
rm instructions.db
npm run seed
```

### Instruction Not Showing in Copilot

**Check database:**

```bash
sqlite3 instructions.db "SELECT * FROM instructions WHERE title LIKE '%Your Title%';"
```

**If found:** Restart VS Code

**If not found:** Reseed database

```bash
rm instructions.db
npm run seed
```

### Seed Script Fails

```bash
# Check for syntax errors in JSON files
cd instructions
for f in *.json; do echo "Checking $f..."; node -e "JSON.parse(require('fs').readFileSync('$f'))"; done
```

Fix any JSON syntax errors, then reseed.

### Database Corruption

```bash
# Check integrity
sqlite3 instructions.db "PRAGMA integrity_check;"

# If corrupted, reset
rm instructions.db
npm run seed
```

---

## 📖 Schema Reference

### `instructions` Table

| Column     | Type    | Description                      |
| ---------- | ------- | -------------------------------- |
| id         | INTEGER | Primary key, auto-increment      |
| title      | TEXT    | Instruction title (required)     |
| content    | TEXT    | Full markdown content (required) |
| category   | TEXT    | Category (required)              |
| language   | TEXT    | Programming language (optional)  |
| project    | TEXT    | Project name (optional)          |
| tags       | TEXT    | Comma-separated tags (optional)  |
| priority   | INTEGER | Priority 0-100, default 50       |
| created_at | TEXT    | ISO timestamp                    |
| updated_at | TEXT    | ISO timestamp                    |

### `instructions_fts` Virtual Table

FTS5 full-text search index on `title` and `content` columns.

**Usage:**

```sql
SELECT * FROM instructions_fts WHERE instructions_fts MATCH 'your search query';
```

---

## 🔗 Related Documentation

- **GETTING_STARTED.md** - Quick start guide
- **README.md** - Full technical reference
- **USAGE.md** - Examples and patterns

---

**Need help?** Check the instruction files in `/instructions/` or ask Copilot!
