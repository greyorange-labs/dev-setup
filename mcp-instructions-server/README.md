# MCP Instructions Server

A **Model Context Protocol (MCP)** server that stores and retrieves development instructions to help **GitHub Copilot** and **Cursor AI** provide better implementations with ~100% accuracy.

---

## 🚀 Quick Start for New Users

**Choose your AI assistant:**

### GitHub Copilot Setup
👉 **[SETUP_FOR_NEW_USERS.md](SETUP_FOR_NEW_USERS.md)** - Complete MCP server setup (5 steps)
👉 **[COPILOT_SETUP_COMMANDS.md](COPILOT_SETUP_COMMANDS.md)** - Copy-paste commands for Copilot Chat

### Cursor AI Setup
👉 **[CURSOR_SETUP.md](CURSOR_SETUP.md)** - Complete Cursor setup (3 steps, simpler!)
👉 **[CURSOR_PROMPTS.md](CURSOR_PROMPTS.md)** - Copy-paste prompts for Cursor Chat
👉 **[CURSOR_LOCAL_SETUP_INSTRUCTIONS.md](CURSOR_LOCAL_SETUP_INSTRUCTIONS.md)** - Local configuration guide

### Both Tools
👉 **[SETUP_COMPARISON.md](SETUP_COMPARISON.md)** - Side-by-side comparison (Cursor vs Copilot)

---

## 📖 Documentation

### Getting Started
- **[SETUP_FOR_NEW_USERS.md](SETUP_FOR_NEW_USERS.md)** ⭐ **NEW USERS START HERE** - MCP server setup
- **[COPILOT_SETUP_COMMANDS.md](COPILOT_SETUP_COMMANDS.md)** 💬 **GitHub Copilot** - One-click setup prompts
- **[CURSOR_SETUP.md](CURSOR_SETUP.md)** 🎯 **Cursor AI** - Complete setup guide
- **[CURSOR_PROMPTS.md](CURSOR_PROMPTS.md)** 💬 **Cursor Prompts** - Test prompts for Cursor Chat
- **[CURSOR_LOCAL_SETUP_INSTRUCTIONS.md](CURSOR_LOCAL_SETUP_INSTRUCTIONS.md)** 🔧 **Cursor Local** - macOS configuration
- **[GETTING_STARTED.md](GETTING_STARTED.md)** 📘 **User Guide** - How the server works, troubleshooting

### Configuration & Customization
- **[COPILOT_CONFIGURATION_GUIDE.md](COPILOT_CONFIGURATION_GUIDE.md)** ⚙️ **Copilot Config** - Make Copilot follow rules everywhere
- **[SETUP_COMPARISON.md](SETUP_COMPARISON.md)** ⚖️ **Comparison** - Cursor vs Copilot differences
- **[HOW_TO_DEFINE_RULES.md](HOW_TO_DEFINE_RULES.md)** 📝 **Creating Rules** - Complete guide to defining instructions

### Operations & Reference
- **[OPERATIONS.md](OPERATIONS.md)** 🔧 **Operations** - Database management, adding/updating instructions
- **[USAGE.md](USAGE.md)** 💡 **Examples** - Detailed usage patterns and examples
- **[README.md](README.md)** 📚 **Technical Reference** - API documentation (this file)

---

## 🎯 Purpose

This MCP server acts as a knowledge base for your development workflow, storing:

- **Workflow Rules**: 4-phase development process (clarification, planning, implementation, review)
- **Coding Patterns**: Language-specific best practices (Erlang, Java, TypeScript, etc.)
- **Component Guidelines**: Project-specific patterns and conventions
- **Common Pitfalls**: Anti-patterns and mistakes to avoid
- **Project Configurations**: Project-specific setup and conventions
- **Testing Strategies**: How to test in different languages/frameworks
- **Architecture Patterns**: High-level design patterns

**NEW:** Documentation approval rule - Copilot must always ask permission before creating docs!

## 🏗️ Architecture

```
mcp-instructions-server/
├── src/
│   ├── index.ts          # MCP server entry point
│   ├── db.ts             # SQLite database layer
│   ├── types.ts          # TypeScript type definitions
│   ├── resources.ts      # MCP resources (read instructions)
│   ├── tools.ts          # MCP tools (manage instructions)
│   └── seed.ts           # Database seeding script
├── instructions/         # Seed data files
│   ├── workflow-rules.json
│   ├── erlang-patterns.json
│   ├── java-patterns.json
│   ├── common-pitfalls.json
│   └── project-configs.json
├── package.json
├── tsconfig.json
└── README.md
```

## 📦 Installation

### 1. Install Dependencies

```bash
cd mcp-instructions-server
npm install
```

### 2. Build the Project

```bash
npm run build
```

### 3. Seed the Database

```bash
npm run seed
```

This creates `instructions.db` and populates it with starter instructions.

## 🚀 Usage

### Starting the Server

The MCP server runs as a stdio-based process:

```bash
npm start
```

### Integration Options

#### Option 1: Cursor AI (Recommended - Simpler!)

**Primary Method:** `.cursorrules` file at workspace root

**Location:** `/Users/amar.c/workspace/dev-setup/.cursorrules`

**How it works:**
- Cursor automatically loads `.cursorrules` from workspace root
- Contains all workflow rules, documentation approval, context gathering
- No server startup needed for basic rules
- Rules apply to all Cursor modes (Chat, Composer, Inline)

**Setup:** [CURSOR_SETUP.md](CURSOR_SETUP.md)

**Optional:** MCP server integration (future support)
```json
{
  "cursor.mcp.servers": {
    "instructions": {
      "command": "node",
      "args": [
        "/Users/amar.c/workspace/dev-setup/mcp-instructions-server/build/index.js"
      ]
    }
  }
}
```

---

#### Option 2: GitHub Copilot with VS Code

**Primary Method:** MCP server + VS Code settings

Add to your VS Code `settings.json` (User or Workspace):

```json
{
  "github.copilot.chat.mcp.servers": {
    "instructions": {
      "command": "node",
      "args": [
        "/Users/amar.c/workspace/dev-setup/mcp-instructions-server/build/index.js"
      ],
      "env": {
        "VSCODE_CURRENT_FILE": "${file}"
      }
    }
  }
}
```

**Setup:** [SETUP_FOR_NEW_USERS.md](SETUP_FOR_NEW_USERS.md)

---

#### Option 3: Claude Desktop Integration

Add to `~/Library/Application Support/Claude/claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "instructions": {
      "command": "node",
      "args": [
        "/Users/amar.c/workspace/dev-setup/mcp-instructions-server/build/index.js"
      ]
    }
  }
}
```

## 📚 Available Resources

Resources are read-only endpoints that retrieve instructions:

### 1. Workflow Rules

```
instructions://workflow-rules
```

Returns the 4-phase development workflow (clarification → planning → implementation → review).

### 2. Coding Patterns by Language

```
instructions://coding-patterns/erlang
instructions://coding-patterns/java
instructions://coding-patterns/typescript
```

Language-specific coding patterns and best practices.

### 3. Project Configuration

```
instructions://project-config/butler-server
instructions://project-config/greymatter-platform
```

Project-specific conventions and setup.

### 4. Common Pitfalls

```
instructions://pitfalls/erlang
instructions://pitfalls/java
```

Common mistakes and anti-patterns to avoid.

### 5. Component Guidelines

```
instructions://component-guidelines/{project}
```

Component-specific guidelines for a project.

### 6. Testing Strategies

```
instructions://testing-strategies/{language}
```

Testing approaches for specific languages.

### 7. Architecture Patterns

```
instructions://architecture-patterns/{language}
```

High-level architecture patterns.

### 8. Context-Aware Instructions

```
instructions://context-aware
```

Automatically retrieves instructions based on the current file/project being edited.

## 🛠️ Available Tools

Tools allow you to manage instructions programmatically:

### 1. `add_instruction`

Add a new instruction to the database.

**Parameters:**
- `title` (string, required): Title of the instruction
- `content` (string, required): Content in markdown format
- `category` (enum, required): One of: `workflow-rules`, `coding-patterns`, `component-guidelines`, `pitfalls`, `project-configs`, `testing-strategies`, `architecture-patterns`
- `language` (string, optional): Programming language
- `project` (string, optional): Project name
- `tags` (array, optional): Tags for categorization
- `priority` (number, optional): Priority level (higher = more important)

### 2. `search_instructions`

Search for instructions by various criteria.

**Parameters:**
- `category` (enum, optional): Filter by category
- `language` (string, optional): Filter by language
- `project` (string, optional): Filter by project
- `tags` (array, optional): Filter by tags
- `query` (string, optional): Full-text search
- `limit` (number, optional): Max results (default: 50)

### 3. `update_instruction`

Update an existing instruction.

**Parameters:**
- `id` (number, required): Instruction ID
- Other fields same as `add_instruction` (all optional)

### 4. `delete_instruction`

Delete an instruction.

**Parameters:**
- `id` (number, required): Instruction ID

### 5. `get_instruction`

Get a specific instruction by ID.

**Parameters:**
- `id` (number, required): Instruction ID

### 6. List Tools

- `list_categories`: Get all categories
- `list_languages`: Get all languages with instructions
- `list_projects`: Get all projects with instructions
- `list_tags`: Get all tags used

## 🎯 How It Works with AI Assistants

### Before (Without Setup)

❌ AI might:
- Skip requirement clarification
- Make assumptions
- Not follow project patterns
- Introduce common pitfalls
- Skip validation steps
- Create documentation without asking

### After (With Setup) - Both Cursor & Copilot

✅ AI will:
1. **Ask before creating docs** (CRITICAL)
2. **Clarify requirements** first
3. **Plan** before implementing
4. **Follow** language-specific patterns
5. **Avoid** known pitfalls
6. **Validate** with proper tools
7. **Summarize** changes

### How Each Tool Works

**[Cursor AI]** - Via `.cursorrules` file:
- Loads rules automatically from workspace
- Applies to all modes (Chat, Composer, Inline)
- No prefix needed
- Simpler setup

**[GitHub Copilot]** - Via MCP server:
- Use `@github` prefix to load MCP
- Dynamic, queryable instruction database
- More powerful pattern queries
- Requires MCP server running

**Both achieve ~100% accuracy!**

## 📝 Workflow Example

When you ask your AI (Cursor or Copilot) to "Add a new user endpoint":

### Phase 1: Clarification
AI reads workflow rules and:
- Restates the requirement
- Identifies scope (which files)
- Notes assumptions
- **Asks for confirmation**

**[Cursor]**: Reads from `.cursorrules`
**[Copilot]**: Reads from `instructions://workflow-rules` via MCP

### Phase 2: Planning
AI reads project-specific patterns:
- Project configuration
- Language patterns (Java/Erlang/TypeScript)
- Common pitfalls to avoid

Then creates a detailed plan with:
- Files to modify
- Testing strategy
- Potential risks
- **Asks: "Should I create documentation?"**

**[Cursor]**: Uses patterns from `.cursorrules`
**[Copilot]**: Queries MCP server for patterns

### Phase 3: Implementation
AI implements following:
- Language-specific patterns (e.g., Spring Boot for Java)
- Avoiding common pitfalls (e.g., N+1 queries)
- Using best practices (e.g., DTOs not entities)
- Proper error handling and validation

**Both follow stored patterns for consistency**

### Phase 4: Review
AI provides:
- Summary of all changes made
- Validation results (linting, tests)
- Testing evidence
- Next steps

**Result: ~100% accuracy, 0 errors** 🎉

## 🔧 Customization

### Adding Instructions

You can add instructions in two ways:

#### 1. Via JSON Files (for bulk import)

Create or edit files in `instructions/`:

```json
[
  {
    "title": "My Pattern",
    "content": "## Description\n\nPattern details...",
    "category": "coding-patterns",
    "language": "python",
    "tags": ["async", "performance"],
    "priority": 90
  }
]
```

Then run:
```bash
npm run seed
```

#### 2. Via MCP Tools (programmatic)

Use the `add_instruction` tool from Copilot or any MCP client.

### Project Detection

The server auto-detects projects based on file paths. Edit `src/index.ts` to add your projects:

```typescript
function detectProjectContext(filePath?: string): ProjectContext | null {
  if (!filePath) return null;

  const absolutePath = resolve(filePath);

  // Add your project
  if (absolutePath.includes('my-project')) {
    return {
      projectPath: '/path/to/my-project',
      projectName: 'my-project',
      language: 'typescript',
      framework: 'nest.js',
      currentFile: filePath,
    };
  }

  // ... existing detections
}
```

## 📊 Database Schema

The server uses SQLite with the following tables:

### `instructions`
- `id`: Primary key
- `title`: Instruction title
- `content`: Markdown content
- `category`: Category enum
- `language`: Programming language (optional)
- `project`: Project name (optional)
- `priority`: Priority level
- `created_at`, `updated_at`: Timestamps

### `tags`
- `id`: Primary key
- `instruction_id`: Foreign key to instructions
- `tag`: Tag name

### `instructions_fts`
- Full-text search virtual table

## 🧪 Testing the Server

### Test Resources

Use the MCP Inspector or manually test:

```bash
# Start the server
npm start

# In another terminal, send MCP requests
echo '{"jsonrpc":"2.0","id":1,"method":"resources/list"}' | npm start
```

### Verify Database

```bash
sqlite3 instructions.db
```

```sql
-- View all instructions
SELECT title, category, language, priority FROM instructions;

-- Search by category
SELECT * FROM instructions WHERE category = 'workflow-rules';

-- Full-text search
SELECT * FROM instructions_fts WHERE instructions_fts MATCH 'error handling';
```

## 🚧 Troubleshooting

### Server Not Starting

1. Check Node.js version (requires Node 18+):
   ```bash
   node --version
   ```

2. Rebuild the project:
   ```bash
   npm run build
   ```

3. Check for errors:
   ```bash
   npm start 2>&1 | tee server.log
   ```

### Instructions Not Loading

1. Verify database exists:
   ```bash
   ls -la instructions.db
   ```

2. Re-seed the database:
   ```bash
   rm instructions.db
   npm run seed
   ```

### Copilot Not Using Instructions

1. Verify MCP server is configured in VS Code settings
2. Restart VS Code
3. Check Copilot logs in Output panel
4. Try explicitly referencing resources in prompts

## 📚 Examples

### Example 1: Adding Erlang GenServer Pattern

```typescript
// Via tool call
{
  "title": "GenServer Timeout Pattern",
  "content": "Always use timeouts in gen_server:call...",
  "category": "coding-patterns",
  "language": "erlang",
  "tags": ["gen_server", "timeout", "reliability"],
  "priority": 95
}
```

### Example 2: Project-Specific Rule

```typescript
{
  "title": "API Versioning Strategy",
  "content": "All public APIs must be versioned...",
  "category": "component-guidelines",
  "project": "butler-server",
  "tags": ["api", "versioning"],
  "priority": 100
}
```

## 🔄 Development

### Making Changes

1. Edit source files in `src/`
2. Rebuild: `npm run build`
3. Test: `npm start`

### Watch Mode

```bash
npm run watch
```

### Adding New Categories

1. Update `InstructionCategory` type in `src/types.ts`
2. Update tool schemas in `src/tools.ts`
3. Update resource handlers in `src/resources.ts`
4. Rebuild

## 🤝 Contributing

To add new instructions to the base set:

1. Create/edit JSON files in `instructions/`
2. Follow the existing structure
3. Use proper markdown formatting
4. Include relevant tags
5. Set appropriate priority

## 📄 License

MIT

## 🙏 Acknowledgments

Based on the Model Context Protocol by Anthropic.

---

**Questions?** Check the [MCP Documentation](https://modelcontextprotocol.io/docs) or open an issue.
