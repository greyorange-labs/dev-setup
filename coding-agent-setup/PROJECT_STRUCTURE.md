# Project Structure

This document explains what each file in this repository does and when to modify it.

## 📁 Directory Layout

```
mcp-instructions-server/
├── README.md                    ⭐ Main documentation (setup, config, troubleshooting)
├── global-rules.md              ⭐ AI assistant rules (the actual rules)
├── .github/
│   └── copilot-instructions.md  → Auto-loads in GitHub Copilot
├── .cursorrules                 → Auto-loads in Cursor AI
├── src/                         → MCP server TypeScript source
│   ├── index.ts                 → Main server entry point
│   ├── db.ts                    → SQLite database wrapper
│   ├── resources.ts             → MCP resource handlers
│   ├── tools.ts                 → MCP tool handlers
│   ├── types.ts                 → TypeScript types
│   └── seed.ts                  → Database seeding
├── build/                       → Compiled JavaScript (auto-generated)
├── instructions/                → Optional JSON instruction templates
├── instructions.db              → SQLite database (auto-generated)
├── package.json                 → Node.js dependencies
├── tsconfig.json                → TypeScript configuration
└── setup.sh                     → Automated setup script
```

## 📄 File Purposes

### Documentation Files (2 files only)

| File              | Purpose                                                   | When to Edit                            |
| ----------------- | --------------------------------------------------------- | --------------------------------------- |
| `README.md`       | Complete setup guide, configuration, troubleshooting, FAQ | When setup process changes              |
| `global-rules.md` | Detailed AI assistant rules with examples                 | When adding/modifying AI behavior rules |

### Rule Files (Auto-load in editors)

| File                              | Purpose                                | When to Edit                      |
| --------------------------------- | -------------------------------------- | --------------------------------- |
| `.github/copilot-instructions.md` | Rules that auto-load in GitHub Copilot | Keep in sync with global-rules.md |
| `.cursorrules`                    | Rules that auto-load in Cursor AI      | Keep in sync with global-rules.md |

### Source Code

| File               | Purpose                       | When to Edit                             |
| ------------------ | ----------------------------- | ---------------------------------------- |
| `src/index.ts`     | Main MCP server logic         | Add new MCP capabilities                 |
| `src/db.ts`        | Database operations           | Modify instruction storage               |
| `src/resources.ts` | Serve rules via MCP resources | Change how rules are served              |
| `src/tools.ts`     | MCP tool implementations      | Add new tools for instruction management |
| `src/types.ts`     | TypeScript types              | Add new data structures                  |
| `src/seed.ts`      | Database seeding logic        | Change seeding behavior                  |

### Configuration

| File            | Purpose                      | When to Edit                |
| --------------- | ---------------------------- | --------------------------- |
| `package.json`  | Dependencies and npm scripts | Add dependencies or scripts |
| `tsconfig.json` | TypeScript compiler settings | Change compilation settings |
| `.gitignore`    | Files to exclude from git    | Add new exclusions          |

### Generated Files (Don't edit manually)

| File/Directory      | Purpose                               |
| ------------------- | ------------------------------------- |
| `build/`            | Compiled JavaScript from TypeScript   |
| `instructions.db`   | SQLite database with instruction data |
| `node_modules/`     | Installed npm dependencies            |
| `package-lock.json` | Locked dependency versions            |

## 🔄 Common Tasks

### Task: Add a new rule for AI assistants

1. Edit `global-rules.md` - Add the new rule with examples
2. Update `.github/copilot-instructions.md` - Add summary of new rule
3. Update `.cursorrules` - Add summary of new rule
4. Restart your editor - Rules don't auto-reload

### Task: Modify MCP server behavior

1. Edit source files in `src/`
2. Run `npm run build` to compile
3. Restart your editor to load new server code

### Task: Update setup instructions

1. Edit `README.md` only
2. Keep documentation in sync between local and global copies

### Task: Share changes with team

1. Commit changes: `git commit -am "Description"`
2. Push: `git push`
3. Tell team: "New rules pushed, do `git pull` and restart editor"

## 🎯 Single Source of Truth

- **Rules content:** `global-rules.md` is the authoritative source
- **Setup docs:** `README.md` is the complete guide
- **Everything else:** Derived from these two files

Keep it simple: Two main docs, everything else is implementation.