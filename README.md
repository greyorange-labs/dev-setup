# Development Setup Repository

**Personal development environment configurations and tools**

---

## 📦 What's Inside

### 🐚 [Shell](shell/)

Shell configurations and customizations:
- **Starship Prompt** - Fast, customizable shell prompt
- Custom prompt themes and configurations

**Apply configurations:**
```bash
# Starship prompt configuration
cp shell/prompt/starship/config.toml ~/.config/starship.toml
```

---

### 🔧 [Git](git/)

Git configuration and utilities:
- Global gitconfig with useful aliases
- Commit squashing scripts
- Git workflow helpers

**Apply configurations:**
```bash
cd git
./apply-gitconfig.sh
```

---

### 🐳 [Docker](docker/)

Docker configurations for local development:
- **PostgreSQL** - Database with initialization scripts
- **NGINX** - Reverse proxy with SSL support
- **Grafana** - Monitoring and visualization
- **Telegraf** - Metrics collection

**Quick Start:**
```bash
cd docker
docker-compose up -d
```

---

### 🧪 [Erlang](erlang/)

Erlang/OTP development resources:
- Installation guides
- Memory troubleshooting guides
- OTP best practices

---

### 🤖 [MCP Instructions Server](mcp-instructions-server/) ⭐ **Featured**

An **MCP (Model Context Protocol) server** that teaches **GitHub Copilot** and **Cursor AI** your development workflow and coding patterns.

**Features:**
- ✅ 4-phase workflow enforcement (clarify → plan → implement → review)
- ✅ Language-specific patterns (Erlang, Java, TypeScript, and more)
- ✅ Common pitfalls and anti-patterns database
- ✅ **Documentation approval rule** - AI always asks before creating docs
- ✅ Context-aware instructions based on current project
- ✅ **Dual AI support**: Works with both Cursor AI and GitHub Copilot
- ✅ **~100% accuracy** for complex implementations

**Quick Start:**

**For Cursor AI (Recommended - Simpler!):**
```bash
# Rules automatically loaded from .cursorrules file at repo root
# No setup needed - just start using Cursor!
```

**For GitHub Copilot:**
```bash
cd mcp-instructions-server
npm install && npm run build && npm run seed
```

📖 **Documentation:**
- **Cursor Users:** [CURSOR_SETUP.md](mcp-instructions-server/CURSOR_SETUP.md) - 3-step setup
- **Copilot Users:** [SETUP_FOR_NEW_USERS.md](mcp-instructions-server/SETUP_FOR_NEW_USERS.md) - 5-step setup
- **Compare Both:** [SETUP_COMPARISON.md](mcp-instructions-server/SETUP_COMPARISON.md)

---

## 🚀 Quick Setup

### 1. Clone Repository

```bash
git clone https://github.com/amarBitMan/dev-setup.git
cd dev-setup
```

### 2. Choose Your Tools

Pick what you need:

**For AI-Enhanced Development (Cursor):**
```bash
# Already configured! .cursorrules file loads automatically
# Just open the workspace in Cursor and start coding
```

**For AI-Enhanced Development (GitHub Copilot):**
```bash
cd mcp-instructions-server
npm install && npm run build && npm run seed
# Then configure VS Code settings (see SETUP_FOR_NEW_USERS.md)
```

**For Docker Development:**
```bash
cd docker
docker-compose up -d
```

**For Git Configuration:**
```bash
cd git
./apply-gitconfig.sh
```

**For Shell Customization:**
```bash
# Install Starship (if not already installed)
brew install starship

# Apply custom prompt
cp shell/prompt/starship/config.toml ~/.config/starship.toml
```

---

## 🌟 Highlights

### MCP Instructions Server

The **standout feature** of this repository. Works with both **Cursor AI** and **GitHub Copilot**.

**After setup, your AI assistant will:**
- Follow your 4-phase workflow automatically
- Apply language-specific best practices (Erlang, Java, TypeScript, etc.)
- Avoid common pitfalls and anti-patterns
- **Always ask before creating documentation**
- Achieve **~100% implementation accuracy**

**Example interaction (works in both Cursor and Copilot):**

```
You: Add a REST API endpoint for user management

AI Phase 1: Let me clarify the requirements...
[Asks questions, confirms understanding]

AI Phase 2: Here's my implementation plan...
Should I create documentation for this endpoint?

You: Yes

AI Phase 3: [Implements with best practices]

AI Phase 4: [Summarizes changes and validation results]
```

**Key Differences:**
- **Cursor**: Uses `.cursorrules` file (no server needed for basic rules)
- **Copilot**: Uses MCP server (dynamic, queryable instructions)
- **Both**: Achieve the same ~100% accuracy results!

---

## 📚 Documentation

### Directory Structure

```
dev-setup/
├── .cursorrules                  # Cursor AI workflow rules
├── .github/                      # GitHub configurations
│   ├── copilot-instructions.md  # Copilot instructions (workspace-specific)
│   └── copilot-instructions-global.md  # Copilot instructions (global)
├── mcp-instructions-server/     # MCP server for AI assistants
├── docker/                       # Docker development environment
├── erlang/                       # Erlang/OTP resources
├── git/                          # Git configurations
└── shell/                        # Shell customizations
    └── prompt/starship/          # Starship prompt config
```

### Documentation by Component

- **[shell/](shell/)** - Shell prompt customization (Starship)
- **[git/](git/)** - Git configuration details and utilities
- **[docker/](docker/)** - Docker service configurations and usage
- **[erlang/](erlang/)** - Erlang development guides and troubleshooting
- **[mcp-instructions-server/](mcp-instructions-server/)** - AI assistant setup guides
  - [CURSOR_SETUP.md](mcp-instructions-server/CURSOR_SETUP.md) - Cursor AI setup
  - [SETUP_FOR_NEW_USERS.md](mcp-instructions-server/SETUP_FOR_NEW_USERS.md) - Copilot setup
  - [SETUP_COMPARISON.md](mcp-instructions-server/SETUP_COMPARISON.md) - Compare both

---

## 🤝 Contributing

This is a personal setup repository, but feel free to:

1. Fork for your own use
2. Suggest improvements via issues
3. Share your own patterns and configurations

---

## 📄 License

MIT License - See [LICENSE](LICENSE) for details.

---

## 🔗 Links

- **GitHub Repository:** [github.com/amarBitMan/dev-setup](https://github.com/amarBitMan/dev-setup)
- **Issues:** [Report bugs or request features](https://github.com/amarBitMan/dev-setup/issues)

---

## 🎯 Getting Started

### Recommended Path

1. **Choose your AI assistant:**
   - **Cursor AI** (Simpler): Open workspace, rules load automatically from `.cursorrules`
   - **GitHub Copilot**: Follow [mcp-instructions-server/SETUP_FOR_NEW_USERS.md](mcp-instructions-server/SETUP_FOR_NEW_USERS.md)

2. **Set up development tools:**
   - Docker containers for local services
   - Git aliases and workflow helpers
   - Shell prompt customization

3. **Explore language-specific resources:**
   - Erlang/OTP guides and troubleshooting
   - Java patterns (via MCP server)
   - TypeScript best practices (via MCP server)

### First Time Using This Repo?

**Start here:** [mcp-instructions-server/SETUP_FOR_NEW_USERS.md](mcp-instructions-server/SETUP_FOR_NEW_USERS.md) (for Copilot) or [mcp-instructions-server/CURSOR_SETUP.md](mcp-instructions-server/CURSOR_SETUP.md) (for Cursor)

---

**Start with the MCP Instructions Server** to supercharge your AI coding assistant! 🚀
