# Development Setup Repository

**Personal development environment configurations and tools**

---

## 📦 What's Inside

### 🤖 [MCP Instructions Server](mcp-instructions-server/) ⭐ **Featured**

An **MCP (Model Context Protocol) server** that teaches GitHub Copilot your development workflow and coding patterns.

**Features:**
- ✅ 4-phase workflow enforcement (clarify → plan → implement → review)
- ✅ Language-specific patterns (Erlang, Java, and more)
- ✅ Common pitfalls and anti-patterns database
- ✅ **Documentation approval rule** - Copilot always asks before creating docs
- ✅ Context-aware instructions based on project

**Quick Start:**
```bash
cd mcp-instructions-server
npm install && npm run build && npm run seed
```

👉 **See [mcp-instructions-server/SETUP_FOR_NEW_USERS.md](mcp-instructions-server/SETUP_FOR_NEW_USERS.md) for complete setup**

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

### 🐚 [Shell](shell/)

Shell configurations and scripts (coming soon)

---

### 💼 [Workspace](workspace/)

Project-specific documentation and guides:

#### [Distributed Tracing](workspace/distributed-tracing/)
- Complete guide for implementing distributed tracing
- High-Level Design (HLD) and Low-Level Design (LLD)
- Tool comparisons (Jaeger, Zipkin, etc.)
- Implementation and migration strategies
- Operational guides

#### [Java Platform](workspace/java_platform/)
- API Gateway documentation
- Dynamic routes configuration
- Request flow diagrams
- Compilation guides

---

## 🚀 Quick Setup

### 1. Clone Repository

```bash
git clone https://github.com/amarBitMan/dev-setup.git
cd dev-setup
```

### 2. Choose Your Tools

Pick what you need:

**For AI-Enhanced Development:**
```bash
cd mcp-instructions-server
# Follow SETUP_FOR_NEW_USERS.md
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

---

## 🌟 Highlights

### MCP Instructions Server

The **standout feature** of this repository. After setup:

- GitHub Copilot follows your workflow automatically
- Applies language-specific best practices
- Warns about common mistakes
- **Always asks before creating documentation**

**Example interaction:**

```
You: @github Add a REST API endpoint for user management

Copilot Phase 1: Let me clarify the requirements...
[Asks questions, confirms understanding]

Copilot Phase 2: Here's my implementation plan...
Should I create documentation for this endpoint?

You: Yes

Copilot Phase 3: [Implements with best practices]

Copilot Phase 4: [Summarizes changes and validation results]
```

---

## 📚 Documentation

Each directory has its own README with specific instructions:

- **[mcp-instructions-server/](mcp-instructions-server/)** - Complete setup guides
- **[docker/](docker/)** - Docker service configurations
- **[erlang/](erlang/)** - Erlang development guides
- **[git/](git/)** - Git configuration details
- **[workspace/](workspace/)** - Project documentation

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

**Start with the MCP Instructions Server** to supercharge your GitHub Copilot experience! 🚀
