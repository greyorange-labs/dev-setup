#!/bin/bash

# MCP Instructions Server - Quick Setup Script

set -e

echo "🚀 Setting up MCP Instructions Server..."
echo ""

# Check Node.js version
echo "📋 Checking Node.js version..."
NODE_VERSION=$(node --version)
echo "   Node.js version: $NODE_VERSION"

# Install dependencies
echo ""
echo "📦 Installing dependencies..."
npm install

# Build TypeScript
echo ""
echo "🔨 Building TypeScript..."
npm run build

# Seed database
echo ""
echo "🌱 Seeding database..."
npm run seed

# Success message
echo ""
echo "✅ Setup complete!"
echo ""
echo "📊 Database location: $(pwd)/instructions.db"
echo ""
echo "🎯 Next steps:"
echo "   1. Add to VS Code settings.json:"
echo '      "github.copilot.chat.mcp.servers": {'
echo '        "instructions": {'
echo '          "command": "node",'
echo "          \"args\": [\"$(pwd)/build/index.js\"],"
echo '          "env": {'
echo '            "VSCODE_CURRENT_FILE": "${file}"'
echo '          }'
echo '        }'
echo '      }'
echo ""
echo "   2. Restart VS Code"
echo ""
echo "   3. Start using GitHub Copilot with instructions!"
echo ""
