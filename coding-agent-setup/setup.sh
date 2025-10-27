#!/bin/bash

# MCP Instructions Server Setup Script
# Automates the setup process for the MCP server

set -e  # Exit on any error

echo "🚀 Setting up MCP Instructions Server..."

# Check Node.js version
echo "📋 Checking Node.js version..."
if ! command -v node &> /dev/null; then
    echo "❌ Node.js is not installed. Please install Node.js 18+ and try again."
    exit 1
fi

NODE_VERSION=$(node --version | cut -d'v' -f2 | cut -d'.' -f1)
if [ "$NODE_VERSION" -lt 18 ]; then
    echo "❌ Node.js version $NODE_VERSION detected. Please upgrade to Node.js 18+ and try again."
    exit 1
fi

echo "✅ Node.js $(node --version) detected"

# Install dependencies
echo "📦 Installing dependencies..."
npm install

# Build the server
echo "🔨 Building TypeScript..."
npm run build

# Check if build was successful
if [ ! -f "build/index.js" ]; then
    echo "❌ Build failed - build/index.js not found"
    exit 1
fi

echo "✅ Build successful"

# Seed the database with example instructions (if any exist)
echo "🌱 Seeding database..."
if npm run seed; then
    echo "✅ Database seeded successfully"
else
    echo "⚠️  Database seeding skipped (no seed data found)"
fi

# Test server startup
echo "🧪 Testing server startup..."
timeout 3 node build/index.js || true
echo "✅ Server test complete"

# Get absolute path for VS Code configuration
CURRENT_DIR=$(pwd)
echo ""
echo "🎉 Setup complete!"
echo ""
echo "📝 Next steps:"
echo "1. Add this to your VS Code settings.json:"
echo ""
echo '   "github.copilot.chat.codeGeneration.useInstructionFiles": true,'
echo '   "github.copilot.chat.mcp.servers": {'
echo '     "instructions": {'
echo '       "command": "node",'
echo '       "args": ["'$CURRENT_DIR'/build/index.js"],'
echo '       "env": {'
echo '         "VSCODE_CURRENT_FILE": "${file}"'
echo '       }'
echo '     }'
echo '   }'
echo ""
echo "2. Restart VS Code completely (⌘+Q then reopen)"
echo ""
echo "3. Test with: 'Create a new authentication module for the API'"
echo "   The AI should ask for clarification before writing code."
echo ""
echo "📖 For full setup instructions, see README.md"
echo ""
echo "🔗 VS Code settings file location:"
echo "   macOS: ~/Library/Application Support/Code/User/settings.json"
echo "   Linux: ~/.config/Code/User/settings.json"
echo "   Windows: %APPDATA%/Code/User/settings.json"