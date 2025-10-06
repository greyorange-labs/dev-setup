#!/bin/bash

# Git Configuration Management Script
# This script applies git configuration from the centralized gitconfig file

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GITCONFIG_FILE="$SCRIPT_DIR/gitconfig"

echo "=== Git Configuration Management ==="
echo "Configuration file: $GITCONFIG_FILE"
echo ""

if [ ! -f "$GITCONFIG_FILE" ]; then
    echo "Error: gitconfig file not found at $GITCONFIG_FILE"
    exit 1
fi

echo "Current git configuration (editor and pager):"
echo "Editor: $(git config --global core.editor || echo 'Not set')"
echo "Pager: $(git config --global core.pager || echo 'Not set')"
echo ""

read -p "Do you want to apply the centralized git configuration? (y/N): " -n 1 -r
echo ""

if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "Applying git configuration..."
    
    # Apply the configuration
    git config --global --replace-all include.path "$GITCONFIG_FILE"
    
    echo "Configuration applied successfully!"
    echo ""
    echo "New configuration:"
    echo "Editor: $(git config --global core.editor)"
    echo "Pager: $(git config --global core.pager)"
    echo ""
    echo "Note: Edit $GITCONFIG_FILE to modify your git settings"
else
    echo "Configuration not applied."
fi
