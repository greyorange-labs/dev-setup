#!/bin/bash

# Git Squash Script
# Usage: ./git-squash.sh <number_of_commits>
# Squashes the specified number of commits and uses the oldest commit's message as default

set -e

# Function to display usage
usage() {
    echo "Usage: $0 <number_of_commits>"
    echo "Example: $0 3  # Squashes the last 3 commits"
    echo ""
    echo "This script will:"
    echo "  1. Squash the specified number of commits"
    echo "  2. Use the oldest commit's message as the default"
    echo "  3. Prompt you to optionally enter a new commit message"
    exit 1
}

# Check if argument is provided
if [ $# -ne 1 ]; then
    usage
fi

# Check if argument is a positive integer
if ! [[ "$1" =~ ^[0-9]+$ ]] || [ "$1" -le 0 ]; then
    echo "Error: Please provide a positive integer for the number of commits"
    usage
fi

COMMIT_COUNT=$1

# Check if we're in a git repository
if ! git rev-parse --git-dir > /dev/null 2>&1; then
    echo "Error: Not in a git repository"
    exit 1
fi

# Check if we have enough commits
TOTAL_COMMITS=$(git rev-list --count HEAD)
if [ "$COMMIT_COUNT" -gt "$TOTAL_COMMITS" ]; then
    echo "Error: Only $TOTAL_COMMITS commits available, cannot squash $COMMIT_COUNT commits"
    exit 1
fi

if [ "$COMMIT_COUNT" -eq 1 ]; then
    echo "Error: Cannot squash just 1 commit. Please specify 2 or more commits."
    exit 1
fi

# Get the oldest commit message (the one we want to keep as default)
OLDEST_COMMIT_HASH=$(git rev-parse HEAD~$((COMMIT_COUNT-1)))
OLDEST_COMMIT_MESSAGE=$(git log --format=%B -n 1 "$OLDEST_COMMIT_HASH")

echo "=== Git Squash Script ==="
echo "About to squash the last $COMMIT_COUNT commits"
echo ""

# Show the commits that will be squashed
echo "Commits to be squashed:"
git log --oneline -n "$COMMIT_COUNT"
echo ""

# Show the default commit message
echo "Default commit message (from oldest commit):"
echo "----------------------------------------"
echo "$OLDEST_COMMIT_MESSAGE"
echo "----------------------------------------"
echo ""

# Prompt for confirmation
read -p "Do you want to proceed with squashing these commits? (y/N): " -n 1 -r
echo ""
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Operation cancelled."
    exit 0
fi

# Start interactive rebase
echo "Starting interactive rebase..."

# Create a temporary script for the rebase
TEMP_SCRIPT=$(mktemp)
trap "rm -f $TEMP_SCRIPT" EXIT

# Generate the rebase script
# First commit should be 'pick', rest should be 'squash'
git log --reverse --format="%H" -n "$COMMIT_COUNT" | while IFS= read -r commit; do
    if [ -z "$FIRST_COMMIT" ]; then
        echo "pick $commit"
        FIRST_COMMIT=1
    else
        echo "squash $commit"
    fi
done > "$TEMP_SCRIPT"

# Perform the rebase
GIT_SEQUENCE_EDITOR="cp $TEMP_SCRIPT" git rebase -i HEAD~"$COMMIT_COUNT"

# After the rebase, prompt for new commit message
echo ""
echo "Squash completed successfully!"
echo ""
echo "Current commit message:"
echo "----------------------------------------"
CURRENT_MESSAGE=$(git log --format=%B -n 1 HEAD)
echo "$CURRENT_MESSAGE"
echo "----------------------------------------"
echo ""

read -p "Do you want to change the commit message? (y/N): " -n 1 -r
echo ""

if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "Enter your new commit message (press Ctrl+D when finished):"
    echo "Current message is shown above for reference."
    echo ""
    
    # Use git commit --amend to change the message
    git commit --amend
    echo ""
    echo "Commit message updated successfully!"
else
    echo "Keeping the current commit message."
fi

echo ""
echo "=== Squash operation completed! ==="
echo "New commit hash: $(git rev-parse HEAD)"
echo ""
echo "Final commit:"
git log --oneline -1