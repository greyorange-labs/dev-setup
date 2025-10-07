#!/bin/bash

# Git Squash Script - New Version
# Usage: ./squash-commits.sh <number_of_commits>
# Squashes commits using git reset --soft (no editor prompts)

set -e

# Function to display usage
usage() {
    echo "Usage: $0 <number_of_commits>"
    echo "Example: $0 3  # Squashes the last 3 commits"
    echo ""
    echo "This script will:"
    echo "  1. Show commits to be squashed"
    echo "  2. Use the earliest commit's message as default"
    echo "  3. Prompt you ONCE to edit the final commit message"
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

# Check if there are any uncommitted changes
if ! git diff-index --quiet HEAD --; then
    echo "Error: You have uncommitted changes. Please commit or stash them first."
    exit 1
fi

# Get the earliest (oldest) commit message that we want to keep as default
EARLIEST_COMMIT_HASH=$(git rev-parse HEAD~$((COMMIT_COUNT-1)))
EARLIEST_COMMIT_MESSAGE=$(git log --format=%B -n 1 "$EARLIEST_COMMIT_HASH")

echo "=== Git Squash Script ==="
echo "About to squash the last $COMMIT_COUNT commits"
echo ""

# Show the commits that will be squashed
echo "Commits to be squashed:"
git log --oneline -n "$COMMIT_COUNT"
echo ""

# Show the default commit message (from earliest commit)
echo "Default commit message (from earliest commit):"
echo "----------------------------------------"
echo "$EARLIEST_COMMIT_MESSAGE"
echo "----------------------------------------"
echo ""

# Prompt for confirmation
read -p "Do you want to proceed with squashing these commits? (y/N): " -n 1 -r
echo ""
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Operation cancelled."
    exit 0
fi

echo "Squashing commits using git reset --soft approach..."

# Get the commit before the range we want to squash
TARGET_COMMIT=$(git rev-parse HEAD~"$COMMIT_COUNT")

# Reset to the target commit but keep all changes staged
git reset --soft "$TARGET_COMMIT"

echo ""
echo "Commits have been squashed. Now you can edit the final commit message."
echo ""
echo "Current default message (from earliest commit):"
echo "----------------------------------------"
echo "$EARLIEST_COMMIT_MESSAGE"
echo "----------------------------------------"
echo ""
echo "Enter your commit message (or press Enter to use the default above):"

# Read the commit message from user input
read -r USER_MESSAGE

# Use the user message if provided, otherwise use the earliest commit message
if [ -n "$USER_MESSAGE" ]; then
    FINAL_MESSAGE="$USER_MESSAGE"
else
    FINAL_MESSAGE="$EARLIEST_COMMIT_MESSAGE"
fi

# Create the squashed commit with no editor prompts
git commit -m "$FINAL_MESSAGE"

echo ""
echo "=== Squash operation completed successfully! ==="
echo "New commit hash: $(git rev-parse HEAD)"
echo ""
echo "Final commit:"
git log --oneline -1
echo ""
echo "Final commit message:"
echo "----------------------------------------"
git log --format=%B -n 1 HEAD
echo "----------------------------------------"