#!/bin/bash#!/bin/bash



# Git Squash Script# Git Squash Script

# Usage: ./git-squash.sh <number_of_commits># Usage: ./git-squash.sh <number_of_commits>

# Squashes the specified number of commits using git reset --soft (no editor prompts)# Squashes the specified number of commits and uses the oldest commit's message as default



set -eset -e



# Function to display usage# Function to display usage

usage() {usage() {

    echo "Usage: $0 <number_of_commits>"    echo "Usage: $0 <number_of_commits>"

    echo "Example: $0 3  # Squashes the last 3 commits"    echo "Example: $0 3  # Squashes the last 3 commits"

    echo ""    echo ""

    echo "This script will:"    echo "This script will:"

    echo "  1. Show commits to be squashed"    echo "  1. Squash the specified number of commits"

    echo "  2. Use the earliest commit's message as default"    echo "  2. Use the oldest commit's message as the default"

    echo "  3. Prompt you once to edit the final commit message"    echo "  3. Prompt you to optionally enter a new commit message"

    exit 1    exit 1

}}



# Check if argument is provided# Check if argument is provided

if [ $# -ne 1 ]; thenif [ $# -ne 1 ]; then

    usage    usage

fifi



# Check if argument is a positive integer# Check if argument is a positive integer

if ! [[ "$1" =~ ^[0-9]+$ ]] || [ "$1" -le 0 ]; thenif ! [[ "$1" =~ ^[0-9]+$ ]] || [ "$1" -le 0 ]; then

    echo "Error: Please provide a positive integer for the number of commits"    echo "Error: Please provide a positive integer for the number of commits"

    usage    usage

fifi



COMMIT_COUNT=$1COMMIT_COUNT=$1



# Check if we're in a git repository# Check if we're in a git repository

if ! git rev-parse --git-dir > /dev/null 2>&1; thenif ! git rev-parse --git-dir > /dev/null 2>&1; then

    echo "Error: Not in a git repository"    echo "Error: Not in a git repository"

    exit 1    exit 1

fifi



# Check if we have enough commits# Check if we have enough commits

TOTAL_COMMITS=$(git rev-list --count HEAD)TOTAL_COMMITS=$(git rev-list --count HEAD)

if [ "$COMMIT_COUNT" -gt "$TOTAL_COMMITS" ]; thenif [ "$COMMIT_COUNT" -gt "$TOTAL_COMMITS" ]; then

    echo "Error: Only $TOTAL_COMMITS commits available, cannot squash $COMMIT_COUNT commits"    echo "Error: Only $TOTAL_COMMITS commits available, cannot squash $COMMIT_COUNT commits"

    exit 1    exit 1

fifi



if [ "$COMMIT_COUNT" -eq 1 ]; thenif [ "$COMMIT_COUNT" -eq 1 ]; then

    echo "Error: Cannot squash just 1 commit. Please specify 2 or more commits."    echo "Error: Cannot squash just 1 commit. Please specify 2 or more commits."

    exit 1    exit 1

fifi



# Check if there are any uncommitted changes# Get the oldest commit message (the one we want to keep as default)

if ! git diff-index --quiet HEAD --; thenOLDEST_COMMIT_HASH=$(git rev-parse HEAD~$((COMMIT_COUNT-1)))

    echo "Error: You have uncommitted changes. Please commit or stash them first."OLDEST_COMMIT_MESSAGE=$(git log --format=%B -n 1 "$OLDEST_COMMIT_HASH")

    exit 1

fiecho "=== Git Squash Script ==="

echo "About to squash the last $COMMIT_COUNT commits"

# Get the earliest (oldest) commit message that we want to keep as defaultecho ""

EARLIEST_COMMIT_HASH=$(git rev-parse HEAD~$((COMMIT_COUNT-1)))

EARLIEST_COMMIT_MESSAGE=$(git log --format=%B -n 1 "$EARLIEST_COMMIT_HASH")# Show the commits that will be squashed

echo "Commits to be squashed:"

echo "=== Git Squash Script ==="git log --oneline -n "$COMMIT_COUNT"

echo "About to squash the last $COMMIT_COUNT commits"echo ""

echo ""

# Show the default commit message

# Show the commits that will be squashedecho "Default commit message (from oldest commit):"

echo "Commits to be squashed:"echo "----------------------------------------"

git log --oneline -n "$COMMIT_COUNT"echo "$OLDEST_COMMIT_MESSAGE"

echo ""echo "----------------------------------------"

echo ""

# Show the default commit message (from earliest commit)

echo "Default commit message (from earliest commit):"# Prompt for confirmation

echo "----------------------------------------"read -p "Do you want to proceed with squashing these commits? (y/N): " -n 1 -r

echo "$EARLIEST_COMMIT_MESSAGE"echo ""

echo "----------------------------------------"if [[ ! $REPLY =~ ^[Yy]$ ]]; then

echo ""    echo "Operation cancelled."

    exit 0

# Prompt for confirmationfi

read -p "Do you want to proceed with squashing these commits? (y/N): " -n 1 -r

echo ""# Start interactive rebase

if [[ ! $REPLY =~ ^[Yy]$ ]]; thenecho "Starting interactive rebase..."

    echo "Operation cancelled."

    exit 0# Create a temporary script for the rebase

fiTEMP_SCRIPT=$(mktemp)

trap "rm -f $TEMP_SCRIPT" EXIT

echo "Squashing commits using git reset --soft approach..."

# Generate the rebase script

# Get the commit before the range we want to squash# First commit should be 'pick', rest should be 'squash'

TARGET_COMMIT=$(git rev-parse HEAD~"$COMMIT_COUNT")git log --reverse --format="%H" -n "$COMMIT_COUNT" | while IFS= read -r commit; do

    if [ -z "$FIRST_COMMIT" ]; then

# Reset to the target commit but keep all changes staged        echo "pick $commit"

git reset --soft "$TARGET_COMMIT"        FIRST_COMMIT=1

    else

echo ""        echo "squash $commit"

echo "Commits have been squashed. Now you can edit the final commit message."    fi

echo ""done > "$TEMP_SCRIPT"

echo "Current default message (from earliest commit):"

echo "----------------------------------------"# Perform the rebase with environment variables to avoid editor prompts

echo "$EARLIEST_COMMIT_MESSAGE"echo "Starting interactive rebase..."

echo "----------------------------------------"export GIT_SEQUENCE_EDITOR="cp $TEMP_SCRIPT"

echo ""export GIT_EDITOR="true"  # Use 'true' command to avoid editor for commit message during rebase

echo "Enter your commit message (or press Enter to use the default above):"git rebase -i HEAD~"$COMMIT_COUNT"



# Read the commit message from user input# After the rebase, prompt for new commit message

read -r USER_MESSAGEecho ""

echo "Squash completed successfully!"

# Use the user message if provided, otherwise use the earliest commit messageecho ""

if [ -n "$USER_MESSAGE" ]; thenecho "Current commit message:"

    FINAL_MESSAGE="$USER_MESSAGE"echo "----------------------------------------"

elseCURRENT_MESSAGE=$(git log --format=%B -n 1 HEAD)

    FINAL_MESSAGE="$EARLIEST_COMMIT_MESSAGE"echo "$CURRENT_MESSAGE"

fiecho "----------------------------------------"

echo ""

# Create the squashed commit with no editor prompts

git commit -m "$FINAL_MESSAGE"read -p "Do you want to change the commit message? (y/N): " -n 1 -r

echo ""

echo ""

echo "=== Squash operation completed successfully! ==="if [[ $REPLY =~ ^[Yy]$ ]]; then

echo "New commit hash: $(git rev-parse HEAD)"    echo ""

echo ""    echo "Enter your new commit message:"

echo "Final commit:"    echo "Type your message and press Enter when finished."

git log --oneline -1    echo ""

echo ""    read -r NEW_MESSAGE

echo "Final commit message:"

echo "----------------------------------------"    if [ -n "$NEW_MESSAGE" ]; then

git log --format=%B -n 1 HEAD        # Use git commit --amend with -m to avoid opening editor

echo "----------------------------------------"        git commit --amend -m "$NEW_MESSAGE"
        echo ""
        echo "Commit message updated successfully!"
    else
        echo "Empty message provided. Keeping the current commit message."
    fi
else
    echo "Keeping the current commit message."
fi

echo ""
echo "=== Squash operation completed! ==="
echo "New commit hash: $(git rev-parse HEAD)"
echo ""
echo "Final commit:"
git log --oneline -1