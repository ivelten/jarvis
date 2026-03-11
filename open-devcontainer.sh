#!/bin/bash

REBUILD=false

# Parse options
while getopts "r" opt; do
  case $opt in
    r) REBUILD=true ;;
    *) echo "Usage: $0 [-r (rebuild)]" ; exit 1 ;;
  esac
done

echo "🔍 Checking host dependencies..."

# Check Node.js
if ! command -v node &> /dev/null; then
    echo "❌ Node.js is not installed. You need it to run the Dev Container CLI."
    exit 1
fi
echo "✅ Node.js: $(node -v)"

# Check Dev Container CLI
if ! command -v devcontainer &> /dev/null; then
    echo "❌ Dev Container CLI (@devcontainers/cli) is not installed."
    echo "👉 Please run: npm install -g @devcontainers/cli"
    # Tip: if you get a permission error, remember to use sudo or configure ~/.npm-global
    exit 1
fi
echo "✅ Dev Container CLI is ready."

if [ "$REBUILD" = true ]; then
    echo "🏗️ Rebuilding container (this might take a while)..."
    devcontainer build --workspace-folder .
fi

echo "🚀 Turning on container..."
if ! devcontainer up --workspace-folder . ; then
    echo "❌ Failed to start container. Is Docker Desktop running?"
    exit 1
fi

echo "⚓️ Entering the container..."
# Try to enter with zsh, if it fails fall back to bash
devcontainer exec --workspace-folder . zsh || devcontainer exec --workspace-folder . /bin/bash

read -p "Do you want to shut down the container now? (y/n) " -n 1 -r
echo

if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "😴 Shutting down..."

    # We look for a container that matches the current folder name and "devcontainer" in its name
    FOLDER_NAME=$(basename "$PWD" | tr '[:upper:]' '[:lower:]')

    # We look for any container that contains the folder name and "devcontainer"
    CONTAINER_ID=$(docker ps -q --filter "name=${FOLDER_NAME}.*devcontainer")

    if [ -z "$CONTAINER_ID" ]; then
        # Plan B: Generic search for any active devcontainer with the folder name label
        CONTAINER_ID=$(docker ps -q --filter "label=devcontainer.local_folder=$PWD")
    fi

    if [ -n "$CONTAINER_ID" ]; then
        docker stop "$CONTAINER_ID"
        echo "✅ Container $CONTAINER_ID stopped."
    else
        echo "❌ Could not find a running container for '$FOLDER_NAME'."
        echo "💡 Tip: Run 'docker ps' to see all active containers."
    fi
fi
