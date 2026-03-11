#!/bin/bash

# Check if .envrc file exists and automatically allow it with direnv
if [ -f "/workspaces/haskell/.envrc" ]; then
  echo "📁 Found .envrc file, running direnv allow..."
  direnv allow /workspaces/haskell
  echo "📂 direnv allow completed."
else
  echo "📁 No .envrc file found, skipping direnv allow."
fi
