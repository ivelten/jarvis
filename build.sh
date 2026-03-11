#!/usr/bin/env bash
set -euo pipefail

CLEAN=false
for arg in "$@"; do
  case "$arg" in
    --clean) CLEAN=true ;;
    *) echo "Unknown argument: $arg"; exit 1 ;;
  esac
done

cd "$(dirname "$0")/orchestrator"

if [ "$CLEAN" = true ]; then
  echo "Cleaning build artifacts..."
  rm -rf dist-newstyle
fi

echo "Updating cabal dependencies..."
cabal update

echo "Building the project..."
cabal build all

echo "Running tests..."
cabal test all
