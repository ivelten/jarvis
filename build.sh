#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/orchestrator"

echo "Updating cabal dependencies..."
cabal update

echo "Building the project..."
cabal build all

echo "Running tests..."
cabal test all
