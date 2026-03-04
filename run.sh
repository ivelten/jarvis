#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/orchestrator"

if [[ ! -f .env ]]; then
  echo "Error: orchestrator/.env not found."
  echo "Copy orchestrator/.env.example to orchestrator/.env and fill in your credentials."
  exit 1
fi

echo "Loading environment from .env..."
set -a
# shellcheck source=/dev/null
source .env
set +a

echo "Starting Jarvis orchestrator..."
exec cabal run orchestrator
