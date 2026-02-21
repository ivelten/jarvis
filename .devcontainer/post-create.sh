#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

echo "Running post-creation setup..."

# Verify GHC, Cabal, and HLS versions
echo "Verifying Haskell toolchain..."
ghc --version
cabal --version
haskell-language-server-wrapper --version

# Verify critical executable commands
echo "Verifying system utilities..."
for cmd in make gcc curl git psql pkg-config direnv socat ps; do
    if ! command -v $cmd &> /dev/null; then
        echo "ERROR: $cmd is not installed!"
        exit 1
    fi
    echo "✓ $cmd is installed"
done

# Create a welcome message
echo "Creating welcome message..."
cat > /home/vscode/.welcome_message << 'EOF'

=======================================================
 Welcome to the Jarvis project development container!
=======================================================

This container has been set up with:
 - GHC 9.6.7
 - Cabal 3.10.3.0
 - All required system dependencies (libpq, etc.)

To start the database:    (Already running via Compose!)
To access the database:   psql -h db -U postgres -d jarvis

EOF

# Add welcome message to bashrc so it prints on new terminals
if ! grep -q ".welcome_message" /home/vscode/.bashrc; then
    echo '[[ $- == *i* ]] && cat /home/vscode/.welcome_message' >> /home/vscode/.bashrc
fi

echo "Post-creation setup complete!"
