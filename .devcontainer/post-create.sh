#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

echo "Running post-creation setup..."

# Verify GHC, Cabal, and HLS versions
echo "Verifying Haskell toolchain..."
ghc --version
cabal --version
haskell-language-server-wrapper --version

# Update Cabal package index
echo "Updating Cabal package index..."
cabal update

# Install Haskell tooling
echo "Installing ormolu (Haskell formatter)..."
cabal install ormolu --overwrite-policy=always

# Verify critical executable commands
echo "Verifying system utilities..."
for cmd in make gcc curl git psql pkg-config direnv socat ps; do
    if ! command -v $cmd &> /dev/null; then
        echo "❌ $cmd is not installed!"
        exit 1
    fi
    echo "✅ $cmd is installed."
done

# Create a welcome message
echo "Creating welcome message..."
cat > /home/vscode/.welcome_message << EOF
🖥️ Welcome to the Jarvis project development container!

This container has been set up with:
- GHC 9.6.7
- Cabal 3.10.3.0
- HLS 2.12.0.0
- ormolu (Haskell formatter)
- Required system dependencies (make, gcc, curl, git, psql, pkg-config, direnv, socat, ps)

To access the database, you can run the following command, or access it directly via SQLTools plugin:

psql -h db -U postgres -d jarvis
EOF

# Add welcome message to zshrc so it prints on new terminals
echo "echo -e \"\$(cat /home/vscode/.welcome_message)\"" >> /home/vscode/.zshrc

echo "Post-creation setup complete!"
