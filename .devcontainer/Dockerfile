FROM mcr.microsoft.com/devcontainers/base:debian

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    git \
    libffi-dev \
    libgmp-dev \
    libpq-dev \
    libssl-dev \
    postgresql-client \
    pkg-config \
    zlib1g-dev \
    direnv \
    socat \
    procps \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Switch to the vscode user for subsequent operations
USER vscode

# Ensures shell runs in bash for following commands
SHELL ["/bin/bash", "-c"]

# Set ENV PATH so that ghcup and cabal are available in all shells without sourcing
ENV PATH="/home/vscode/.cabal/bin:/home/vscode/.local/bin:/home/vscode/.ghcup/bin:$PATH"

# Installs GHCup + GHC + Cabal (no HLS yet)
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=9.6.7 \
    BOOTSTRAP_HASKELL_CABAL_VERSION=3.10.3.0 \
    BOOTSTRAP_HASKELL_INSTALL_HLS=0 \
    sh

# Hook direnv
RUN echo 'eval "$(direnv hook bash)"' >> ~/.bashrc

# Installs and configures HLS
RUN ghcup install hls 2.12.0.0 --set

# Configure cabal for better performance
RUN mkdir -p ~/.cabal && \
    echo "jobs: \$ncpus" >> ~/.cabal/config && \
    echo "documentation: False" >> ~/.cabal/config

# Set working directory
WORKDIR /workspaces/jarvis

# Reset environment variable
ENV DEBIAN_FRONTEND=dialog