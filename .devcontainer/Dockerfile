FROM ivelten/haskell-devcontainer

USER root

RUN apt-get update && apt-get install -y \
    libpq-dev \
    postgresql-client \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

USER vscode

RUN cabal install hspec-discover
