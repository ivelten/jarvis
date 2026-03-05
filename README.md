# Jarvis

A high-reliability orchestration engine that bridges the gap between .NET expertise and functional programming mastery through AI-assisted technical content delivery.

## Overview

**Jarvis** is a robust, Haskell-based orchestration engine designed to manage the full lifecycle of the **Daily Haskell In Real Life** blog. Born from over 15 years of experience building high-consistency backend systems in the .NET ecosystem, this project serves as a pragmatic bridge for developers transitioning from enterprise imperative environments to the functional elegance of Haskell.

By leveraging the **Gemini API** and a **GitOps-driven deployment pipeline**, Jarvis automates topic selection — prioritised by real-world engagement metrics — while maintaining a strict "human-in-the-loop" review process via Discord. This project exemplifies a "Cyborg" workflow, where AI-powered research and drafting meet senior-level technical oversight.

## Core Objectives

- **Paradigm Bridge**: Document the transition from C#/.NET to Haskell through a senior-to-senior lens.
- **Data-Driven Content**: Utilise GitHub engagement and SEO metrics to prioritise technical topics.
- **Engineering Excellence**: Showcase a complete Haskell ecosystem, including database persistence, multimodal AI integration, and automated CI/CD.
- **GitOps Maturity**: Automate the delivery of a Hugo-based site hosted on GitHub Pages with zero manual intervention.

## Architecture

The orchestrator runs four parallel workers, each on its own independent schedule:

```text
main thread
├── forkIO: Discovery worker  (default: every 24 h)
│     └── Gemini → PostgreSQL (raw_content)
├── forkIO: Draft worker      (default: every 1 h)
│     └── PostgreSQL → Gemini → Discord thread
└── startBot (blocks main thread)
      ├── MessageReactionAdd (✅ / ❌ on embed)
      └── MessageCreate      (feedback / approvalphrase in thread)
```

### Review flow

1. The draft worker picks the next pending content item, calls Gemini to generate a Markdown draft, and creates a new thread in your Discord forum channel with the embed as the starter post.
2. The full draft is posted as the first reply in that thread.
3. You can type feedback freely in the thread — Gemini revises the draft and posts the updated version back.
4. When you are happy, approve in one of two ways:
   - React with **✅** on the forum post, **or**
   - Type an approval phrase in the thread (`publish`, `approve`, `lgtm`, `looks good`, `ship it`, `done`, `go ahead`, `deploy`).
5. The final draft body is committed to GitHub and the deploy workflow is triggered automatically.

React with **❌** at any point to reject the draft (it is removed from the queue and logged).

## Development environment

> **This project is designed for [Visual Studio Code](https://code.visualstudio.com/) with the official Dev Containers extension.**
>
> AI-focused editors forked from VS Code (such as Cursor, Windsurf, and similar) currently have incomplete or broken devcontainer support. If you use one of those editors, you will likely encounter issues bringing the environment up. Plain VS Code is the recommended and tested host.

### Prerequisites

- [Visual Studio Code](https://code.visualstudio.com/)
- [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) (`ms-vscode-remote.remote-containers`)
- [Docker Desktop](https://www.docker.com/products/docker-desktop/) (or Docker Engine on Linux)

### Starting the devcontainer

1. Clone the repository and open the folder in VS Code.
2. When prompted, click **Reopen in Container** (or run **Dev Containers: Reopen in Container** from the command palette).
3. VS Code builds the image and starts a PostgreSQL sidecar automatically. Both services are defined in `.devcontainer/docker-compose.yml`.
4. Once inside the container, GHC 9.6 and Cabal are ready on the `PATH`.

## How to run locally

### 1. Create a Discord bot

1. Go to <https://discord.com/developers/applications> → **New Application**.
2. Open the **Bot** tab → **Reset Token** and copy the value (you will need it for `DISCORD_BOT_TOKEN`).
3. On the same page, under **Privileged Gateway Intents**, enable:
   - ✅ **Message Content Intent**
   - ✅ **Server Members Intent**
4. Open **OAuth2 → URL Generator**, set scopes to `bot`, and grant these permissions:
   - Send Messages
   - Read Message History
   - Add Reactions
   - Create Public Threads
5. Open the generated URL in a browser and invite the bot to your private Discord server.

### 2. Collect the Discord IDs

Enable **Developer Mode** in Discord: **Settings → Advanced → Developer Mode**.

- Right-click your server icon → **Copy Server ID** → `DISCORD_GUILD_ID`
- In your server, create a **Forum** channel (channel type: *Forum*) for reviews.
- Right-click that forum channel → **Copy Channel ID** → `DISCORD_CHANNEL_ID`

### 3. Get a Gemini API key

Create a free key at <https://aistudio.google.com/app/apikey> and copy it into `GEMINI_API_KEY`.

### 4. Set up GitHub access (optional for local testing)

Create a **classic** Personal Access Token at <https://github.com/settings/tokens/new> and enable these two scopes:

- ✅ **`repo`** — full control of repositories (read/write file contents)
- ✅ **`workflow`** — trigger and update GitHub Actions workflows

> **Note:** the scopes above are for the classic token page. If you use a fine-grained PAT instead, the equivalent permissions are `Contents: Read and Write` and `Actions: Read and Write`.

Fill in the `GITHUB_*` variables with the token and your repository details. You may leave these blank during initial testing — the bot and discovery pipeline will work fine, and only the final commit step will log an error without crashing.

### 5. Configure environment variables

```bash
cd orchestrator
cp .env.example .env
```

Open `.env` and fill in at minimum:

```dotenv
GEMINI_API_KEY=your-key-here
DISCORD_BOT_TOKEN=your-token-here
DISCORD_GUILD_ID=123456789012345678
DISCORD_CHANNEL_ID=123456789012345678
```

The `DATABASE_URL` default (`postgresql://postgres:postgres@db:5432/jarvis`) already points to the devcontainer PostgreSQL service — no changes needed there.

### 6. Run the orchestrator

From the repository root:

```bash
./run.sh
```

The script loads `orchestrator/.env` automatically and starts the orchestrator via `cabal run`. The database schema is migrated on first start. You will see output similar to:

```text
[Jarvis] Starting orchestrator...
[Jarvis] Database ready.
[Jarvis] All workers started. Discord bot running...
[Discovery] Discovering content via Gemini...
[Discovery] Done.
[Drafts] Generating draft for: <title>
[Drafts] Draft sent to Discord for review.
```

### 7. Review and publish

- Check your Discord forum channel — a new forum post appears for the draft.
- Open the post thread to read the full draft.
- Type feedback in the thread to request AI revisions.
- When satisfied, type `publish` (or react ✅) to commit and deploy.

## Configuration reference

All configuration is read from environment variables. See `.env.example` for the full list with descriptions.

| Variable | Required | Default | Description |
| --- | --- | --- | --- |
| `DATABASE_URL` | ✅ | — | PostgreSQL connection string |
| `GEMINI_API_KEY` | ✅ | — | Google AI API key |
| `GEMINI_MODEL` | | `gemini-2.5-flash` | Gemini model name |
| `GITHUB_TOKEN` | ✅ | — | PAT with `contents:write`, `actions:write` |
| `GITHUB_REPO_OWNER` | ✅ | — | GitHub user or org name |
| `GITHUB_REPO_NAME` | ✅ | — | Repository name |
| `GITHUB_BRANCH` | | `main` | Branch to commit posts to |
| `GITHUB_POSTS_PATH` | | `content/posts` | Path to Hugo posts inside the repo |
| `GITHUB_WORKFLOW_ID` | | `deploy.yml` | Workflow filename to dispatch |
| `DISCORD_BOT_TOKEN` | ✅ | — | Bot token (without `Bot` prefix) |
| `DISCORD_GUILD_ID` | ✅ | — | Server (guild) ID |
| `DISCORD_CHANNEL_ID` | ✅ | — | Forum channel ID for review threads |
| `DISCOVERY_INTERVAL_SECS` | | `86400` | How often to discover new content (seconds) |
| `DRAFT_INTERVAL_SECS` | | `3600` | How often to generate a new draft (seconds) |

## Running the tests

```bash
cd orchestrator
cabal test
```

The test suite does not require a live database, Discord connection, or Gemini account — all external dependencies are faked in-process.
