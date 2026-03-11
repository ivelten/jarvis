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
├── forkIO: Discovery worker  (default: every 24 h, sleeps before first run)
│     └── Gemini → PostgreSQL (raw_content)
├── forkIO: Draft worker      (default: every 12 h, sleeps before first run)
│     └── PostgreSQL → Gemini → Discord thread
├── forkIO: Retry worker      (default: every 1 h, sleeps before first run)
│     └── PostgreSQL (publish_failed drafts) → GitHub → deploy
└── startBot (blocks main thread)
      ├── Ready               (registers slash commands)
      ├── InteractionCreate   (/discover, /draft, /subject, etc. in the commands channel)
      ├── ThreadCreate        (user creates a thread in the forum → custom post flow)
      └── MessageCreate       (feedback / approve / reject phrase in a review thread)
```

> **Note:** all background workers sleep for their full interval on startup before
> running for the first time. This avoids a burst of AI calls every time the
> process restarts. Use `/discover` or `/draft` in Discord to trigger an
> immediate run instead.

### Review flow

1. The draft worker picks the next pending content item, calls Gemini to generate a bilingual (EN + PT-BR) Markdown draft, and creates a new thread in your Discord forum channel.
2. The full draft is posted as file attachments in that thread.
3. You can type feedback freely in the thread — Gemini revises the draft and posts the updated version back.
4. When you are happy, type **`approve`** (or any approval phrase such as `publish`, `lgtm`, `looks good`, `ship it`, `done`, `go ahead`, `deploy`) in the thread.
5. The final draft body is committed to GitHub and the deploy workflow is triggered automatically. A notification is posted to your interaction channel when the deploy is dispatched. The thread is then **automatically archived and locked**.

Type **`reject`** (or `discard`, `cancel`, `abort`, `drop`) at any point to reject the draft — it is marked rejected in the database, logged, and the thread is archived.

### Custom post flow

You can also request a post on a specific topic without going through the discovery queue:

1. **Create a new thread** in the forum channel yourself.
2. In the thread starter message, describe what you want — the thread title is used as the topic hint and the message body as additional instructions for Gemini.
3. Jarvis detects the new thread (the `ThreadCreate` event), generates a bilingual draft, and posts it back to the same thread.
4. The review flow then proceeds identically to the automated draft flow above.

> **Note:** Jarvis only reacts to threads created by the configured `DISCORD_OWNER_ID`. Threads from other users are silently ignored.

### Publish failure and automatic retry

If the GitHub commit step fails (e.g. due to a network error or a temporary GitHub outage), the draft is **not** discarded or reset to a new draft. Instead it is marked `publish_failed` in the database and left untouched. The retry worker (default: every hour) automatically picks up all `publish_failed` drafts and re-attempts the commit and deploy, logging the outcome for each one. Once the commit succeeds the draft transitions to `published` and a notification is posted as normal.

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
4. Open **OAuth2 → URL Generator**, set scopes to `bot` **and** `applications.commands`, and grant these permissions:
   - Send Messages
   - Read Message History
   - Create Public Threads
   - Manage Threads
   - Use Slash Commands
5. Open the generated URL in a browser and invite the bot to your private Discord server.

### 2. Collect the Discord IDs

Enable **Developer Mode** in Discord: **Settings → Advanced → Developer Mode**.

- Right-click your server icon → **Copy Server ID** → `DISCORD_GUILD_ID`
- In your server, create a **Forum** channel (channel type: *Forum*) for reviews.
- Right-click that forum channel → **Copy Channel ID** → `DISCORD_CHANNEL_ID`
- Create a regular **Text** channel where you will type slash commands and receive bot notices.
- Right-click that text channel → **Copy Channel ID** → `DISCORD_INTERACTION_CHANNEL_ID`
- Right-click **your own username** → **Copy User ID** → `DISCORD_OWNER_ID`

> **Security note:** the bot only acts on interactions (slash commands, reactions, and thread messages) that originate from `DISCORD_OWNER_ID`. All others are silently ignored.

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
DISCORD_INTERACTION_CHANNEL_ID=123456789012345678
DISCORD_OWNER_ID=123456789012345678
```

The `DATABASE_URL` default (`postgresql://postgres:postgres@db:5432/jarvis`) already points to the devcontainer PostgreSQL service — no changes needed there.

### 6. Run the orchestrator

From the repository root:

```bash
./run.sh
```

The script loads `orchestrator/.env` automatically and starts the orchestrator via `cabal run`. The database schema is migrated on first start.

### 7. Review and publish

- Check your Discord forum channel — a new forum post appears for the draft.
- Open the post thread to read the full draft.
- Type feedback in the thread to request AI revisions.
- When satisfied, type `approve` to commit and deploy. The thread will be archived automatically.

### Slash commands

Once the bot is running, two slash commands are registered in your guild and available in the interaction channel (`DISCORD_INTERACTION_CHANNEL_ID`):

| Command | What it does |
| --- | --- |
| `/discover` | Immediately runs a content discovery cycle (Gemini → database). |
| `/draft` | Immediately runs a draft-generation cycle (database → Gemini → Discord thread). |
| `/subject <name>` | Adds a new subject of interest with a default interest score of 3. |
| `/disable-subject <id>` | Disables a subject by its numeric ID so it is excluded from discovery and drafting. |
| `/list-subjects` | Posts a numbered list of all currently enabled subjects as a `.md` file. |

All commands are restricted to the owner (`DISCORD_OWNER_ID`) and only work in the interaction channel (`DISCORD_INTERACTION_CHANNEL_ID`). Commands from any other user or channel are silently ignored.

### Approval and rejection keywords

When a review thread is active, plain text messages are interpreted as follows:

| Intent | Keywords (case-insensitive, partial match) |
| --- | --- |
| **Approve** | `approve`, `publish`, `lgtm`, `looks good`, `ship it`, `done`, `go ahead`, `deploy` |
| **Reject** | `reject`, `discard`, `cancel`, `abort`, `drop` |
| **Revision** | Anything else — treated as feedback and sent to Gemini for a revision. |

## Configuration reference

All configuration is read from environment variables. See `.env.example` for the full list with descriptions.

| Variable | Required | Default | Description |
| --- | --- | --- | --- |
| `DATABASE_URL` | ✅ | — | PostgreSQL connection string |
| `GEMINI_API_KEY` | ✅ | — | Google AI API key |
| `GEMINI_MODELS` | | `gemini-2.5-flash-lite,gemini-2.5-flash` | Comma-separated list of Gemini model names in priority order. On a rate-limit error (HTTP 429) the next model is tried automatically. |
| `GITHUB_TOKEN` | ✅ | — | PAT with `contents:write`, `actions:write` |
| `GITHUB_REPO_OWNER` | ✅ | — | GitHub user or org name |
| `GITHUB_REPO_NAME` | ✅ | — | Repository name |
| `GITHUB_BRANCH` | | `main` | Branch to commit posts to |
| `GITHUB_POSTS_PATH` | | `content/posts` | Path to Hugo posts inside the repo |
| `GITHUB_WORKFLOW_ID` | | `deploy.yml` | Workflow filename to dispatch |
| `DISCORD_BOT_TOKEN` | ✅ | — | Bot token (without `Bot` prefix) |
| `DISCORD_GUILD_ID` | ✅ | — | Server (guild) ID |
| `DISCORD_CHANNEL_ID` | ✅ | — | Forum channel ID for review threads |
| `DISCORD_INTERACTION_CHANNEL_ID` | ✅ | — | Text channel ID for slash commands and bot notices (`/discover`, `/draft`, etc.) |
| `DISCORD_OWNER_ID` | ✅ | — | Your Discord user ID — only interactions from this user are acted upon; all others are silently ignored |
| `DISCOVERY_INTERVAL_SECS` | | `86400` | Seconds to sleep between discovery runs (first run also delayed) |
| `DRAFT_INTERVAL_SECS` | | `43200` | Seconds to sleep between draft-generation runs (first run also delayed) |
| `RETRY_INTERVAL_SECS` | | `3600` | Seconds to sleep between publish-retry runs. The retry worker re-attempts any draft in the `publish_failed` state without generating a new draft or losing the approved content. |
