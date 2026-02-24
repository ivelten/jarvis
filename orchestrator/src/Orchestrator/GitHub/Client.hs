module Orchestrator.GitHub.Client
  ( GitHubConfig (..),
    commitPost,
    triggerDeploy,
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Runtime configuration for the GitHub client.
data GitHubConfig = GitHubConfig
  { -- | Personal access token (from environment).
    ghToken :: !Text,
    -- | Repository owner username, e.g. @"yourusername"@.
    ghRepoOwner :: !Text,
    -- | Repository name, e.g. @"yourusername.github.io"@.
    ghRepoName :: !Text,
    -- | Target branch, e.g. @"main"@.
    ghBranch :: !Text,
    -- | Posts directory within the repo, e.g. @"content/posts"@.
    ghPostsPath :: !Text
  }

-- | Commit a new Hugo post Markdown file to the GitHub Pages repository.
-- Uses the GitHub Contents API (no local git required).
commitPost ::
  GitHubConfig ->
  -- | Post filename, e.g. @"my-post.md"@.
  Text ->
  -- | Markdown content (with Hugo front-matter).
  Text ->
  IO ()
commitPost _cfg _filename _content = do
  -- TODO: PUT /repos/{owner}/{repo}/contents/{path}
  pure ()

-- | Trigger a GitHub Actions workflow dispatch to rebuild the Hugo site.
triggerDeploy :: GitHubConfig -> IO ()
triggerDeploy _cfg = do
  -- TODO: POST /repos/{owner}/{repo}/actions/workflows/{id}/dispatches
  pure ()
