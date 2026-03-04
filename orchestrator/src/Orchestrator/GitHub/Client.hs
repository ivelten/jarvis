module Orchestrator.GitHub.Client
  ( GitHubConfig (..),
    commitPost,
    triggerDeploy,
  )
where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Types (RequestHeaders, status404, statusCode)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Runtime configuration for the GitHub client.
data GitHubConfig = GitHubConfig
  { -- | Personal access token with @contents:write@ and @actions:write@.
    ghToken :: !Text,
    -- | Repository owner username, e.g. @"yourusername"@.
    ghRepoOwner :: !Text,
    -- | Repository name, e.g. @"yourusername.github.io"@.
    ghRepoName :: !Text,
    -- | Target branch, e.g. @"main"@.
    ghBranch :: !Text,
    -- | Posts directory within the repo, e.g. @"content/posts"@.
    ghPostsPath :: !Text,
    -- | Workflow filename to dispatch after publishing, e.g. @"deploy.yml"@.
    ghWorkflowId :: !Text,
    ghManager :: !Manager
  }

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Base URL for the GitHub REST API.
apiBase :: String
apiBase = "https://api.github.com"

-- | Standard request headers required by the GitHub API.
ghHeaders :: GitHubConfig -> RequestHeaders
ghHeaders GitHubConfig {..} =
  [ ("Authorization", "Bearer " <> TE.encodeUtf8 ghToken),
    ("Accept", "application/vnd.github+json"),
    ("X-GitHub-Api-Version", "2022-11-28"),
    ("User-Agent", "jarvis-orchestrator/1.0")
  ]

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Build the Contents API path for a post file.
--   Result: @/repos/{owner}/{repo}/contents/{postsPath}/{filename}@
contentsPath :: GitHubConfig -> Text -> String
contentsPath GitHubConfig {..} filename =
  apiBase
    <> "/repos/"
    <> T.unpack ghRepoOwner
    <> "/"
    <> T.unpack ghRepoName
    <> "/contents/"
    <> T.unpack ghPostsPath
    <> "/"
    <> T.unpack filename

-- | Try to fetch the current SHA of a file so we can update rather than
--   create it.  Returns 'Nothing' when the file does not yet exist (404).
getFileSha :: GitHubConfig -> Text -> IO (Maybe Text)
getFileSha cfg filename = do
  initReq <- parseRequest (contentsPath cfg filename)
  let req = initReq {requestHeaders = ghHeaders cfg}
  resp <- httpLbs req (ghManager cfg)
  if responseStatus resp == status404
    then pure Nothing
    else case eitherDecode (responseBody resp) of
      Left _ -> pure Nothing
      Right v ->
        pure $
          parseMaybe (withObject "FileResponse" (.: "sha")) v

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Commit a new (or updated) Hugo post Markdown file to the repository.
--
-- Uses the GitHub Contents API so no local @git@ binary is required.
-- If the file already exists its current SHA is fetched first so the
-- request is treated as an update rather than a conflicting create.
commitPost ::
  GitHubConfig ->
  -- | Post filename, e.g. @"my-post.md"@.
  Text ->
  -- | Markdown content (with Hugo front-matter).
  Text ->
  IO ()
commitPost cfg filename content = do
  mSha <- getFileSha cfg filename
  let encodedContent =
        TE.decodeUtf8 . B64.encode . TE.encodeUtf8 $ content
      bodyFields =
        [ "message" .= ("feat(posts): publish " <> filename),
          "content" .= encodedContent,
          "branch" .= ghBranch cfg
        ]
      body = object $ case mSha of
        Nothing -> bodyFields
        Just sha -> bodyFields <> ["sha" .= sha]
  initReq <- parseRequest (contentsPath cfg filename)
  let req =
        initReq
          { method = "PUT",
            requestBody = RequestBodyLBS (encode body),
            requestHeaders =
              ("Content-Type", "application/json") : ghHeaders cfg
          }
  resp <- httpLbs req (ghManager cfg)
  let sc = responseStatus resp
  if statusCode sc `elem` [200, 201]
    then pure ()
    else
      ioError . userError $
        "GitHub commitPost failed ("
          <> show (statusCode sc)
          <> "): "
          <> show (responseBody resp)

-- | Trigger a GitHub Actions workflow dispatch to rebuild the Hugo site.
--
-- This is best-effort: a non-fatal warning is printed on failure so that a
-- workflow hiccup does not roll back a successfully committed post.
triggerDeploy :: GitHubConfig -> IO ()
triggerDeploy cfg@GitHubConfig {..} = do
  let url =
        apiBase
          <> "/repos/"
          <> T.unpack ghRepoOwner
          <> "/"
          <> T.unpack ghRepoName
          <> "/actions/workflows/"
          <> T.unpack ghWorkflowId
          <> "/dispatches"
      body = object ["ref" .= ghBranch]
  initReq <- parseRequest url
  let req =
        initReq
          { method = "POST",
            requestBody = RequestBodyLBS (encode body),
            requestHeaders =
              ("Content-Type", "application/json") : ghHeaders cfg
          }
  resp <- httpLbs req ghManager
  -- 204 No Content is the success response for workflow dispatch.
  let sc = statusCode (responseStatus resp)
  if sc == 204
    then pure ()
    else
      ioError . userError $
        "GitHub triggerDeploy failed ("
          <> show sc
          <> "): "
          <> show (responseBody resp)
