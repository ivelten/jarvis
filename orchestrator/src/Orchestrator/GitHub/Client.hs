module Orchestrator.GitHub.Client
  ( GitHubConfig (..),
    CommitRequest (..),
    defaultApiBase,
    ghHeaders,
    contentsPath,
    buildCommitBody,
    buildDeployBody,
    commitPost,
    triggerDeploy,
  )
where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
    ghManager :: !Manager,
    -- | API base URL.  Override in tests to point at a local mock server;
    --   production code should use 'defaultApiBase'.
    ghApiBase :: !Text
  }

-- | Parameters for a single file commit via the GitHub Contents API.
data CommitRequest = CommitRequest
  { -- | Branch to commit to.
    crBranch :: !Text,
    -- | Post title, used in the commit message.
    crTitle :: !Text,
    -- | Filename (used in the URL path).
    crFilename :: !Text,
    -- | Markdown content; will be base64-encoded before sending.
    crContent :: !Text,
    -- | Existing file SHA; 'Nothing' creates a new file, 'Just' updates it.
    crSha :: !(Maybe Text)
  }

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Default base URL for the GitHub REST API.
-- Override 'ghApiBase' in 'GitHubConfig' when pointing at a mock server.
defaultApiBase :: Text
defaultApiBase = "https://api.github.com"

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
--   Result: @{apiBase}/repos/{owner}/{repo}/contents/{postsPath}/{filename}@
contentsPath :: GitHubConfig -> Text -> String
contentsPath GitHubConfig {..} filename =
  T.unpack ghApiBase
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

-- | Build the JSON request body for a Contents API PUT (create or update).
--
-- Exported for testing; most callers should use 'commitPost' directly.
buildCommitBody :: CommitRequest -> Value
buildCommitBody CommitRequest {..} =
  let encodedContent = TE.decodeUtf8 . B64.encode . TE.encodeUtf8 $ crContent
      baseFields =
        [ "message" .= ("\x1F4DD Post: " <> crTitle),
          "content" .= encodedContent,
          "branch" .= crBranch
        ]
   in object $ case crSha of
        Nothing -> baseFields
        Just sha -> baseFields <> ["sha" .= sha]

-- | Build the JSON request body for a workflow dispatch event.
--
-- Exported for testing; most callers should use 'triggerDeploy' directly.
buildDeployBody :: Text -> Value
buildDeployBody branch = object ["ref" .= branch]

-- | Commit a new (or updated) Hugo post Markdown file to the repository.
--
-- Uses the GitHub Contents API so no local @git@ binary is required.
-- If the file already exists its current SHA is fetched first so the
-- request is treated as an update rather than a conflicting create.
commitPost ::
  GitHubConfig ->
  -- | Post title, used verbatim in the commit message.
  Text ->
  -- | Post filename, e.g. @"my-post.md"@.
  Text ->
  -- | Markdown content (with Hugo front-matter).
  Text ->
  IO ()
commitPost cfg title filename content = do
  mSha <- getFileSha cfg filename
  let body =
        buildCommitBody
          CommitRequest
            { crBranch = ghBranch cfg,
              crTitle = title,
              crFilename = filename,
              crContent = content,
              crSha = mSha
            }
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
-- Raises an 'IOError' on failure. Callers are encouraged to catch this and
-- treat it as a warning when the post has already been successfully committed.
triggerDeploy :: GitHubConfig -> IO ()
triggerDeploy cfg@GitHubConfig {..} = do
  let url =
        T.unpack ghApiBase
          <> "/repos/"
          <> T.unpack ghRepoOwner
          <> "/"
          <> T.unpack ghRepoName
          <> "/actions/workflows/"
          <> T.unpack ghWorkflowId
          <> "/dispatches"
      body = buildDeployBody ghBranch
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
