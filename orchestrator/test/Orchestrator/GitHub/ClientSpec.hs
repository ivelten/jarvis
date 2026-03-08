{-# LANGUAGE ScopedTypeVariables #-}

-- | Pure and IO unit tests for 'Orchestrator.GitHub.Client'.
--
-- IO tests spin up a minimal TCP mock server on a free localhost port so no
-- real network connection is required.  The mock accepts one request per
-- connection, returns a preset HTTP response, and closes.
module Orchestrator.GitHub.ClientSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (SomeException, bracket, try)
import Data.Aeson (FromJSON, Key, Value)
import Data.Aeson.Types (parseMaybe, withObject, (.:))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import Orchestrator.GitHub.Client
import Orchestrator.IOUtils (tryIO)
import Orchestrator.TextUtils (emojiDraft)
import Test.Hspec

spec :: Spec
spec = do
  mgr <- runIO (newManager defaultManagerSettings)
  describe "contentsPath" $ do
    it "builds the correct URL for a given filename" $ do
      let cfg = stubConfig mgr "https://api.github.com"
      contentsPath cfg "my-post.md"
        `shouldBe` "https://api.github.com/repos/testowner/testrepo/contents/content/posts/my-post.md"

    it "respects a custom ghApiBase" $ do
      let cfg = stubConfig mgr "http://localhost:9999"
      contentsPath cfg "foo.md"
        `shouldBe` "http://localhost:9999/repos/testowner/testrepo/contents/content/posts/foo.md"

    it "includes the configured postsPath in the URL" $ do
      let cfg = (stubConfig mgr "https://api.github.com") {ghPostsPath = "blog/articles"}
      contentsPath cfg "bar.md"
        `shouldBe` "https://api.github.com/repos/testowner/testrepo/contents/blog/articles/bar.md"

  describe "ghHeaders" $ do
    it "sets Authorization with Bearer scheme" $ do
      let hdrs = ghHeaders (stubConfig mgr "http://x")
      lookup "Authorization" hdrs `shouldBe` Just "Bearer test-token"

    it "sets Accept to the GitHub API media type" $ do
      let hdrs = ghHeaders (stubConfig mgr "http://x")
      lookup "Accept" hdrs `shouldBe` Just "application/vnd.github+json"

    it "sets X-GitHub-Api-Version" $ do
      let hdrs = ghHeaders (stubConfig mgr "http://x")
      lookup "X-GitHub-Api-Version" hdrs `shouldBe` Just "2022-11-28"

    it "sets a User-Agent" $ do
      let hdrs = ghHeaders (stubConfig mgr "http://x")
      lookup "User-Agent" hdrs `shouldBe` Just "jarvis-orchestrator/1.0"

  describe "buildCommitBody" $ do
    it "base64-encodes the file content" $ do
      let expected = TE.decodeUtf8 (B64.encode (TE.encodeUtf8 "Hello, World!"))
          body = buildCommitBody (req "main" "My Post" "post.md" "Hello, World!" Nothing)
      getField "content" body `shouldBe` Just expected

    it "sets the commit message from the post title" $ do
      let body = buildCommitBody (req "main" "Beyond OOP" "my-post.md" "body" Nothing)
      getField "message" body `shouldBe` Just (emojiDraft <> " Post: Beyond OOP")

    it "sets the branch from the first argument" $ do
      let body = buildCommitBody (req "gh-pages" "Title" "p.md" "body" Nothing)
      getField "branch" body `shouldBe` Just ("gh-pages" :: Text)

    it "omits 'sha' when Nothing is passed (new file create)" $ do
      let body = buildCommitBody (req "main" "Title" "p.md" "body" Nothing)
      (getField "sha" body :: Maybe Text) `shouldBe` Nothing

    it "includes 'sha' when Just is passed (file update)" $ do
      let body = buildCommitBody (req "main" "Title" "p.md" "body" (Just "deadbeef123"))
      getField "sha" body `shouldBe` Just ("deadbeef123" :: Text)

    it "round-trips multibyte Unicode content through base64" $ do
      let content = "Haskell är kul \x1F389"
          body = buildCommitBody (req "main" "Title" "p.md" content Nothing)
          encoded = getField "content" body :: Maybe Text
          decoded = fmap (TE.decodeUtf8 . B64.decodeLenient . TE.encodeUtf8) encoded
      decoded `shouldBe` Just content

  describe "buildDeployBody" $ do
    it "contains the ref key with the provided branch" $ do
      getField "ref" (buildDeployBody "main") `shouldBe` Just ("main" :: Text)

    it "uses the exact branch name given" $ do
      getField "ref" (buildDeployBody "release/v2") `shouldBe` Just ("release/v2" :: Text)

  describe "commitPost (mock HTTP)" $ do
    it "creates a new file when the server returns 404 then 201" $
      withMockServer [sha404, created201] $ \port -> do
        let cfg = stubConfig mgr (localhost port)
        commitPost cfg "New Post" "new-post.md" "some content"

    it "updates an existing file when the server returns 200 (with SHA) then 200" $
      withMockServer [shaFound "abc123", updated200] $ \port -> do
        let cfg = stubConfig mgr (localhost port)
        commitPost cfg "Existing Post" "existing-post.md" "updated content"

    it "throws an IOError that mentions the status code on a non-201 commit response" $ do
      result <- try $ withMockServer [sha404, errUnprocessable] $ \port -> do
        let cfg = stubConfig mgr (localhost port)
        commitPost cfg "Bad Post" "bad.md" "content"
      case result of
        Left (e :: SomeException) -> show e `shouldContain` "422"
        Right _ -> expectationFailure "expected an exception on 422 commit"

  describe "triggerDeploy (mock HTTP)" $ do
    it "succeeds silently on a 204 No Content response" $
      withMockServer [noContent204] $ \port -> do
        let cfg = stubConfig mgr (localhost port)
        triggerDeploy cfg

    it "throws an IOError that mentions the status code on failure" $ do
      result <- try $ withMockServer [errUnprocessable] $ \port -> do
        let cfg = stubConfig mgr (localhost port)
        triggerDeploy cfg
      case result of
        Left (e :: SomeException) -> show e `shouldContain` "422"
        Right _ -> expectationFailure "expected an exception on 422 deploy"

-- ---------------------------------------------------------------------------
-- Stub config
-- ---------------------------------------------------------------------------

-- | Minimal 'GitHubConfig' for pure tests.  Pass a real Manager for IO tests.
stubConfig :: Manager -> Text -> GitHubConfig
stubConfig mgr base =
  GitHubConfig
    { ghToken = "test-token",
      ghRepoOwner = "testowner",
      ghRepoName = "testrepo",
      ghBranch = "main",
      ghPostsPath = "content/posts",
      ghWorkflowId = "deploy.yml",
      ghManager = mgr,
      ghApiBase = base
    }

localhost :: Int -> Text
localhost port = "http://localhost:" <> T.pack (show port)

-- | Shorthand for constructing a 'CommitRequest' in tests.
req :: Text -> Text -> Text -> Text -> Maybe Text -> CommitRequest
req branch title filename content mSha =
  CommitRequest
    { crBranch = branch,
      crTitle = title,
      crFilename = filename,
      crContent = content,
      crSha = mSha
    }

-- ---------------------------------------------------------------------------
-- Canned HTTP responses
-- ---------------------------------------------------------------------------

sha404 :: BC.ByteString
sha404 = mkResponse 404 Nothing ""

created201 :: BC.ByteString
created201 = mkResponse 201 (Just "application/json") "{}"

updated200 :: BC.ByteString
updated200 = mkResponse 200 (Just "application/json") "{}"

noContent204 :: BC.ByteString
noContent204 = mkResponse 204 Nothing ""

errUnprocessable :: BC.ByteString
errUnprocessable = mkResponse 422 (Just "application/json") "{\"message\":\"Validation Failed\"}"

shaFound :: Text -> BC.ByteString
shaFound sha =
  let body = TE.encodeUtf8 ("{\"sha\":\"" <> sha <> "\"}")
   in mkResponse 200 (Just "application/json") body

-- | Build a minimal HTTP/1.1 response byte string.
-- Includes @Connection: close@ so @http-client@ opens a fresh connection for
-- the next request rather than reusing this one.
mkResponse :: Int -> Maybe BC.ByteString -> BC.ByteString -> BC.ByteString
mkResponse sc mContentType body =
  let statusLine = "HTTP/1.1 " <> BC.pack (show sc) <> " " <> reasonPhrase sc
      hdrs =
        [ "Connection: close",
          "Content-Length: " <> BC.pack (show (BS.length body))
        ]
          <> maybe [] (\ct -> ["Content-Type: " <> ct]) mContentType
   in BS.intercalate "\r\n" (statusLine : hdrs) <> "\r\n\r\n" <> body

reasonPhrase :: Int -> BC.ByteString
reasonPhrase 200 = "OK"
reasonPhrase 201 = "Created"
reasonPhrase 204 = "No Content"
reasonPhrase 404 = "Not Found"
reasonPhrase 422 = "Unprocessable Entity"
reasonPhrase _ = "Unknown"

-- ---------------------------------------------------------------------------
-- Mock TCP server
-- ---------------------------------------------------------------------------

-- | Start a minimal HTTP mock server on a free localhost port.
--
-- The server accepts TCP connections in a background thread, returning the
-- next entry from @responses@ to each connection in order (one response per
-- connection).  The listening socket is closed when @action@ returns.
withMockServer :: [BC.ByteString] -> (Int -> IO a) -> IO a
withMockServer responses action =
  bracket openSock NS.close $ \sock -> do
    queue <- newMVar responses
    port <- NS.socketPort sock
    _ <- forkIO (acceptLoop sock queue)
    threadDelay 5_000 -- give the accept thread a moment to reach accept()
    action (fromIntegral port)
  where
    openSock = do
      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      NS.setSocketOption sock NS.ReuseAddr 1
      NS.bind sock (NS.SockAddrInet 0 (NS.tupleToHostAddress (127, 0, 0, 1)))
      NS.listen sock 10
      pure sock

    acceptLoop sock queue = do
      result <- tryIO (NS.accept sock)
      case result of
        Left _ -> pure () -- socket closed; accept loop exits cleanly
        Right (conn, _) -> do
          _ <- forkIO (serveConn conn queue)
          acceptLoop sock queue

    serveConn conn queue = do
      drainRequest conn
      resp <- modifyMVar queue $ \case
        (r : rs) -> pure (rs, r)
        [] -> pure ([], mkResponse 500 Nothing "mock server: no more responses")
      NSB.sendAll conn resp
      NS.close conn

-- | Read and discard a complete HTTP request (headers + body) so the client
-- is never left mid-send when we reply.
drainRequest :: NS.Socket -> IO ()
drainRequest sock = do
  -- Read header bytes until we find the blank-line separator.
  -- On localhost, body bytes often arrive in the same segment.
  buf <- readUntilBlankLine sock ""
  -- Separate headers from any body bytes that arrived early.
  let (hdrPart, afterSep) = BS.breakSubstring "\r\n\r\n" buf
      bodyAlreadyRead = BS.length (BS.drop 4 afterSep)
      bodyLen = parseContentLength hdrPart
  -- Read any remaining body bytes not already buffered.
  _ <- readExact sock (max 0 (bodyLen - bodyAlreadyRead))
  pure ()
  where
    readUntilBlankLine s acc = do
      chunk <- NSB.recv s 4096
      let acc' = acc <> chunk
      if "\r\n\r\n" `BS.isInfixOf` acc'
        then pure acc'
        else
          if BS.null chunk
            then pure acc' -- connection closed early
            else readUntilBlankLine s acc'

    readExact _ 0 = pure ""
    readExact s n = go n ""
      where
        go 0 acc = pure acc
        go remaining acc = do
          chunk <- NSB.recv s (min remaining 4096)
          if BS.null chunk
            then pure acc
            else go (remaining - BS.length chunk) (acc <> chunk)

-- | Case-insensitive search for @Content-Length@ in raw HTTP header bytes.
parseContentLength :: BC.ByteString -> Int
parseContentLength headers =
  let ls = BC.lines headers
      isClHeader l =
        let low = BC.pack (map toLowerASCII (BC.unpack l))
         in BC.isPrefixOf "content-length:" low
      clLines = filter isClHeader ls
   in case clLines of
        (l : _) ->
          let raw = BC.dropWhile (\c -> c == ' ' || c == '\t') (BC.drop (BC.length (BC.takeWhile (/= ':') l) + 1) l)
           in case reads (BC.unpack raw) of
                [(n, _)] -> n
                _ -> 0
        _ -> 0

toLowerASCII :: Char -> Char
toLowerASCII c
  | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
  | otherwise = c

-- ---------------------------------------------------------------------------
-- Aeson helper
-- ---------------------------------------------------------------------------

-- | Extract a field from a 'Value' that must be an Object.
getField :: (FromJSON a) => Key -> Value -> Maybe a
getField k = parseMaybe (withObject "obj" (.: k))
