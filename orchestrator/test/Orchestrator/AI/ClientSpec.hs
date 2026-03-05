-- | Pure unit tests for 'Orchestrator.AI.Client'.
--
-- No network or database connections are needed; all functions under test
-- are pure or only inspect in-memory data structures.
module Orchestrator.AI.ClientSpec (spec) where

import Data.Aeson (Value, decode, encode, object, (.=))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Orchestrator.AI.Client
import Orchestrator.TextUtils (splitTitle, toSlug)
import Test.Hspec

spec :: Spec
spec = do
  describe "extractText" $ do
    it "extracts text from a well-formed Gemini response" $
      extractText (geminiResponse "hello world")
        `shouldBe` Just "hello world"

    it "returns Nothing for an empty candidates array" $
      extractText (object ["candidates" .= ([] :: [Value])])
        `shouldBe` Nothing

    it "returns Nothing when the parts array is empty" $
      extractText
        ( object
            [ "candidates"
                .= [ object
                       [ "content"
                           .= object ["parts" .= ([] :: [Value])]
                       ]
                   ]
            ]
        )
        `shouldBe` Nothing

    it "returns Nothing when the response has no 'candidates' key" $
      extractText (object ["other" .= ("x" :: Text)])
        `shouldBe` Nothing

  describe "splitTitle" $ do
    it "strips the leading # and whitespace from an H1 heading" $ do
      let (title, _) = splitTitle "# My Post Title\nBody here."
      title `shouldBe` "My Post Title"

    it "returns the remaining lines as the body" $ do
      let (_, body) = splitTitle "# Title\nLine 1\nLine 2\n"
      T.strip body `shouldBe` "Line 1\nLine 2"

    it "handles a document with only a title line" $ do
      let (title, _) = splitTitle "# Solo Title"
      title `shouldBe` "Solo Title"

    it "returns Untitled for an empty input" $ do
      let (title, _) = splitTitle ""
      title `shouldBe` "Untitled"

  describe "toSlug" $ do
    it "lowercases words and joins them with hyphens" $
      toSlug "Hello World" `shouldBe` "hello-world"

    it "replaces non-alphanumeric characters with hyphens" $
      toSlug "Hello, World!" `shouldBe` "hello-world"

    it "collapses multiple spaces / separators" $
      toSlug "foo  bar" `shouldBe` "foo-bar"

    it "handles an all-punctuation input" $
      toSlug "!!!" `shouldBe` ""

    it "preserves digits" $
      toSlug "GHC 9.6" `shouldBe` "ghc-9-6"

  describe "DiscoveredContent JSON" $ do
    it "round-trips via encode/decode" $ do
      let dc = DiscoveredContent "Title" "https://example.com" "A summary" (Just "Haskell")
      decode (encode dc) `shouldBe` Just dc

    it "decodes without the optional 'subject' field" $ do
      let json = "{\"title\":\"T\",\"url\":\"https://x\",\"summary\":\"S\"}"
      (decode json :: Maybe DiscoveredContent)
        `shouldBe` Just (DiscoveredContent "T" "https://x" "S" Nothing)

    it "fails to decode when required fields are missing" $ do
      let json = "{\"title\":\"T\"}"
      (decode json :: Maybe DiscoveredContent) `shouldBe` Nothing

  describe "parseTagsLine" $ do
    it "parses a TAGS line and returns trimmed tags" $ do
      let input = "TAGS: haskell, functional-programming, dotnet\n# Title\nBody."
          (tags, body) = parseTagsLine input
      tags `shouldBe` ["haskell", "functional-programming", "dotnet"]
      T.isPrefixOf "# Title" body `shouldBe` True

    it "strips leading/trailing whitespace from each tag" $ do
      let (tags, _) = parseTagsLine "TAGS:  foo ,  bar baz  \n# T\n"
      tags `shouldBe` ["foo", "bar baz"]

    it "returns empty tags and the full text when no TAGS line is present" $ do
      let input = "# Title\nBody."
          (tags, body) = parseTagsLine input
      tags `shouldBe` []
      body `shouldBe` input

    it "returns empty tags for empty input" $ do
      let (tags, body) = parseTagsLine ""
      tags `shouldBe` []
      body `shouldBe` ""

    it "ignores a TAGS line that is not the first line" $ do
      let input = "# Title\nTAGS: haskell\nBody."
          (tags, _) = parseTagsLine input
      tags `shouldBe` []

    it "excludes empty segments from comma-split" $ do
      let (tags, _) = parseTagsLine "TAGS: haskell,,dotnet\n# T\n"
      tags `shouldBe` ["haskell", "dotnet"]

  describe "extractGroundingChunks" $ do
    it "returns an empty list when there are no candidates" $
      extractGroundingChunks (object ["candidates" .= ([] :: [Value])])
        `shouldBe` []

    it "returns an empty list when groundingMetadata is absent" $
      extractGroundingChunks (geminiResponse "hello")
        `shouldBe` []

    it "extracts (title, uri) pairs from grounding chunks" $
      extractGroundingChunks
        ( groundingResponse
            "ignored text"
            [ ("Functional Programming Basics Every C# Developer Can Use Today", "https://example.com/fp-basics"),
              ("Haskell STM Guide", "https://example.com/stm")
            ]
        )
        `shouldBe` [ ("Functional Programming Basics Every C# Developer Can Use Today", "https://example.com/fp-basics"),
                     ("Haskell STM Guide", "https://example.com/stm")
                   ]

    it "falls back to the URI as title when the title field is absent" $
      extractGroundingChunks
        ( object
            [ "candidates"
                .= [ object
                       [ "content" .= object ["parts" .= [object ["text" .= ("t" :: Text)]]],
                         "groundingMetadata"
                           .= object
                             [ "groundingChunks"
                                 .= [object ["web" .= object ["uri" .= ("https://no-title.example.com" :: Text)]]]
                             ]
                       ]
                   ]
            ]
        )
        `shouldBe` [("https://no-title.example.com", "https://no-title.example.com")]

  describe "resolveRedirectUrl" $ do
    let realUrl = "https://example.com/fp-basics-for-csharp"
        chunkMap = Map.fromList [("Functional Programming Basics Every C# Developer Can Use Today", realUrl)]
        itemWith url = DiscoveredContent "Functional Programming Basics Every C# Developer Can Use Today" url "A summary" Nothing

    it "replaces an ephemeral redirect URL with the matching real URL" $
      dcUrl (resolveRedirectUrl chunkMap (itemWith redirectUrl))
        `shouldBe` realUrl

    it "leaves a non-redirect URL untouched" $
      dcUrl (resolveRedirectUrl chunkMap (itemWith "https://already-real.example.com"))
        `shouldBe` "https://already-real.example.com"

    it "keeps the redirect URL when no title matches are found" $ do
      let emptyMap = Map.empty
      dcUrl (resolveRedirectUrl emptyMap (itemWith redirectUrl))
        `shouldBe` redirectUrl

    it "matches when the chunk title is a substring of the item title" $ do
      let item = DiscoveredContent "Functional Programming Basics Every C# Developer Can Use Today \8212 Deep Dive" redirectUrl "S" Nothing
      dcUrl (resolveRedirectUrl chunkMap item) `shouldBe` realUrl

    it "matches when the item title is a substring of the chunk title" $ do
      let item = DiscoveredContent "Functional Programming Basics" redirectUrl "S" Nothing
          wideMap = Map.fromList [("Functional Programming Basics Every C# Developer Can Use Today", realUrl)]
      dcUrl (resolveRedirectUrl wideMap item) `shouldBe` realUrl

  describe "followRedirect" $ do
    it "returns a non-redirect URL unchanged (no HTTP call made)" $ do
      mgr <- newManager defaultManagerSettings
      result <- followRedirect mgr "https://already-real.example.com"
      result `shouldBe` "https://already-real.example.com"

  describe "mergeWithChunks" $ do
    let chunk1 = ("Functional Programming in Haskell Guide", "https://example.com/fp-haskell")
        item1 = ModelItem "Functional Programming in Haskell" "Great intro" (Just "Haskell Basics")

    it "pairs a model item with a matching chunk and uses the chunk's real URL" $
      mergeWithChunks [chunk1] [item1]
        `shouldBe` [DiscoveredContent "Functional Programming in Haskell" "https://example.com/fp-haskell" "Great intro" (Just "Haskell Basics")]

    it "drops model items that have no matching grounding chunk" $
      mergeWithChunks
        [("Completely Unrelated", "https://example.com/other")]
        [item1]
        `shouldBe` []

    it "returns empty when chunks list is empty" $
      mergeWithChunks [] [item1] `shouldBe` []

    it "returns empty when model items list is empty" $
      mergeWithChunks [chunk1] [] `shouldBe` []

    it "matches on a keyword from the chunk title appearing in the model title" $ do
      -- 'effectful' (10 chars) appears in the model title but not the short chunk title
      let chunk = ("effectful Haskell backend", "https://github.com/user/effectful-example")
          item = ModelItem "Haskell RealWorld example with effectful" "A production example" Nothing
      mergeWithChunks [chunk] [item]
        `shouldBe` [DiscoveredContent "Haskell RealWorld example with effectful" "https://github.com/user/effectful-example" "A production example" Nothing]

    it "matches on a keyword from the model title appearing in the chunk title" $ do
      -- 'servant' (7 chars) appears in the chunk title
      let chunk = ("Servant Web Framework - Haskell Documentation", "https://docs.servant.dev")
          item = ModelItem "Building REST APIs with Servant" "How to build APIs" (Just "Tooling")
      mergeWithChunks [chunk] [item]
        `shouldBe` [DiscoveredContent "Building REST APIs with Servant" "https://docs.servant.dev" "How to build APIs" (Just "Tooling")]

    it "ignores short words (< 5 chars) when matching" $
      -- 'STM' is 3 chars, 'GHC' is 3 chars — no significant keyword overlap
      mergeWithChunks
        [("STM GHC API", "https://example.com/stm")]
        [ModelItem "STM API in GHC" "About STM" Nothing]
        `shouldBe` []

    it "handles multiple items and chunks, pairing each item to its best chunk" $ do
      let chunks =
            [ ("Haskell Persistent database library", "https://example.com/persistent"),
              ("servant web framework Haskell", "https://example.com/servant")
            ]
          items =
            [ ModelItem "Database access with Persistent" "Using persistent" (Just "Tooling"),
              ModelItem "Web APIs with Servant" "Using servant" (Just "Tooling")
            ]
          result = mergeWithChunks chunks items
      map dcUrl result
        `shouldBe` ["https://example.com/persistent", "https://example.com/servant"]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Build a minimal Gemini API response wrapping the given text.
geminiResponse :: Text -> Value
geminiResponse txt =
  object
    [ "candidates"
        .= [ object
               [ "content"
                   .= object
                     ["parts" .= [object ["text" .= txt]]]
               ]
           ]
    ]

-- | Build a Gemini response with grounding metadata attached to the candidate.
groundingResponse :: Text -> [(Text, Text)] -> Value
groundingResponse txt chunks =
  object
    [ "candidates"
        .= [ object
               [ "content"
                   .= object ["parts" .= [object ["text" .= txt]]],
                 "groundingMetadata"
                   .= object
                     [ "groundingChunks"
                         .= map
                           ( \(title, uri) ->
                               object ["web" .= object ["uri" .= uri, "title" .= title]]
                           )
                           chunks
                     ]
               ]
           ]
    ]

-- | The ephemeral redirect URL from a real Gemini discovery run.
redirectUrl :: Text
redirectUrl = "https://vertexaisearch.cloud.google.com/grounding-api-redirect/AUZIYQHa2AxPWL6rRKTE3kkfTmoO-tnTfO6AuYK1SrHhWIE4dM63Z1B0MLOCpZ1eW6S0X0DstjFJPizqgKp24eHlwffWJJ9AkRYk4r2u6ub9HI62dfon2mot98XqZ49kW4FyzSJ3bQZ7qHy47lO7-NL1NZx8BTLIMwoJBPuGOjX0aUnN49hVWlfc9WfNC5st4kNv4j3dpTftMge5eipOUEJ0PeNpkziHack="
