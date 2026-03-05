-- | Pure unit tests for 'Orchestrator.AI.Client'.
--
-- No network or database connections are needed; all functions under test
-- are pure or only inspect in-memory data structures.
module Orchestrator.AI.ClientSpec (spec) where

import Data.Aeson (Value, decode, encode, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Orchestrator.AI.Client
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
