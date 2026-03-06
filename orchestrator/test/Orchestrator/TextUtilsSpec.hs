-- | Pure unit tests for 'Orchestrator.TextUtils'.
module Orchestrator.TextUtilsSpec (spec) where

import qualified Data.Text as T
import Orchestrator.TextUtils
import Test.Hspec

spec :: Spec
spec = do
  -- -------------------------------------------------------------------------
  describe "truncateText" $ do
    it "returns the text unchanged when it fits within the limit" $
      truncateText 100 "hello" `shouldBe` "hello"

    it "appends an ellipsis when the text exceeds the limit" $ do
      let result = truncateText 10 "hello world foo bar"
      T.last result `shouldBe` '\x2026'

    it "does not exceed the character limit (excluding the ellipsis)" $ do
      let result = truncateText 10 "hello world foo bar"
      T.length result `shouldSatisfy` (<= 11) -- 10 chars + ellipsis
    it "breaks at a word boundary, not mid-word" $ do
      let result = truncateText 8 "hello world"
      -- "hello " is the last run that fits; trailing space + punctuation stripped → "hello…"
      result `shouldBe` "hello\x2026"

    it "strips trailing punctuation before the ellipsis" $ do
      let result = truncateText 7 "foo bar, baz"
      T.isInfixOf "," result `shouldBe` False

    it "handles text that is exactly the limit" $
      truncateText 5 "hello" `shouldBe` "hello"

    it "handles empty input" $
      truncateText 10 "" `shouldBe` ""

  -- -------------------------------------------------------------------------
  describe "chunkText" $ do
    it "returns a single element when text fits within the limit" $
      chunkText 1900 "short text" `shouldBe` ["short text"]

    it "returns an empty list for empty input" $
      chunkText 1900 "" `shouldBe` []

    it "splits into multiple chunks when text exceeds the limit" $ do
      let long = T.replicate 50 "word "
      length (chunkText 20 long) `shouldSatisfy` (> 1)

    it "no individual chunk exceeds the limit" $ do
      let long = T.replicate 200 "word "
          chunks = chunkText 50 long
      all (\c -> T.length c <= 50) chunks `shouldBe` True

    it "reassembling chunks preserves all content" $ do
      let body = T.unlines $ map (\i -> "Line " <> T.pack (show i)) [1 .. 100 :: Int]
          chunks = chunkText 100 body
      -- Every line must appear somewhere across the chunks
      all (\i -> any (T.isInfixOf ("Line " <> T.pack (show i))) chunks) [1 .. 100 :: Int]
        `shouldBe` True

    it "prefers paragraph breaks over line breaks" $ do
      -- Two paragraphs; split limit chosen so the first paragraph fits but not both
      let para1 = T.replicate 40 "a"
          para2 = T.replicate 40 "b"
          body = para1 <> "\n\n" <> para2
          -- limit just wide enough for para1 + "\n\n" but not para2
          chunks = chunkText 44 body
      head chunks `shouldBe` para1

    it "closes an open code fence when splitting inside one" $ do
      let fenced = "```haskell\n" <> T.replicate 200 "code line\n" <> "```\n"
          chunks = chunkText 100 fenced
          -- Every chunk except the last must end with ``` if it opened a fence
          closedProperly = all (\c -> not ("```haskell" `T.isInfixOf` c) || "```" `T.isSuffixOf` T.strip c) (init chunks)
      closedProperly `shouldBe` True

    it "does not leave an orphaned [...] token at the start of a chunk" $ do
      -- Simulate a bullet list item whose inline annotation wraps across a boundary
      let prefix = T.replicate 1850 "x"
          suffix = " some text\n* `ReqBody`\n[JSON] User : rest of line\n"
          body = prefix <> suffix
          chunks = chunkText 1900 body
      -- No chunk (beyond the first) should start with '[' followed by non-link content
      let startsWithOrphan c =
            "[" `T.isPrefixOf` T.strip c
              && not ("](" `T.isInfixOf` T.take 50 (T.strip c))
      any startsWithOrphan (drop 1 chunks) `shouldBe` False

    it "does not split a Markdown link across two chunks" $ do
      let prefix = T.replicate 1880 "x"
          body = prefix <> " [Some Link](https://example.com) end"
          chunks = chunkText 1900 body
      -- The link must not span two chunks (opening bracket in one, URL in next)
      let linkIntact = any (T.isInfixOf "[Some Link](https://example.com)") chunks
      linkIntact `shouldBe` True

  -- -------------------------------------------------------------------------
  describe "splitTitle" $ do
    it "strips the H1 from the body" $ do
      let (_, body) = splitTitle "# My Title\nFirst paragraph."
      T.isInfixOf "# My Title" body `shouldBe` False

    it "the returned body starts with the first non-heading line" $ do
      let (_, body) = splitTitle "# My Title\nFirst paragraph."
      T.strip body `shouldBe` "First paragraph."

    it "handles a document without an H1 (uses first line as title)" $ do
      let (title, body) = splitTitle "Just a plain line\nSecond line."
      title `shouldBe` "Just a plain line"
      T.strip body `shouldBe` "Second line."
