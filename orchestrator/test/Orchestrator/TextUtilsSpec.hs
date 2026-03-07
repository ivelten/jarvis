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
