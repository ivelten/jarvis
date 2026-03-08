module Orchestrator.Posts.GeneratorSpec (spec) where

import Data.Text (unpack)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Orchestrator.Posts.Generator
import Test.Hspec

spec :: Spec
spec = do
  describe "buildHugoFrontMatter" $ do
    it "includes the post title" $ do
      let fm = buildHugoFrontMatter sampleMeta
      unpack fm `shouldContain` "My Title"

    it "includes the slug" $ do
      let fm = buildHugoFrontMatter sampleMeta
      unpack fm `shouldContain` "my-title"

    it "sets draft = false" $ do
      unpack (buildHugoFrontMatter sampleMeta) `shouldContain` "draft = false"

    it "includes tags when provided" $ do
      let fm = buildHugoFrontMatter sampleMeta {hpmTags = ["haskell", "fp"]}
      unpack fm `shouldContain` "\"haskell\""
      unpack fm `shouldContain` "\"fp\""

    it "escapes backslashes in the title" $ do
      let fm = buildHugoFrontMatter sampleMeta {hpmTitle = "foo\\bar", hpmSlug = "foo-bar"}
      -- A single backslash in input must become \\ in the TOML string
      unpack fm `shouldContain` "foo\\\\bar"

    it "wraps the block in +++ delimiters" $ do
      let fm = buildHugoFrontMatter sampleMeta
      take 3 (unpack fm) `shouldBe` "+++"
      unpack fm `shouldContain` "\n+++"

  describe "renderHugoPost" $ do
    it "combines front-matter and body" $ do
      let rendered = renderHugoPost sampleMeta "Hello, World!"
      unpack rendered `shouldContain` "Hello, World!"
      unpack rendered `shouldContain` "+++"

    it "does not include a duplicate H1 heading when body has none" $ do
      let body = "First paragraph.\n\nSecond paragraph."
          rendered = renderHugoPost sampleMeta {hpmTitle = "My Title", hpmSlug = "my-title"} body
      -- Title comes from front-matter; the raw body should appear as-is
      T.count "# My Title" rendered `shouldBe` 0

sampleDate :: UTCTime
sampleDate = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)

sampleMeta :: HugoPostMeta
sampleMeta =
  HugoPostMeta
    { hpmTitle = "My Title",
      hpmSlug = "my-title",
      hpmDate = sampleDate,
      hpmTags = []
    }
