module Orchestrator.Posts.GeneratorSpec (spec) where

import Data.Text (unpack)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Orchestrator.Posts.Generator
import Test.Hspec

spec :: Spec
spec = do
  describe "buildHugoFrontMatter" $ do
    it "includes the post title" $ do
      let fm = buildHugoFrontMatter "My Title" "my-title" sampleDate []
      unpack fm `shouldContain` "My Title"

    it "includes the slug" $ do
      let fm = buildHugoFrontMatter "My Title" "my-title" sampleDate []
      unpack fm `shouldContain` "my-title"

    it "sets draft = false" $ do
      let fm = buildHugoFrontMatter "T" "t" sampleDate []
      unpack fm `shouldContain` "draft = false"

  describe "renderHugoPost" $ do
    it "combines front-matter and body" $ do
      let rendered = renderHugoPost "T" "t" sampleDate [] "Hello, World!"
      unpack rendered `shouldContain` "Hello, World!"
      unpack rendered `shouldContain` "+++"

sampleDate :: UTCTime
sampleDate = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
