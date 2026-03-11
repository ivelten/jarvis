-- | Pure unit tests for 'Orchestrator.Database.Models'.
--
-- These tests do not require a database connection.  They verify that
-- every 'PersistField' instance correctly round-trips Haskell values
-- through the 'PersistValue' representation used on the wire.
module Orchestrator.Database.ModelsSpec (spec) where

import Data.ByteString.Char8 (pack)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (PersistField (..), PersistValue (..))
import Orchestrator.Database.Models
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (elements, forAll, listOf)

spec :: Spec
spec = do
  describe "ContentStatus" $ do
    it "round-trips all values via PersistText (happy path)" $
      all
        (\s -> fromPersistValue (toPersistValue s) == Right s)
        ([minBound .. maxBound] :: [ContentStatus])
        `shouldBe` True

    it "round-trips via PersistLiteral (PostgreSQL wire format)" $ do
      (fromPersistValue (PersistLiteral (pack "new")) :: Either Text ContentStatus)
        `shouldBe` Right ContentNew
      (fromPersistValue (PersistLiteral (pack "rejected")) :: Either Text ContentStatus)
        `shouldBe` Right ContentRejected
      (fromPersistValue (PersistLiteral (pack "drafted")) :: Either Text ContentStatus)
        `shouldBe` Right ContentDrafted

    it "rejects an unknown value" $
      (fromPersistValue (PersistText "oops") :: Either Text ContentStatus)
        `shouldBe` Left "Unknown ContentStatus value: oops"

  describe "DraftStatus" $ do
    it "round-trips all values via PersistText (happy path)" $
      all
        (\s -> fromPersistValue (toPersistValue s) == Right s)
        ([minBound .. maxBound] :: [DraftStatus])
        `shouldBe` True

    it "round-trips via PersistLiteral (PostgreSQL wire format)" $ do
      (fromPersistValue (PersistLiteral (pack "reviewing")) :: Either Text DraftStatus)
        `shouldBe` Right DraftReviewing
      (fromPersistValue (PersistLiteral (pack "approved")) :: Either Text DraftStatus)
        `shouldBe` Right DraftApproved
      (fromPersistValue (PersistLiteral (pack "published")) :: Either Text DraftStatus)
        `shouldBe` Right DraftPublished
      (fromPersistValue (PersistLiteral (pack "rejected")) :: Either Text DraftStatus)
        `shouldBe` Right DraftRejected

    it "rejects an unknown value" $
      (fromPersistValue (PersistText "oops") :: Either Text DraftStatus)
        `shouldBe` Left "Unknown DraftStatus value: oops"

  describe "CommentAuthor" $ do
    it "round-trips all values via PersistText (happy path)" $
      all
        (\s -> fromPersistValue (toPersistValue s) == Right s)
        ([minBound .. maxBound] :: [CommentAuthor])
        `shouldBe` True

    it "round-trips via PersistLiteral (PostgreSQL wire format)" $ do
      (fromPersistValue (PersistLiteral (pack "user")) :: Either Text CommentAuthor)
        `shouldBe` Right CommentAuthorUser
      (fromPersistValue (PersistLiteral (pack "jarvis")) :: Either Text CommentAuthor)
        `shouldBe` Right CommentAuthorJarvis

    it "rejects an unknown value" $
      (fromPersistValue (PersistText "oops") :: Either Text CommentAuthor)
        `shouldBe` Left "Unknown CommentAuthor value: oops"

  describe "TagList" $ do
    let rt tl = fromPersistValue (toPersistValue tl)

    it "round-trips an empty list" $
      rt (TagList []) `shouldBe` Right (TagList [])

    it "round-trips simple tags" $
      rt (TagList ["haskell", "fp", "types"])
        `shouldBe` Right (TagList ["haskell", "fp", "types"])

    it "round-trips a tag with spaces" $
      rt (TagList ["lazy evaluation", "type classes"])
        `shouldBe` Right (TagList ["lazy evaluation", "type classes"])

    it "round-trips a tag with a comma" $
      rt (TagList ["a,b"]) `shouldBe` Right (TagList ["a,b"])

    it "round-trips a tag with double-quotes" $
      rt (TagList ["say \"hello\""]) `shouldBe` Right (TagList ["say \"hello\""])

    it "round-trips a single-element list" $
      rt (TagList ["haskell"]) `shouldBe` Right (TagList ["haskell"])

    it "rejects a non-array literal" $
      ( fromPersistValue (PersistLiteral (pack "not an array")) ::
          Either Text TagList
      )
        `shouldBe` Left "Invalid PostgreSQL array format: not an array"

  describe "InterestScore" $ do
    it "accepts all values in range 1-5" $
      map (fmap unInterestScore . mkInterestScore) [1 .. 5]
        `shouldBe` map Right [1 .. 5]

    it "rejects 0" $
      mkInterestScore 0
        `shouldBe` Left "InterestScore must be between 1 and 5, got: 0"

    it "rejects 6" $
      mkInterestScore 6
        `shouldBe` Left "InterestScore must be between 1 and 5, got: 6"

    it "round-trips via PersistField for all valid values" $
      map (\n -> fmap unInterestScore (mkInterestScore n >>= fromPersistValue . toPersistValue)) [1 .. 5]
        `shouldBe` map Right [1 .. 5]

  describe "TagList (property)" $ do
    let genTag = T.pack <$> listOf (elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "0123456789 ,\"\\{}")
    prop "round-trips arbitrary tag lists" $
      forAll (listOf genTag) $ \tags ->
        fromPersistValue (toPersistValue (TagList tags)) == Right (TagList tags)
