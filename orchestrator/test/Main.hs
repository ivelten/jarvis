module Main where

import Control.Exception (bracket_)
import Test.Hspec (hspec)
import qualified Spec
import TestHelpers (createTestDatabase, dropTestDatabase)

main :: IO ()
main =
  bracket_
    createTestDatabase
    dropTestDatabase
    (hspec Spec.spec)
