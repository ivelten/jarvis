{-# LANGUAGE TemplateHaskell #-}

module Orchestrator.Database.TH
  ( executeSqlFile,
  )
where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Text (pack)
import Database.Persist.Sql (rawExecute)
import Language.Haskell.TH (Exp, Q)

-- | Embed a SQL file at compile time and produce an expression that executes
-- it as a raw statement with no parameters.
--
-- The file path must be relative to the @.cabal@ file. Because this is a
-- Template Haskell helper, call it with a splice: @$(executeSqlFile "…")@.
executeSqlFile :: FilePath -> Q Exp
executeSqlFile path =
  [|rawExecute (pack $(makeRelativeToProject path >>= embedStringFile)) []|]
