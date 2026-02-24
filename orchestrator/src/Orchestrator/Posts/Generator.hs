module Orchestrator.Posts.Generator
  ( buildHugoFrontMatter,
    renderHugoPost,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)

-- | Build the TOML front-matter block for a Hugo post.
buildHugoFrontMatter :: Text -> Text -> UTCTime -> [Text] -> Text
buildHugoFrontMatter title slug date tags =
  T.unlines
    [ "+++",
      "title = " <> quote title,
      "date = " <> T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" date),
      "draft = false",
      "slug = " <> quote slug,
      "tags = [" <> T.intercalate ", " (map quote tags) <> "]",
      "+++"
    ]
  where
    quote t = "\"" <> t <> "\""

-- | Combine front-matter and body into a full Hugo Markdown file.
renderHugoPost :: Text -> Text -> UTCTime -> [Text] -> Text -> Text
renderHugoPost title slug date tags body =
  buildHugoFrontMatter title slug date tags <> "\n" <> body
