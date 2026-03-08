module Orchestrator.Posts.Generator
  ( HugoPostMeta (..),
    buildHugoFrontMatter,
    renderHugoPost,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)

-- | Metadata required to render a Hugo post front-matter block.
data HugoPostMeta = HugoPostMeta
  { -- | Post title, written verbatim into the front-matter @title@ field.
    hpmTitle :: !Text,
    -- | URL slug, e.g. @"my-post-title"@.
    hpmSlug :: !Text,
    -- | Publication date; becomes the front-matter @date@ field.
    hpmDate :: !UTCTime,
    -- | List of tags written into the front-matter @tags@ array.
    hpmTags :: ![Text]
  }

-- | Build the TOML front-matter block for a Hugo post.
buildHugoFrontMatter :: HugoPostMeta -> Text
buildHugoFrontMatter HugoPostMeta {..} =
  T.unlines
    [ "+++",
      "title = " <> quote hpmTitle,
      "date = " <> T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" hpmDate),
      "draft = false",
      "slug = " <> quote hpmSlug,
      "tags = [" <> T.intercalate ", " (map quote hpmTags) <> "]",
      "+++"
    ]
  where
    -- Escape backslashes first, then double-quotes, to produce valid TOML strings.
    quote t = "\"" <> T.replace "\\" "\\\\" (T.replace "\"" "\\\"" t) <> "\""

-- | Combine front-matter and body into a full Hugo Markdown file.
renderHugoPost :: HugoPostMeta -> Text -> Text
renderHugoPost meta body =
  buildHugoFrontMatter meta <> "\n" <> body
