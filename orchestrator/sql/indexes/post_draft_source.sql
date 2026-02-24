CREATE INDEX IF NOT EXISTS idx_post_draft_source_draft_id ON post_draft_source (post_draft_id);
CREATE INDEX IF NOT EXISTS idx_post_draft_source_content_id ON post_draft_source (raw_content_id);
