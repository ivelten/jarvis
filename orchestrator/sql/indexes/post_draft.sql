CREATE INDEX IF NOT EXISTS idx_post_draft_status ON post_draft (status);
CREATE INDEX IF NOT EXISTS idx_post_draft_subject_id ON post_draft (subject_id);
-- Partial unique index: discord_thread_id is unique when set; multiple NULLs are allowed.
CREATE UNIQUE INDEX IF NOT EXISTS idx_post_draft_discord_thread_id ON post_draft (discord_thread_id)
WHERE discord_thread_id IS NOT NULL;
