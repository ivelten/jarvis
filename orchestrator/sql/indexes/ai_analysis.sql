CREATE INDEX IF NOT EXISTS idx_draft_ai_analysis_draft_id ON draft_ai_analysis (post_draft_id);
CREATE INDEX IF NOT EXISTS idx_content_search_ai_analysis_searched_at ON content_search_ai_analysis (searched_at);
