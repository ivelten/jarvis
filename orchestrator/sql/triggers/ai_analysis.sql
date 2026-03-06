CREATE OR REPLACE FUNCTION preserve_analyzed_at() RETURNS TRIGGER AS $$ BEGIN NEW.analyzed_at = OLD.analyzed_at;
RETURN NEW;
END;
$$ LANGUAGE plpgsql;
DO $$ BEGIN CREATE TRIGGER preserve_draft_ai_analysis_analyzed_at BEFORE
UPDATE ON draft_ai_analysis FOR EACH ROW EXECUTE FUNCTION preserve_analyzed_at();
EXCEPTION
WHEN duplicate_object THEN NULL;
END $$;
DO $$ BEGIN CREATE TRIGGER preserve_content_search_ai_analysis_searched_at BEFORE
UPDATE ON content_search_ai_analysis FOR EACH ROW EXECUTE FUNCTION preserve_analyzed_at();
EXCEPTION
WHEN duplicate_object THEN NULL;
END $$;
