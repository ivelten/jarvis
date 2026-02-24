CREATE OR REPLACE FUNCTION set_updated_at() RETURNS TRIGGER AS $$ BEGIN NEW.updated_at = NOW();
RETURN NEW;
END;
$$ LANGUAGE plpgsql;
DO $$ BEGIN CREATE TRIGGER set_subject_updated_at BEFORE
UPDATE ON subject FOR EACH ROW EXECUTE FUNCTION set_updated_at();
EXCEPTION
WHEN duplicate_object THEN NULL;
END $$;
DO $$ BEGIN CREATE TRIGGER set_raw_content_updated_at BEFORE
UPDATE ON raw_content FOR EACH ROW EXECUTE FUNCTION set_updated_at();
EXCEPTION
WHEN duplicate_object THEN NULL;
END $$;
DO $$ BEGIN CREATE TRIGGER set_post_draft_updated_at BEFORE
UPDATE ON post_draft FOR EACH ROW EXECUTE FUNCTION set_updated_at();
EXCEPTION
WHEN duplicate_object THEN NULL;
END $$;
