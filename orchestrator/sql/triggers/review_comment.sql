CREATE OR REPLACE FUNCTION preserve_created_at() RETURNS TRIGGER AS $$ BEGIN NEW.created_at = OLD.created_at;
RETURN NEW;
END;
$$ LANGUAGE plpgsql;
DO $$ BEGIN CREATE TRIGGER preserve_review_comment_created_at BEFORE
UPDATE ON review_comment FOR EACH ROW EXECUTE FUNCTION preserve_created_at();
EXCEPTION
WHEN duplicate_object THEN NULL;
END $$;
