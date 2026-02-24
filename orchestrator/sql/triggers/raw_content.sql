CREATE OR REPLACE FUNCTION set_updated_at() RETURNS TRIGGER AS $$ BEGIN NEW.updated_at = NOW();
RETURN NEW;
END;
$$ LANGUAGE plpgsql;
CREATE OR REPLACE FUNCTION preserve_created_at() RETURNS TRIGGER AS $$ BEGIN NEW.created_at = OLD.created_at;
RETURN NEW;
END;
$$ LANGUAGE plpgsql;
DO $$ BEGIN CREATE TRIGGER set_raw_content_updated_at BEFORE
UPDATE ON raw_content FOR EACH ROW EXECUTE FUNCTION set_updated_at();
EXCEPTION
WHEN duplicate_object THEN NULL;
END $$;
DO $$ BEGIN CREATE TRIGGER preserve_raw_content_created_at BEFORE
UPDATE ON raw_content FOR EACH ROW EXECUTE FUNCTION preserve_created_at();
EXCEPTION
WHEN duplicate_object THEN NULL;
END $$;
