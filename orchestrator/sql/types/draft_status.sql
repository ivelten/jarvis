DO $$ BEGIN IF NOT EXISTS (
    SELECT 1
    FROM pg_type
    WHERE typname = 'draft_status'
) THEN CREATE TYPE draft_status AS ENUM ('reviewing', 'approved', 'published');
END IF;
END $$;
