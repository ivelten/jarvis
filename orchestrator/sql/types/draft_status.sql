DO $$ BEGIN IF NOT EXISTS (
    SELECT 1
    FROM pg_type
    WHERE typname = 'draft_status'
) THEN CREATE TYPE draft_status AS ENUM ('reviewing', 'approved', 'published', 'rejected');
END IF;
END $$;
-- Add 'rejected' to existing databases that were created before this value existed.
ALTER TYPE draft_status
ADD VALUE IF NOT EXISTS 'rejected';
