DO $$ BEGIN IF NOT EXISTS (
    SELECT 1
    FROM pg_type
    WHERE typname = 'draft_status'
) THEN CREATE TYPE draft_status AS ENUM (
    'reviewing',
    'approved',
    'published',
    'rejected',
    'publish_failed'
);
END IF;
END $$;
-- Add values to existing databases that were created before they existed.
ALTER TYPE draft_status
ADD VALUE IF NOT EXISTS 'rejected';
ALTER TYPE draft_status
ADD VALUE IF NOT EXISTS 'publish_failed';
