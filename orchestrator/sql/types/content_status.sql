DO $$ BEGIN IF NOT EXISTS (
    SELECT 1
    FROM pg_type
    WHERE typname = 'content_status'
) THEN CREATE TYPE content_status AS ENUM ('new', 'rejected', 'drafted');
END IF;
END $$;
