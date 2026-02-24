DO $$ BEGIN IF NOT EXISTS (
    SELECT 1
    FROM pg_type
    WHERE typname = 'comment_author'
) THEN CREATE TYPE comment_author AS ENUM ('user', 'jarvis');
END IF;
END $$;
