DO $$ BEGIN
ALTER TABLE subject
ADD CONSTRAINT subject_interest_score_range CHECK (
        interest_score BETWEEN 1 AND 5
    );
EXCEPTION
WHEN duplicate_object THEN NULL;
END $$;
