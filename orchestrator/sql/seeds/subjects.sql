INSERT INTO subject (
        name,
        description,
        interest_score,
        created_at,
        updated_at
    )
VALUES (
        'Haskell in production / real-world Haskell',
        NULL,
        3,
        NOW(),
        NOW()
    ),
    (
        'Functional programming patterns for developers coming from C#/.NET',
        NULL,
        3,
        NOW(),
        NOW()
    ),
    (
        'Type-driven design and type-level programming in Haskell',
        NULL,
        3,
        NOW(),
        NOW()
    ),
    (
        'Practical Haskell libraries (servant, persistent, optics, async, etc.)',
        NULL,
        3,
        NOW(),
        NOW()
    ),
    (
        'Paradigm comparisons between OOP/imperative and functional styles',
        NULL,
        3,
        NOW(),
        NOW()
    ) ON CONFLICT (name) DO NOTHING;
