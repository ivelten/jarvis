INSERT INTO subject (
        name,
        interest_score,
        enabled,
        created_at,
        updated_at
    )
VALUES (
        'Haskell in production / real-world Haskell',
        3,
        TRUE,
        NOW(),
        NOW()
    ),
    (
        'Functional programming patterns for developers coming from C#/.NET',
        3,
        TRUE,
        NOW(),
        NOW()
    ),
    (
        'Type-driven design and type-level programming in Haskell',
        3,
        TRUE,
        NOW(),
        NOW()
    ),
    (
        'Practical Haskell libraries (servant, persistent, optics, async, etc.)',
        3,
        TRUE,
        NOW(),
        NOW()
    ),
    (
        'Paradigm comparisons between OOP/imperative and functional styles',
        3,
        TRUE,
        NOW(),
        NOW()
    ) ON CONFLICT (name) DO NOTHING;
