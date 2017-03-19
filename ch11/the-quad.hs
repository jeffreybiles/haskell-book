data Quad = One
          | Two
          | Three
          | Four
          deriving (Eq, Show)

-- 1. 8 (2 * 4)
-- 2. 16 (4 * 4)
-- 3. 256 (4 ^ 4)
-- 4. 8 (2 * 2 * 2)
-- 5. 16 (2 ^ (2 * 2))
-- 6. 65536 (4 ^ (2 * 4))
