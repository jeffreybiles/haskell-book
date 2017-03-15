-- exercise 5
-- a
foldr (++) [] ["woot", "WOOT", "woot"]
-- b
foldr max ' ' "fear is the little death"
-- c
foldr (&&) True [False, True]
-- d
foldr (||) False [False, True]
-- e
foldr ((++) . show) "" [1..5]
-- or, to get "54321"
foldl (flip ((++) . show)) "" [1..5]
-- f, just make the types match and you're good
foldr const 0 [1..5]
-- g, same as above
foldr const 'a' "tacos"
-- h
foldl (flip const) '' "burritos"
-- i
foldl (flip const) 0 [1..5]
