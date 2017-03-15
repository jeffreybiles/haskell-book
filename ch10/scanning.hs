fibs = 1 : scanl (+) 1 fibs
fibs20 = take 20 fibs
-- how to do this, aside from artificially limiting the list (using fibs20 instead of fibs)?
smallFibs = takeWhile (<= 100) fibs

factorial x = take x $ scanl (*) 1 [2..]
