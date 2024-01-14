fibs = 1 : scanl (+) 1 fibs

fibs20 = take 20 fibs

fibs100 = [x | x <- fibs, x < 100]


factorial :: [Int]
factorial = scanl (*) 1 [1..]

factorialN n = factorial !! n
