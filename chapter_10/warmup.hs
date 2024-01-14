
stops = "pbtdkg"
vowels = "aeiou"

tuples = [(a, b, c) | a <- stops, b <- vowels, c <- stops]
tuples' =  [(a, b, c) | a <- stops, a == 'p', b <- vowels, c <- stops]

nouns = ["hill", "car", "trombone"]
verbs = ["kick", "jump", "work"]


tuples'' =  [(a, b, c) | a <- nouns, b <- verbs, c <- nouns]

seekritFunc x = (/) (fromIntegral (sum (map length (words x)))) (fromIntegral (length (words x)))
