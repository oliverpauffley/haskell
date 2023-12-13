multiplesOfThree = filter (\x -> rem x 3 == 0)

multiplesThreeThirty = multiplesOfThree [1..30]

lengthThirty = length . multiplesOfThree


isArticle :: String -> Bool
isArticle "the" = True
isArticle "a"   = True
isArticle "an"  = True
isArticle _     = False

stripArticles :: String -> [String]
stripArticles l = filter (not . isArticle) (words l)
