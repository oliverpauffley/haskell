import           Data.Maybe (listToMaybe)
isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing  = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a id

listToMaybe :: [a] -> Maybe a
listToMaybe (x:xs) = Just x
listToMaybe []     = Nothing

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes ((Just a):xs) = a : catMaybes xs
catMaybes (Nothing:xs)  = catMaybes xs
catMaybes []            = []

-- my attempt
-- flipMaybe :: [Maybe a] -> Maybe [a]
-- flipMaybe xs = if all isJust xs then Just (catMaybes xs) else Nothing

-- better
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe []           = Just []
flipMaybe (Nothing:xs) = Nothing
flipMaybe (Just x:xs)  = case flipMaybe xs of Nothing -> Nothing
                                              Just l  -> Just (x:l)
