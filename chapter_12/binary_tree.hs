
data BinaryTree a = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)


unfold :: (a -> Maybe (a, b, a))
        -> a
        -> BinaryTree b
unfold f a = case f a of
  Nothing        -> Leaf
  Just (x, y, z) -> Node (unfold f x) y (unfold f z)


treeBuild :: Integer -> BinaryTree Integer
treeBuild = unfold tBuild

tBuild :: Integer -> Maybe(Integer, Integer, Integer)
tBuild n
  | n <= 0 = Nothing
  | otherwise = Just (n-1, n-1, n-1)
