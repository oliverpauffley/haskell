data BinaryTree a =
   Leaf
 | Node (BinaryTree a) a (BinaryTree a)
 deriving (Eq, Ord, Show)

insert' :: Ord a
        => a
        -> BinaryTree a
        -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f Leaf                = Leaf
mapTree f (Node Leaf a Leaf)  =Node Leaf (f a) Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)


foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf                = b
foldTree f b (Node left a right) =  foldTree f (foldTree f (f a b) left) right
