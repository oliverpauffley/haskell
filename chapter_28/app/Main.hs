module Main where

import           Criterion.Main

-- custom operators
infixl 9 !?
{-# INLINABLE (!?) #-}

(!?) :: [a] -> Int -> Maybe a
xs !? n
 | n < 0 = Nothing
 | otherwise = foldr
    (\x r k ->
       case k of
         0 -> Just x
         _ -> r (k-1))
    (const Nothing) xs n


myList :: [Int]
myList = [1..9999]


-- Difference list
newtype DList a = DL {unDL :: [a] -> [a]}

empty :: DList a
empty = DL $ const []
{-# INLINE empty #-}

singleton :: a -> DList a
singleton a =  DL $ (:) a
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList a =  unDL a []
{-# INLINE toList #-}


-- Prepend a single element to a dlist.
infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

-- Append a single element to a dlist
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (:) x)
{-# INLINE snoc #-}

-- Append dlists
append :: DList a -> DList a -> DList a
append (DL xs) (DL ys) = DL (xs . ys)
{-# INLINE append #-}

-- concatting -- SLOW!
schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs =
          go (n-1)
          (singleton n `append` xs)


data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

-- adds an item
push :: a -> Queue a -> Queue a
push x xs = Queue{ enqueue = x:enqueue xs,
                   dequeue = dequeue xs
                 }

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue es []) = pop $ Queue (init es) ([last es])
pop (Queue es ds) = Just (last ds, Queue es (init ds))


listPoppy :: Int -> [Int]
listPoppy x = go x [1..10000]
  where go 0 xs = xs
        go n xs
          | even n = go (n-1) (tail xs)
          | otherwise = go (n-1) (init xs)

queuePoppy :: Int -> Queue Int
queuePoppy x = go x (Queue [1..5000] [5000..10000])
  where go 0 xs = xs
        go n xs
          | even n = go (n-1) (push n xs)
          | otherwise = go (n-1) (popper xs)
        popper :: Queue a -> Queue a
        popper xs = case pop xs of
          Just (_, qs) -> qs
          Nothing      -> error "couldn't pop"

main :: IO ()
main = defaultMain [
  bgroup "!? operator" [
      bench "index list 9999" $ whnf (myList !!) 9998
      , bench "index list maybe index 9999" $ whnf (myList !?) 9998],
    bgroup "dlists" [
      bench "concat list" $
      whnf schlemiel 123456
      , bench "concat dlist" $
      whnf constructDlist 123456],
    bgroup "queue" [
      bench "pop on lists" $
      whnf listPoppy 9998
      , bench "pop on queues" $
      whnf queuePoppy 9998
                   ]
  ]
