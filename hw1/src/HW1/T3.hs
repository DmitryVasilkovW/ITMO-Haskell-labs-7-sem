module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Meta = ()

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize = undefined

tdepth :: Tree a -> Int
tdepth = undefined

-- Uncomment this function signature to implement this function.
-- The correct signature is commented to avoid compilation failure
-- with '-Werror' in the template project.
--
-- tmember :: Ord a => a -> Tree a -> Bool
tmember :: a -> Tree a -> Bool
tmember = undefined

-- Uncomment this function signature to implement this function.
-- The correct signature is commented to avoid compilation failure
-- with '-Werror' in the template project.
--
-- tinsert :: Ord a => a -> Tree a -> Tree a
tinsert :: a -> Tree a -> Tree a
tinsert = undefined

-- Uncomment this function signature to implement this function.
-- The correct signature is commented to avoid compilation failure
-- with '-Werror' in the template project.
--
-- tFromList :: Ord a => [a] -> Tree a
tFromList :: [a] -> Tree a
tFromList = undefined
