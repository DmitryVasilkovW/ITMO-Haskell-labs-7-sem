module HW1.T3
  ( Tree (..),
    tsize,
    tdepth,
    tmember,
    tinsert,
    tFromList,
  )
where

import Prelude hiding (Left, Right)

type Meta = Int

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)

instance (Show a) => Show (Tree a) where
  show = printTree

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch left value right = Branch (newSize left right) left value right
  where
    newSize left' right' = 1 + tsize left' + tsize right'

tsize :: Tree a -> Int
tsize Leaf                = 0
tsize (Branch size _ _ _) = size

tdepth :: Tree a -> Int
tdepth Leaf                    = 0
tdepth (Branch _ left _ right) = 1 + max (tdepth left) (tdepth right)

tmember :: (Ord a) => a -> Tree a -> Bool
tmember _ Leaf = False
tmember value (Branch _ left anotherValue right)
  | value < anotherValue = tmember value left
  | value > anotherValue = tmember value right
  | otherwise = True

tbalance :: Tree a -> Tree a
tbalance Leaf = Leaf
tbalance tree@(Branch _ left value right)
  | subTreesAreBalanced = tree
  | shouldBalance right left = fullRightRotate
  | shouldBalance left right = fullLeftRotate
  | otherwise = tree
  where
    subTreesAreBalanced = tsize left + tsize right <= 1
    sizeTreshhold = 3
    sizeSubtreeTreshhold = 2
    shouldBalance subTree anotherSubTree = tsize subTree > sizeTreshhold * tsize anotherSubTree
    shouldBalanceMainTreeFromLeft leftSubTree rightSubTree =
      tsize rightSubTree < sizeSubtreeTreshhold * tsize leftSubTree
    shouldBalanceMainTreeFromRight leftSubTree rightSubTree =
      tsize leftSubTree < sizeSubtreeTreshhold * tsize rightSubTree

    fullRightRotate =
      rotateTree
        right
        (mkBranch left value $ rotateRight right)
        shouldBalanceMainTreeFromRight
        rotateLeft

    fullLeftRotate =
      rotateTree
        left
        (mkBranch (rotateLeft left) value right)
        shouldBalanceMainTreeFromLeft
        rotateRight

    rotateTree mainSubTree mkBranchFunc shouldBalanceMainTree rotateFunc =
      case mainSubTree of
        Branch _ firstSubTree _ secondSubTree
          | shouldBalanceMainTree firstSubTree secondSubTree -> rotateFunc tree
          | otherwise -> rotateFunc mkBranchFunc
        Leaf -> tree

    rotateLeft (Branch _ l val (Branch _ rl rVal rr)) = mkBranch (mkBranch l val rl) rVal rr
    rotateLeft tree'                                  = tree'
    rotateRight (Branch _ (Branch _ ll lVal lr) val r) = mkBranch ll lVal (mkBranch lr val r)
    rotateRight tree'                                  = tree'

tinsert :: (Ord a) => a -> Tree a -> Tree a
tinsert value Leaf = mkBranch Leaf value Leaf
tinsert value tree@(Branch _ left anotherValue right)
  | value < anotherValue = tbalance $ mkBranch (tinsert value left) anotherValue right
  | value > anotherValue = tbalance $ mkBranch left anotherValue $ tinsert value right
  | otherwise = tree

tFromList :: (Ord a) => [a] -> Tree a
tFromList = foldr tinsert Leaf

-- Function to visualize the tree.
-- The parameter 'tree' is the tree to be visualized.
printTree :: (Show a) => Tree a -> String
printTree Leaf = "Leaf"
printTree tree = renderTree tree "" True
  where
    renderTree Leaf _ _ = ""
    renderTree (Branch _ left value right) prefix isLast =
      prefix
        ++ (if isLast then "|__ " else "|-- ")
        ++ show value
        ++ "\n"
        ++ renderSubTree left False
        ++ renderSubTree right True
      where
        renderSubTree Leaf _ = ""
        renderSubTree node isLast' =
          renderTree node (prefix ++ (if isLast then "    " else "|   ")) isLast'
