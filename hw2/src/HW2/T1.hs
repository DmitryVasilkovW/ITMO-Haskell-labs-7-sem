module HW2.T1
  ( Tree (..)
  , tfoldr
  ) where

data Tree a = Leaf | Branch Int (Tree a) a (Tree a)

instance (Show a) => Show (Tree a) where
  show = printTree

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ accumulateValue Leaf = accumulateValue
tfoldr function accumulateValue (Branch _ left treeValue right) =
  tfoldr function rightResult left
  where
    rightResult = function treeValue subRightResult
    subRightResult = tfoldr function accumulateValue right

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