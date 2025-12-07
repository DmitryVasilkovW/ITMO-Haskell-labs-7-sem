{-# LANGUAGE TypeApplications #-}

module HW0.T4
  ( repeat'
  , map'
  , fib
  , fac
  ) where

import Data.Function (fix)
import Numeric.Natural (Natural)

repeat' :: a -> [a]
repeat' = fix . (:)

map' :: (a -> b) -> [a] -> [b]
map' f = fix $ \step list ->
 case list of
    [] -> []
    (x:xs) -> f x : step xs

fib :: Natural -> Natural
fib = fix $ \next number ->
 case number of
    0 -> 0
    1 -> 1
    _ -> next (number - 1) + next (number - 2)

fac :: Natural -> Natural
fac = fix $ \recurse n ->
 case n of
    0 -> 1
    _ -> n * recurse (n - 1)
