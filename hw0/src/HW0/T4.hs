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
fib n = go 0 1 n
  where
    go current next 0 = current
    go current next remaining = go next (current + next) (remaining - 1)

fac :: Natural -> Natural
fac = fix $ \recurse n ->
 case n of
    0 -> 1
    _ -> n * recurse (n - 1)
