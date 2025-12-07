{-# LANGUAGE TypeApplications #-}

module HW0.T4 where

import Data.Function (fix)
import Numeric.Natural (Natural)

repeat' :: a -> [a]
repeat' = fix . (:) 

map' :: (a -> b) -> [a] -> [b]
map' f = fix $ \rec lst -> case lst of
    [] -> []
    (x:xs) -> f x : rec xs

fib :: Natural -> Natural
fib = fix $ \rec n -> case n of
    0 -> 0
    1 -> 1
    _ -> rec (n - 1) + rec (n - 2)

fac :: Natural -> Natural
fac = fix $ \rec n -> case n of
    0 -> 1
    _ -> n * rec (n - 1)
