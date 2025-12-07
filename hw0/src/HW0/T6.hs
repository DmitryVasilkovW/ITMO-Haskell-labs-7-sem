module HW0.T6
  ( distrib
  , a
  , b
  , c
  , a_whnf
  , b_whnf
  , c_whnf
  ) where

import Data.Char (isSpace)

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left x) = (Left x, Left x)
distrib (Right (y, z)) = (Right y, Right z)

a :: (Either [Char] b, Either [Char] c)
a = distrib (Left ("AB" ++ "CD" ++ "EF"))

b :: [Bool]
b = map isSpace "Hello, World"

c :: String
c = if (1 :: Int) > 0 || error "X" then "Y" else "Z"

a_whnf :: Either [Char] b
a_whnf = Left ("AB" ++ "CD" ++ "EF")

b_whnf :: [Bool]
b_whnf = (isSpace 'H') : map isSpace "ello, World"

c_whnf :: String
c_whnf = "Y"
