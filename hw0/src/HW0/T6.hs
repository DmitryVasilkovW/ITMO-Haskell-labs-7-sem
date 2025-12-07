module HW0.T6 where

import Data.Char (isSpace)

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left a) = (Left a, Left a)
distrib (Right (b, c)) = (Right b, Right c)

a = distrib (Left ("AB" ++ "CD" ++ "EF"))
b = map isSpace "Hello, World"
c = if 1 > 0 || error "X" then "Y" else "Z"

-- WHNF forms:
a_whnf = Left ("AB" ++ "CD" ++ "EF")
b_whnf = (isSpace 'H') : map isSpace "ello, World"
c_whnf = "Y"
