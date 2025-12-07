module HW0.T2 where

import Data.Void (Void)

type Not a = a -> Void

doubleNeg :: a -> Not (Not a)
doubleNeg x = \f -> f x

reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg tripleNeg = \a -> tripleNeg (\doubleNeg -> doubleNeg a)
