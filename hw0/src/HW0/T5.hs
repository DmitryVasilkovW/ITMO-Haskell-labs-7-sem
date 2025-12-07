module HW0.T5 where

import Numeric.Natural (Natural)

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz = \successor zero -> zero

ns :: Nat a -> Nat a
ns number = \successor zero -> successor (number successor zero)

nplus :: Nat a -> Nat a -> Nat a
nplus a b = \successor zero -> a successor (b successor zero)

nmult :: Nat a -> Nat a -> Nat a
nmult a b = \successor zero -> a (b successor) zero

nFromNatural :: Natural -> Nat a
nFromNatural natural = \successor zero -> iterate successor zero !! fromIntegral natural

nToNum :: Num a => Nat a -> a
nToNum number = number (+1) 0
