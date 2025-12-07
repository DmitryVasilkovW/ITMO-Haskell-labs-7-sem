module HW0.T5 where

import Numeric.Natural (Natural)

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz = \f x -> x

ns :: Nat a -> Nat a
ns n = \f x -> f (n f x)

nplus :: Nat a -> Nat a -> Nat a
nplus a b = \f x -> a f (b f x)

nmult :: Nat a -> Nat a -> Nat a
nmult a b = \f x -> a (b f) x

nFromNatural :: Natural -> Nat a
nFromNatural n = \f x -> iterate f x !! fromIntegral n

nToNum :: Num a => Nat a -> a
nToNum n = n (+1) 0
