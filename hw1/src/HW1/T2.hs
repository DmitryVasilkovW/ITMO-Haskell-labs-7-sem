module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural

data N = Z | S N

nplus :: N -> N -> N
nplus = undefined

nmult :: N -> N -> N
nmult = undefined

nsub :: N -> N -> Maybe N
nsub = undefined

ncmp :: N -> N -> Ordering
ncmp = undefined

nFromNatural :: Natural -> N
nFromNatural = undefined

-- Uncomment this function signature to implement this function.
-- The correct signature is commented to avoid compilation failure
-- with '-Werror' in the template project.
--
-- nToNum :: Num a => N -> a
nToNum :: N -> a
nToNum = undefined

nEven :: N -> Bool
nEven = undefined

nOdd :: N -> Bool
nOdd = undefined

ndiv :: N -> N -> N
ndiv = undefined

nmod :: N -> N -> N
nmod = undefined
