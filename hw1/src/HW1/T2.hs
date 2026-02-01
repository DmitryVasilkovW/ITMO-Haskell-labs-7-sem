module HW1.T2
  ( N (..),
    nplus,
    nmult,
    nsub,
    nFromNatural,
    nToNum,
    ncmp,
    nEven,
    nOdd,
    ndiv,
    nmod,
  )
where

import Numeric.Natural

data N = Z | S N

instance Show N where
  show n = show (nToNum n :: Integer)

nplus :: N -> N -> N
nplus Z otherNumber          = otherNumber
nplus (S number) otherNumber = S (nplus number otherNumber)

nmult :: N -> N -> N
nmult Z _ = Z
nmult (S number) otherNumber =
  nplus otherNumber $
    nmult number otherNumber

nsub :: N -> N -> Maybe N
nsub Z (S _)                    = Nothing
nsub number Z                   = Just number
nsub (S number) (S otherNumber) = nsub number otherNumber

ncmp :: N -> N -> Ordering
ncmp Z Z                        = EQ
ncmp Z (S _)                    = LT
ncmp (S _) Z                    = GT
ncmp (S number) (S otherNumber) = ncmp number otherNumber

nFromNatural :: Natural -> N
nFromNatural 0      = Z
nFromNatural number = S (nFromNatural (number - 1))

-- Uncomment this function signature to implement this function.
-- The correct signature is commented to avoid compilation failure
-- with '-Werror' in the template project.
--
-- nToNum :: Num a => N -> a
nToNum :: (Num a) => N -> a
nToNum Z          = 0
nToNum (S number) = 1 + nToNum number

nEven :: N -> Bool
nEven Z          = True
nEven (S number) = nOdd number

nOdd :: N -> Bool
nOdd Z          = False
nOdd (S number) = nEven number

-- Implemented with safe analog functions div and mod,
-- but commented out due to change in method signatures.
-- safeNfullDivide :: N -> N -> Either String (N, N)
-- safeNfullDivide _ Z = Left "Division by zero is undefined"
-- safeNfullDivide number divider = go number Z
--  where
--    go num count
--      | ncmp num divider == LT = Right (count, num)
--      | otherwise = case nsub num divider of
--          Nothing -> Left "Subtraction failed"
--          Just number' -> go number' (S count)
--
-- safeNdiv :: N -> N -> Either String N
-- safeNdiv number divider = fmap fst (safeNfullDivide number divider)
--
-- safeNmod :: N -> N -> Either String N
-- safeNmod number divider = fmap snd (safeNfullDivide number divider)

ndiv :: N -> N -> N
ndiv number divider =
  go $ nsub number divider
    where
      go Nothing          = Z
      go (Just subResult) = S $ ndiv subResult divider

nmod :: N -> N -> N
nmod number divider =
  go $ nsub number divider
  where
    go Nothing          = number
    go (Just subResult) = nmod subResult divider
