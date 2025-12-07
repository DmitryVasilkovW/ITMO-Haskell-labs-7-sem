{-# LANGUAGE TypeOperators #-}

module HW0.T1
  ( type (<->)(Iso)
  , flipIso
  , runIso
  , distrib
  , assocPair
  , assocEither
  ) where

data a <-> b = Iso (a -> b) (b -> a)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso forward backward) = Iso backward forward

runIso :: (a <-> b) -> (a -> b)
runIso (Iso forward _) = forward

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left value) = (Left value, Left value)
distrib (Right (first, second)) = (Right first, Right second)

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso to from
  where
    to (first, (second, third)) = ((first, second), third)
    from ((first, second), third) = (first, (second, third))

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso to from
  where
    to (Left value) = Left (Left value)
    to (Right (Left value)) = Left (Right value)
    to (Right (Right value)) = Right value

    from (Left (Left value)) = Left value
    from (Left (Right value)) = Right (Left value)
    from (Right value) = Right (Right value)