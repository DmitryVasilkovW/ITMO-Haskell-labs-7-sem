module HW2.T4
  ( ListPlus (..)
  , Inclusive (..)
  , DotString (..)
  , Fun (..)
  ) where


data ListPlus a = a :+ ListPlus a | Last a
  deriving (Show, Eq)

infixr 5 :+

instance Semigroup (ListPlus a) where
  Last value       <> array = value :+ array
  (first :+ rest) <> array = first :+ (rest <> array)


data Inclusive a b = This a | That b | Both a b
  deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  This value    <> This anotherValue    = This (value <> anotherValue)
  This value    <> That anotherValue    = Both value anotherValue
  This value    <> Both anotherValue value'  = Both (value <> anotherValue) value'

  That value    <> That anotherValue    = That (value <> anotherValue)
  That value    <> This anotherValue    = Both anotherValue value
  That value    <> Both anotherValue value'  = Both anotherValue (value <> value')

  Both value anotherValue  <> This value'    = Both (value <> value') anotherValue
  Both value anotherValue  <> That value'    = Both value (anotherValue <> value')
  Both value anotherValue  <> Both value' anotherValue'  =
   Both (value <> value') (anotherValue <> anotherValue')


newtype DotString = DS String
  deriving (Show, Eq)

instance Semigroup DotString where
  DS "" <> DS anotherValue  = DS anotherValue
  DS value  <> DS "" = DS value
  DS value  <> DS anotherValue  = DS (value ++ "." ++ anotherValue)

instance Monoid DotString where
  mempty = DS ""
  mappend = (<>)


newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  F function <> F anotherFunction = F (function . anotherFunction)

instance Monoid (Fun a) where
  mempty = F id
  mappend = (<>)
