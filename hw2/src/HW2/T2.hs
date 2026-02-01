module HW2.T2
  ( splitOn
  , joinWith
  ) where

import Data.List.NonEmpty (NonEmpty (..))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn _ [] = [] :| []
splitOn sep array = go array
  where
    go [] = [] :| []
    go arr =
      let (chunk, other) = break (== sep) arr
          remaining = case other of
            [] -> []
            (_:rest) -> goAndMapToArray rest
      in chunk :| remaining
      where
        goAndMapToArray value =
         case go value of
           h :| t -> h : t

joinWith :: a -> NonEmpty [a] -> [a]
joinWith _ (first :| []) = first
joinWith sep (first :| rest) = first ++ concatMap (sep :) rest
