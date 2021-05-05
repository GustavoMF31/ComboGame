module Nat (Nat, nat, natMinus, natAsDouble, natAdd, monus) where

import Data.Maybe (fromMaybe)
import GHC.Natural (Natural)

-- A safe wrapper around GHC.Natural
newtype Nat = MkNat Natural
    deriving (Eq, Ord, Show)

natMinus :: Nat -> Nat -> Maybe Nat
natMinus (MkNat x) (MkNat y)
    | x < y = Nothing
    | otherwise = Just $ MkNat $ x - y

nat :: Natural -> Nat
nat = MkNat

natAsDouble :: Nat -> Double
natAsDouble (MkNat n) = fromIntegral n

natAdd :: Nat -> Nat -> Nat
natAdd (MkNat m) (MkNat n) = MkNat $ m + n

(.:) :: (a -> b) -> (x -> y -> a) -> x -> y -> b
(.:) = (.) . (.)

monus :: Nat -> Nat -> Nat
monus = fromMaybe (nat 0) .: natMinus
