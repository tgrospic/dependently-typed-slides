{-# LANGUAGE TemplateHaskell, TypeFamilies, GADTs, KindSignatures, DataKinds, PolyKinds, ScopedTypeVariables, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, RankNTypes, UndecidableInstances, FlexibleInstances, InstanceSigs, DefaultSignatures #-}
module SingletonsSample
    (
    ) where

import Data.Singletons.Prelude
import Data.Singletons.TH

singletons [d|
  data Nat = Z | S Nat
             deriving (Show, Eq, Ord)

  (+) :: Nat -> Nat -> Nat
  Z   + n = n
  S m + n = S (m + n)

  (*) :: Nat -> Nat -> Nat
  Z   * _ = Z
  S n * m = n * m + m

  min :: Nat -> Nat -> Nat
  min Z     Z     = Z
  min Z     (S _) = Z
  min (S _) Z     = Z
  min (S m) (S n) = S (min m n)
  |]


