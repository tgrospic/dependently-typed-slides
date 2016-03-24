{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}
{-# LANGUAGE TypeOperators, TypeFamilies #-}
module TypeOperators
    (
    ) where

--data Nat = Z | S Nat

data Nat :: * where
    Z :: Nat
    S :: Nat -> Nat

type family (x :: Nat) + (y :: Nat) where
    'Z   + y = y
    'S x + y = 'S (x + y)

(+#) :: Nat -> Nat -> Nat
Z   +# y = y
S x +# y = S (x +# y)

data Vect :: Nat -> * -> * where
    Nil   :: Vect Z a
    (:-:) :: a -> Vect n a -> Vect (S n) a

