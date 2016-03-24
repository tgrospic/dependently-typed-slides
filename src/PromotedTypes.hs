{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}
module PromotedTypes
    (
    ) where

data Nat = Z | S Nat

data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

data Vect :: Nat -> * -> * where
    Nil   :: Vect Z a
    (:-) :: a -> Vect n a -> Vect (S n) a

head' :: Vect (S n) a -> a
head' (a :- _) = a

tail' :: Vect (S n) a -> Vect n a
tail' (_ :- xs) = xs

replicate' :: SNat n -> a -> Vect n a
replicate' SZ     _ = Nil
replicate' (SS n) a = a :- replicate' n a

map' :: (a -> b) -> Vect n a -> Vect n b
map' _ Nil       = Nil
map' f (x :- xs) = f x :- map' f xs

fromList :: SNat n -> [a] -> Vect n a
fromList SZ     []   = Nil
fromList (SS n) (x : xs) = x :- (fromList n xs)

