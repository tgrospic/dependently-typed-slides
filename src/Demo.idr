module Demo

import Data.Vect

-- %default total

isSingleton : Bool -> Type
isSingleton True = Int
isSingleton False = List Nat

plus' : Nat -> Nat -> Nat
plus' Z j = j
plus' (S k) j = S (plus k j)

append : Vect n a -> Vect m a -> Vect (n + m) a
append [] ys = ys
append (x :: xs) ys = ?sd

take' : (n : Nat) -> Vect (n + m) a -> Vect n a
take' = ?rhs

total
head' : Vect (S n) a -> a
head' (x :: xs) = x
