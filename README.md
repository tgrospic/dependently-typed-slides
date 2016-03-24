layout: true

background-size: cover
background-image: url(assets/bg1.jpg)
---

class: center, middle

# λ<sub>Π</sub>

# Dependently Typed

Tomislav Grospić

Lambda Meetup - 24 March 2016, Zagreb

---

# Why LambdaPi?

* # More expressive type system

* # Type level computation

* # Types help us write programs

* # Proofs included in program source

---

# Why LambdaPi?

> Haskell, as implemented in the Glasgow Haskell Compiler (GHC),
is enriched with many extensions that support **type-level programming**,
such as **promoted datatypes**, **kind polymorphism**, and **type
families**. Yet, the expressiveness of the type-level language remains
limited.

> .src[[Promoting Functions to Type Families in Haskell][type-promotion] - 2014]

<!-- break -->

> Current Haskell implementations, for instance, support **generalized algebraic data types**,
**multi-parameter type classes with functional dependencies**, **associated types and type
families**, **impredicative higher-ranked types**, and there are even more extensions on the way.
Programmers seem to be willing to go to great lengths just to avoid dependent types.

> .src[[A tutorial implementation of a dependently typed lambda calculus][lam-pi-löh-mcbride-swierstra] - 2009]

[type-promotion]: https://www.cis.upenn.edu/~eir/papers/2014/promotion/promotion.pdf
[lam-pi-löh-mcbride-swierstra]: https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf

---

# We all love lambda, right? :)

<!-- [pwadler-lam]: /assets/pwadler-super-lam.jpg -->

.zoom1[
![](/assets/wadler-1.jpg)]

---

# We all love lambda, right? :)

.zoom1[
![](/assets/wadler-2.jpg)]

???
- lambda in any calculus
- curry-howard isomorphism connects with logic
- propositions as types
- each proposition is interpreted as the set of its proofs <==> type

---

# We all love lambda, right? :)

Everything is an expression.

```hs
check p = if p then "true" else "false"
```

Functions as values.

```hs
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : (map f xs)
```

--

Parametric polymorphism (generics in C# and Java) - types depends on types.  

It is the same `map` but with _more_ explicit quantifier.

```hs
{-# LANGUAGE RankNTypes #-}
map' :: forall a b. (a -> b) -> [a] -> [b]
map' = map
```

???
RankNTypes only to allow `forall`, still rank-1 type.

---

# We have variables in types! Really?

`forall` binds variables `a` and `b` in the type scope.

```hs
map :: forall a b. `(a -> b) -> [a] -> [b]`
```

--

<!-- We can see type definition as the **type level function** i.e. `id = Λa:*.λx:a.x`. -->

In Haskell `*` star is the **type of type** (kind).

```hs
{-# LANGUAGE KindSignatures #-}
map :: forall (a :: *) (b :: *). (a -> b) -> [a] -> [b]
```

--

On _kind_ level how many types we have and what is the type of _kind_?

```hs
λ> :k Int
Int :: *
λ> :k (->)
(->) :: * -> * -> *
λ> :k *
```

---

# Back to the cave

I know lower case letters for types. Jesus will come. :)

```hs
List<b> map<a, b>(Func<a, b> f, List<a> xs)
```

--

In C# `Type` _represent_ type of type. But `Type` have different meaning at compile time and runtime.  
This is like `Expression` in LINQ which duplicates `Type`.

```hs
var x = 42;

Type t = x.GetType();
```

--

We can now specify type of generic arguments (variables).

```hs
List<b> map<Type a, Type b>(Func<a, b> f, List<a> xs)
```

What is the type of `Type`?

---

# What else we need

This is totally OK function. `typeof` _cast_ to runtime _representation_ of a type (one and only) because **functions can't return types**.  
We also lose all the compile time checks, this `Type` is a runtime thing, so dynamic. :)

```hs
Type isSingleton<a>(Boolean p) {
  if (p) return `typeof`(a);
  else   return `typeof`(List<a>);
}
```

--

But this is imposible. Return type depends on boolean value. Weird. :)

```hs
isSingleton<a>(p) makeSingle<Type a>(Boolean p, a x) {
  if (p) return x;
  else   return new[] { x, x }.ToList();
}

makeSingle(true, 42);  -- 42
makeSingle(false, "42");  -- ["42", "42"]
```

--

All we want is to remove `typeof` on syntactic level. ;)

---

# Everything is a term

**One language** at compile-time and run-time.

--

##### **Types can contain values, functions can return types.**

##### **Terms**, **types**, and **kinds** now reside on the **same syntactic level**.

--

We are writing programs with bunch of terms, some of them are evaluated at what we call compile-time and some at what we call run-time.

One can say that we have compiler available at runtime (yeaa! Roslyn) and also one can say that we have runtime system available at compile time (we love code generation, build systems, meta specifications...).

--

#### Context in which terms are evaluated tells as about their behaviour.

---

# Everything is a term

#### Our chains are broken and we are free with λ<sub>Π</sub> powers!

.zoom2[
![](/assets/sb-lambda.jpg)]

???
- terms reduction (evaluation)
- termination
- types erasure
---

# Idris

* Pure functional programming language
* Full dependent types
* Compiles to C, LLVM, Java, Javascript (browser and Node)
* Syntax inspired by Haskell
* Strict evaluation order
* Extensible syntax
* Integrated theorem proving
* Free software - everything is on Github

```hs
MyInt : Type
MyInt = Int

numbers : List MyInt
numbers = [10, 21, 42]

compose : (b -> c) -> (a -> b) -> a -> c
compose f g = \x => f (g x)
```

---

# You already know Idris!

Explictly and implicitly defined arguments

```hs
map : `(a : Type)` -> `(b : Type)` -> (a -> b) -> List a -> List b
map `a` `b` f []      = []
map `a` `b` f (x::xs) = f x :: map `a` `b` f xs

map' : `{a : Type}` -> `{b : Type}` -> (a -> b) -> List a -> List b
map' f []      = []
map' f (x::xs) = f x :: map' f xs

map' : (a -> b) -> List a -> List b
map' f []      = []
map' f (x::xs) = f x :: map' f xs
```

--

Whoa we have a functor! Generalizes `List` which type is `Type -> Type`.

```hs
mapF : {f : Type -> Type} -> (a -> b) -> f a -> f b
```

---

# Dependent types

Where simple types express a program's meaning, _dependent types_ allow types to be predicated on values, **so expressing a more precise meaning**.

This allows the programmer both to write programs and to verify specifications within the same framework.

Dependent types are types that depend on elements of other types.

**Dependent product type** (Π type, dependent function) models universal quantifier.

**Dependent sum type** (∑ type, dependent pair) models existential quantifier.

---

## Π type

**Π<sub>(x : A)</sub> B(x)** - pi-type, dependent (product) type

A dependent function's **return type may depend on the value** (not just type) of an argument.

```hs
isSingleton : Bool -> Type
isSingleton True = Nat
isSingleton False = List Nat

-- dependently typed function
mkSingle : (`x` : Bool) -> isSingleton `x`
mkSingle True = 10
mkSingle False = [42, 21]
```

```hs
λΠ> mkSingle True
10 : Nat
λΠ> mkSingle False
[42, 21] : List Nat
```

---

## ∑ type

**∑<sub>(x : A)</sub> B(x)** - sigma-type, dependent pair/sum type

Dependent pairs allow the type of the **second element** of a pair to **depend on** the value of the **first element**.

```hs
vec : (`n` : Nat ** Vect `n` String)
vec = (`2` ** ["4", "2"])

-- or with types infered
vec : (n ** Vect n String)
vec = (_ ** ["4", "2"])
```

---

# That's it!

--

From our quasi C# example. This is so {f} awesome. :)

```hs
isSingleton : {a : Type} -> Bool -> Type
isSingleton {a} True = a
isSingleton {a} False  = List a

makeSingle : {a : Type} -> (x : Bool) -> a -> isSingleton {a} x
makeSingle True y = y
makeSingle False  y = [y]

id' : a -> a
id' = makeSingle True

toList' : a -> List a
toList' = makeSingle False
```

---

# That's it!

.zoom1[
![](/assets/amy-sheldon.jpg)]

---

# Data types

<!-- Types are expressed in terms of data. -->
Dependent types are types expressed in terms of data, explicitly relating their inhabitants to that data. Sounds Lisp-y. :)

`Nat` can be constructed from `Z` (zero) or `S` (successor) of another `Nat`. It is inductive type `S (S (S Z))`.

```hs
data Nat : Type  where
  Z : Nat
  S : Nat -> Nat
```

Also can be written shortly like in Haskell.

```hs
data Nat = Z | S Nat
```

--

We make case analysis on function arguments. Why not compiler write code for us. :)  
For that must be a demo!

```hs
plus : Nat -> Nat -> Nat
plus Z     y = y
plus (S x) y = S (plus x y)
```

???
- meta-variables (holes)
- type inference
- pattern matching
---

# Parametrized data types

`List` defined in Idris prelude [List.idr](https://github.com/idris-lang/Idris-dev/blob/master/libs/prelude/Prelude/List.idr).

```hs
data List : (elem : Type) -> Type where
  -- Empty list
  Nil : List elem
  -- A non-empty list, consisting of a head element and the rest of
  -- the list.
  (::) : (x : elem) -> (xs : List elem) -> List elem
```
**Dependent type** `Vect` defined in Idris base lib [Vect.idr](https://github.com/idris-lang/Idris-dev/blob/master/libs/base/Data/Vect.idr).

```hs
data Vect : Nat -> Type -> Type where
  -- Empty vector
  Nil  : Vect Z a
  -- A non-empty vector of length \`S k`, consisting of a head element and
  -- the rest of the list, of length \`k`.
  (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a
```

???
- Functor instance
- new syntax optional _instance_; _interface_ instead of _class_
---

# Haskell - Datatype promotion

With `-XDataKinds`, GHC automatically promotes every suitable datatype to be a kind, and its (value) constructors to be type constructors.

```hs
{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}
data Nat = Z | S Nat

data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

data Vect :: Nat -> * -> * where
    Nil   :: Vect Z a
    (:-) :: a -> Vect n a -> Vect (S n) a

replicate' :: SNat n -> a -> Vect n a
replicate' SZ     _ = Nil
replicate' (SS n) a = a :- replicate' n a
```

---

# Haskell - Singletons library

.small-code[```hs
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
```]

---

# Typed lambda evaluator

.small-code[```hs
data Ty = TyBool | TyFun Ty Ty

typeOf : Ty -> Type
typeOf TyBool = Bool
typeOf (TyFun x y) = typeOf x -> typeOf y

using (n : Nat, ctx : Vect n Ty)
  data Expr : Vect n Ty -> Ty -> Type where
    T     : Expr ctx TyBool
    F     : Expr ctx TyBool
    IF    : Expr ctx TyBool -> Expr ctx t -> Expr ctx t -> Expr ctx t
    Var'  : (i : Fin n) -> Expr ctx (index i ctx)
    Lam'  : Expr (a :: ctx) b -> Expr ctx (TyFun a b)
    App'  : Expr ctx (TyFun a b) -> Expr ctx a -> Expr ctx b

data Env : Vect n Ty -> Type where
  Nil  : Env []
  (::) : typeOf t -> Env ctx -> Env (t::ctx)

lookup : (i : Fin n) -> (env : Env ctx) -> typeOf (index i ctx)
lookup FZ     (x :: xs) = x
lookup (FS x) (y :: xs) = lookup x xs

eval : Env ctx -> Expr ctx t -> typeOf t
eval env (Var' i) = lookup i env
eval env (Lam' body) = \x => eval (x :: env) body
eval env (App' f x) = eval env f $ eval env x
eval env T = True
eval env F = False
eval env (IF expr expr' expr'') = if eval env expr then eval env expr' else eval env expr''
```]

---

# Links

[Existential Type - Robert Harper](https://existentialtype.wordpress.com)

[A tutorial implementation of a dependently typed lambda calculus](https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf)

[The Power of Pi](https://cs.ru.nl/~wouters/Publications/ThePowerOfPi.pdf)

[Faking It - Simulating Dependent Types in Haskell](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.22.2636&rep=rep1&type=pdf)

VIDEO

[Type-Driven Development in Idris - Edwin Brady](https://www.youtube.com/watch?v=X36ye-1x_HQ)

[Coding for Types: The Universe Patern in Idris - David Christiansen](https://www.youtube.com/watch?v=AWeT_G04a0A)

---

class: center, middle

# THANK YOU
