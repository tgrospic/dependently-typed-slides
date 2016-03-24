module SimpleLambdaLang

import Data.Fin
import Data.Vect

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


not'' : Expr ctx (TyFun TyBool TyBool)
not'' = Lam' $ IF (Var' 0) F T
