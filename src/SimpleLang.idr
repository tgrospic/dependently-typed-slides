module SimpleLang

data Ty = TyInt | TyBool

interp : Ty -> Type
interp TyInt  = Int
interp TyBool = Bool

data Expr : Ty -> Type where
  CInt  : Int -> Expr TyInt
  CBool : Bool -> Expr TyBool
  Add   : Expr TyInt -> Expr TyInt -> Expr TyInt
  LT    : Expr TyInt -> Expr TyInt -> Expr TyBool

eval : Expr t -> interp t
eval (CInt x) = x
eval (CBool x) = x
eval (Add x y) = eval x + eval y
eval (LT x y) = eval x < eval y
