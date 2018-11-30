module ExprDSL where

import Prelude

import Data.Leibniz (type (~), coerceSymm)

class EvalExpr e o | e -> o where
  evalExpr :: e -> o

data ExprF o i
  = If (i ~ Boolean) (Expr Boolean) (Expr o) (Expr o)
  | Val (i ~ Void) o
  | Equal (o ~ Boolean) (Expr i) (Expr i)

data Expr o = Expr (∀ e. (∀ i. Eq i => ExprF o i -> e) -> e)

mkExpr :: ∀ o i. Eq o => Eq i => ExprF o i -> Expr o
mkExpr e = Expr (_ $ e)

runExpr :: ∀ o. Eq o => (∀ i. Eq i => ExprF o i -> o) -> Expr o -> o
runExpr r (Expr f) = f r

if_ :: ∀ o. Eq o => Expr Boolean -> Expr o -> Expr o -> Expr o
if_ x y z = mkExpr $ If identity x y z

val_ :: ∀ o. Eq o => o -> Expr o
val_ = mkExpr <<< Val identity

equal_ :: ∀ i. Eq i => Eq i => Expr i -> Expr i -> Expr Boolean
equal_ x y = mkExpr $ Equal identity x y

instance evalX :: Eq o => EvalExpr (Expr o) o where
  evalExpr = runExpr eval
    where
      eval :: ∀ i. Eq i => ExprF o i -> o
      eval (If _ x y z) = if evalExpr x then evalExpr y else evalExpr z
      eval (Val _ x) = x
      eval (Equal l x y) = coerceSymm l $ evalExpr x == evalExpr y

a :: Expr Boolean
a = if_
  (equal_
    (val_ 1)
    (val_ 1)
  )
  (val_ true)
  (val_ false)

b :: Expr String
b = if_ a
  (if_
    (equal_
      (val_ false)
      (val_ false)
    )
    (val_ "yep")
    (val_ "nah")
  )
  (val_ "uhh")
