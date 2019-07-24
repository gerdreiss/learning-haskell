module ShowExpr where

data Expr a
  = Lit a
  | Sub (Expr a) (Expr a)
  | Add (Expr a) (Expr a)
  | Mult (Expr a) (Expr a)

instance Show a => Show (Expr a) where
  showsPrec _ (Lit a)      = shows a
  showsPrec p (Sub e1 e2)  = showParen (p > 5) (showOp 5 " - " e1 e2)
  showsPrec p (Add e1 e2)  = showParen (p > 5) (showOp 5 " + " e1 e2)
  showsPrec p (Mult e1 e2) = showParen (p > 6) (showOp 6 " * " e1 e2)

showOp :: Show a => Int -> String -> a -> a -> ShowS
showOp prec op e1 e2 =
  showsPrec prec e1 . -- print first expression
  showString op . -- print operator
  showsPrec prec e2 -- print second expression
