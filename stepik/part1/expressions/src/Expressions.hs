module Expressions (Expr(..), expand, expand0) where

infixl 6 :+:
infixl 7 :*:

data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)


expand :: Expr -> Expr
expand exp@((e1 :+: e2) :*: e) = helper exp (expand e1 :*: expand e :+: expand e2 :*: expand e)
expand exp@(e :*: (e1 :+: e2)) = helper exp (expand e :*: expand e1 :+: expand e :*: expand e2)
expand exp@(e1 :+: e2)         = helper exp (expand e1 :+: expand e2)
expand exp@(e1 :*: e2)         = helper exp (expand e1 :*: expand e2)
expand e                       = e

helper exp def = if exp == def then def else expand def


-- more elegant
expand0 :: Expr -> Expr
expand0 = foldr1 (:+:) . expandList
  where
    expandList :: Expr -> [Expr]
    expandList (Val i)   = [Val i]
    expandList (l :+: r) = expandList l ++ expandList r
    expandList (l :*: r) = [ e1 :*: e2 | e1 <- expandList l, e2 <- expandList r]
