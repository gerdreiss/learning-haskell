module Numberz where

data Op = Plus | Minus | Mult | Div | Pow
  deriving (Eq, Show)

{-
   The core symbolic manipulation type.
   It can be a simple number, a symbol, a binary arithmetic operation (such as +),
   or a unary arithmetic operation (such as cos)

   Notice the types of BinaryArith and UnaryArith: it's a recursive type.
   So, we could represent a (+) over two SymbolicManips.
-}
data SymbolicManipulation a =
   Number a           -- Simple number, such as 5
 | Symbol String      -- A symbol, such as *
 | BinaryArithmetic Op (SymbolicManipulation a) (SymbolicManipulation a)
 | UnaryArithmetic String (SymbolicManipulation a)
   deriving (Eq)

{-
   SymbolicManipulation will be an instance of Num.
   Define how the Num operations are handled over a SymbolicManipulation.
   This will implement things like (+) for SymbolicManipulation.
-}
instance Num a => Num (SymbolicManipulation a) where
  a + b         = BinaryArithmetic Plus a b
  a - b         = BinaryArithmetic Minus a b
  a * b         = BinaryArithmetic Mult a b
  negate a      = BinaryArithmetic Mult (Number (-1)) a
  abs a         = UnaryArithmetic "abs" a
  signum _      = error "signum is not implemented"
  fromInteger i = Number (fromInteger i)


{- Make SymbolicManip an instance of Fractional -}
instance (Fractional a) => Fractional (SymbolicManipulation a) where
   a / b          = BinaryArithmetic Div a b
   recip a        = BinaryArithmetic Div (Number 1) a
   fromRational r = Number (fromRational r)

{- Make SymbolicManip an instance of Floating -}
instance (Floating a) => Floating (SymbolicManipulation a) where
   pi      = Symbol "pi"
   exp a   = UnaryArithmetic "exp" a
   log a   = UnaryArithmetic "log" a
   sqrt a  = UnaryArithmetic "sqrt" a
   a ** b  = BinaryArithmetic Pow a b
   sin a   = UnaryArithmetic "sin" a
   cos a   = UnaryArithmetic "cos" a
   tan a   = UnaryArithmetic "tan" a
   asin a  = UnaryArithmetic "asin" a
   acos a  = UnaryArithmetic "acos" a
   atan a  = UnaryArithmetic "atan" a
   sinh a  = UnaryArithmetic "sinh" a
   cosh a  = UnaryArithmetic "cosh" a
   tanh a  = UnaryArithmetic "tanh" a
   asinh a = UnaryArithmetic "asinh" a
   acosh a = UnaryArithmetic "acosh" a
   atanh a = UnaryArithmetic "atanh" a

{- Show a SymbolicManip as a String, using conventional algebraic notation -}
prettyShow :: (Show a, Num a) => SymbolicManipulation a -> String

-- Show a number or symbol as a bare number or serial
prettyShow (Number x) = show x
prettyShow (Symbol x) = x
prettyShow (BinaryArithmetic op a b) =
    let pa  = simpleParen a
        pb  = simpleParen b
        pop = op2str op
        in pa ++ pop ++ pb
prettyShow (UnaryArithmetic opstr a) =
    opstr ++ "(" ++ show a ++ ")"

op2str :: Op -> String
op2str Plus  = "+"
op2str Minus = "-"
op2str Mult  = "*"
op2str Div   = "/"
op2str Pow   = "**"

{-
   Add parenthesis where needed.
   This function is fairly conservative and will add parenthesis
   when not needed in some cases.

   Haskell will have already figured out precedence for us
   while building up the SymbolicManipulation.
-}
simpleParen :: (Show a, Num a) => SymbolicManipulation a -> String
simpleParen (Number x)                 = prettyShow (Number x)
simpleParen (Symbol x)                 = prettyShow (Symbol x)
simpleParen x@(BinaryArithmetic _ _ _) = "(" ++ prettyShow x ++ ")"
simpleParen x@(UnaryArithmetic _ _)    = prettyShow x

{- Showing a SymbolicManip calls the prettyShow function on it -}
instance (Show a, Num a) => Show (SymbolicManipulation a) where
    show a = prettyShow a
