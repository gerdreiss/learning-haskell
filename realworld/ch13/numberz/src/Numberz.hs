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

{- Showing a SymbolicManip calls the prettyShow function on it -}
instance (Show a, Num a) => Show (SymbolicManipulation a) where
   show a = prettyShow a

{- New data type: Units.  A Units type contains a number
and a SymbolicManip, which represents the units of measure.
A simple label would be something like (Symbol "m") -}
data Num a => Units a = Units a (SymbolicManipulation a)
           deriving (Eq)

{- Make Units an instance of Fractional -}
instance (Fractional a) => Fractional (Units a) where
   (Units xa ua) / (Units xb ub) = Units (xa / xb) (ua / ub)
   recip a = 1 / a
   fromRational r = Units (fromRational r) (Number 1)

{- 
   Floating implementation for Units.
   Use some intelligence for angle calculations: support deg and rad
-}
instance (Floating a) => Floating (Units a) where
   pi = (Units pi (Number 1))
   exp _ = error "exp not yet implemented in Units"
   log _ = error "log not yet implemented in Units"
   (Units xa ua) ** (Units xb ub) 
       | ub == Number 1 = Units (xa ** xb) (ua ** Number xb)
       | otherwise = error "units for RHS of ** not supported"
   sqrt (Units xa ua) = Units (sqrt xa) (sqrt ua)
   sin (Units xa ua) 
       | ua == Symbol "rad" = Units (sin xa) (Number 1)
       | ua == Symbol "deg" = Units (sin (deg2rad xa)) (Number 1)
       | otherwise = error "Units for sin must be deg or rad"
   cos (Units xa ua) 
       | ua == Symbol "rad" = Units (cos xa) (Number 1)
       | ua == Symbol "deg" = Units (cos (deg2rad xa)) (Number 1)
       | otherwise = error "Units for cos must be deg or rad"
   tan (Units xa ua)
       | ua == Symbol "rad" = Units (tan xa) (Number 1)
       | ua == Symbol "deg" = Units (tan (deg2rad xa)) (Number 1)
       | otherwise = error "Units for tan must be deg or rad"
   asin (Units xa ua) 
       | ua == Number 1 = Units (rad2deg $ asin xa) (Symbol "deg")
       | otherwise = error "Units for asin must be empty"
   acos (Units xa ua)
       | ua == Number 1 = Units (rad2deg $ acos xa) (Symbol "deg")
       | otherwise = error "Units for acos must be empty"
   atan (Units xa ua)
       | ua == Number 1 = Units (rad2deg $ atan xa) (Symbol "deg")
       | otherwise = error "Units for atan must be empty"
   sinh = error "sinh not yet implemented in Units"
   cosh = error "cosh not yet implemented in Units"
   tanh = error "tanh not yet implemented in Units"
   asinh = error "asinh not yet implemented in Units"
   acosh = error "acosh not yet implemented in Units"
   atanh = error "atanh not yet implemented in Units"

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

{- Show a SymbolicManipulation using RPN.  HP calculator users may
find this familiar. -}
rpnShow :: (Show a, Num a) => SymbolicManipulation a -> String
rpnShow i =
    let toList (Number x) = [show x]
        toList (Symbol x) = [x]
        toList (BinaryArithmetic op a b) = toList a ++ toList b ++
           [op2str op]
        toList (UnaryArithmetic op a) = toList a ++ [op]
        join :: [a] -> [[a]] -> [a]
        join delim l = concat (intersperse delim l)
    in join " " (toList i)

{- Perform some basic algebraic simplifications on a SymbolicManip. -}
simplify :: (Num a) => SymbolicManipulation a -> SymbolicManipulation a
simplify (BinaryArithmetic op ia ib) = 
    let sa = simplify ia
        sb = simplify ib
        in
        case (op, sa, sb) of 
                (Mult, Number 1, b) -> b
                (Mult, a, Number 1) -> a
                (Mult, Number 0, b) -> Number 0
                (Mult, a, Number 0) -> Number 0
                (Div, a, Number 1) -> a
                (Plus, a, Number 0) -> a
                (Plus, Number 0, b) -> b
                (Minus, a, Number 0) -> a
                _ -> BinaryArithmetic op sa sb
simplify (UnaryArithmetic op a) = UnaryArithmetic op (simplify a)
simplify x = x
