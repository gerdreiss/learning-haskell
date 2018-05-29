data Expression = Number Int
                | Add Expression Expression
                | Subtract Expression Expression
                deriving (Eq, Ord, Show)

calculate :: Expression -> Int
calculate (Number a) = a
calculate (Add a b) = (calculate a) + (calculate b)
calculate (Subtract a b) = (calculate a) - (calculate b)
