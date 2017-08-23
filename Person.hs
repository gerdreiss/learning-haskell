module Person where

data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName person
    | (length fn > 2) = person { firstName = (head fn) : "." }
    | otherwise       = person
    where fn = firstName person
