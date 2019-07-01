module Primes where

data PrimeError
  = TooLarge
  | InvalidValue

instance Show PrimeError where
  show TooLarge     = "Value exceed max bound"
  show InvalidValue = "Value is not a valid candidate for prime checking"

maxN :: Int
maxN = 10000

primes :: [Int]
primes = sieve [2 .. 10000]

isPrime0 :: Int -> Maybe Bool
isPrime0 n
  | n < 2 = Nothing
  | n >= length primes = Nothing
  | otherwise = Just (n `elem` primes)

isPrime :: Int -> Either PrimeError Bool
isPrime n
  | n < 2 = Left InvalidValue
  | n > maxN = Left TooLarge
  | otherwise = Right (n `elem` primes)

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
  where
    noFactors = filter ((/= 0) . (`mod` nextPrime)) rest

unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 [] = []
unsafePrimeFactors n [] = []
unsafePrimeFactors n (next:primes) =
  if n `mod` next == 0
    then next : unsafePrimeFactors (n `div` next) (next : primes)
    else unsafePrimeFactors n primes

primeFactors :: Int -> Maybe [Int]
primeFactors n
  | n < 2 = Nothing
  | n >= length primes = Nothing
  | otherwise = Just (unsafePrimeFactors n primesLessThanN)
  where
    primesLessThanN = filter (<= n) primes

displayResult :: Either PrimeError Bool -> String
displayResult (Right True)      = "It's prime"
displayResult (Right False)     = "It's composite"
displayResult (Left primeError) = show primeError
