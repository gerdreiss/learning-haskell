module Main where

import           Primes

main0 :: IO ()
main0 = do
  putStrLn "Enter a number to check if it's prime:"
  n <- readLn :: IO Int
  putStrLn "Checking entered number..."
  if n < 2 || n > maximum primes
    then putStrLn "Sorry, this number is not a valid candidate for primality testing"
    else if isPrime0 n == Just True
           then putStrLn "It is prime!"
           else putStrLn "It is not prime."

main :: IO ()
main = do
  putStrLn "Enter a number to test for primality:"
  n <- readLn :: IO Int
  -- n <- read <$> getLine
  putStrLn "Checking entered number..."
  let result = isPrime n
  print (displayResult result)
