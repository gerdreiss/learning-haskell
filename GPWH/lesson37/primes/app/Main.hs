module Main where

import           Primes

main :: IO ()
main = do
  putStrLn "Enter a number to check if it's prime:"
  n <- readLn :: IO Int
  putStrLn "Checking entered number..."
  if n < 2 || n > maximum primes
    then putStrLn "Sorry, this number is not a valid candidate for primality testing"
    else if isPrime n == Just True
           then putStrLn "It is prime!"
           else putStrLn "It is not prime."
