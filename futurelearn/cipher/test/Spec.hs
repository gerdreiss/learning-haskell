{-# LANGUAGE TemplateHaskell #-}

import           Cipher
import           Test.QuickCheck

testcipher = (\n -> (\s -> ((decipher n (cipher n s)) == s))) :: Int -> [Char] -> Bool

main :: IO ()
main = quickCheck testcipher
