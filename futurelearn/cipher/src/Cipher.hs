{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

-- Caesar's Cipher example
module Cipher
  ( cipher
  , decipher
  ) where


import           Data.Char

-- is this a letter to be ciphered?
shouldcipher :: Char -> Bool
shouldcipher c = isLetter(c) && isAscii(c)

-- enciphers single char at a time
cipherchar :: Int -> Char -> Char
cipherchar shift c
  | shouldcipher c = chr(ord(c) + shift)
  | otherwise      = c

-- encipher the whole string
cipher :: Int -> [Char] -> [Char]
cipher shift plaintext = map (cipherchar shift) plaintext

-- decipher the whole string
decipher :: Int -> [Char] -> [Char]
decipher shift ciphertext = cipher (-shift) ciphertext
