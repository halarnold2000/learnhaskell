module Cipher
   (
     cipher
   , unCipher
   , cipherChar
   ) where

import Data.Char (ord, chr)

-- offset :: Int
spaceVal = ord ' '

cipher :: Int -> String -> String
cipher offset str = map (cipherChar offset) str

cipherChar :: Int -> Char -> Char
cipherChar shift c =
   let encodeChar = ord c
   in chr $ (shift + encodeChar) `mod` 121

unCipher :: Int -> String -> String
unCipher offset str = cipher (- offset) str
