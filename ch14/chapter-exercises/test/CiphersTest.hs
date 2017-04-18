module CiphersTest where

import Test.QuickCheck
import Ciphers (caesar, unCaesar)


genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

ciphersTest :: IO ()
ciphersTest = do
  quickCheck ceasarRoundtrip

ceasarRoundtrip :: String -> Int -> Bool
ceasarRoundtrip s i = (unCaesar (caesar s i) i) == s
-- how do I get genSafeChar into this?
