{-# LANGUAGE OverloadedStrings #-}
module Cryptography where

import           Crypto.Hash
import           Crypto.PubKey.ECC.ECDSA
import           Crypto.PubKey.ECC.Generate
import           Crypto.PubKey.ECC.Types
import           Data.ByteString            (ByteString)

p256 :: Curve
p256 = getCurveByName SEC_p256r1

text :: ByteString
text = "test"

test :: IO ()
test = do
  (pubKey, privKey) <- generate p256
  print pubKey
  print privKey
  sig <- sign privKey SHA3_512 text
  print sig
  let valid = verify SHA3_512 pubKey sig text
  if valid then
    putStrLn "Works."
  else
    putStrLn "Failed."
  return ()
