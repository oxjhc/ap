{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Cryptography where

import           Control.Monad
import           Control.Monad.Loops
import           Crypto.Hash
import           Crypto.Number.Serialize
import           Crypto.PubKey.ECC.ECDSA
import           Crypto.PubKey.ECC.Generate
import           Crypto.PubKey.ECC.Types
import           Data.ASN1.BinaryEncoding
import           Data.ASN1.BitArray
import           Data.ASN1.Encoding
import           Data.ASN1.Parse
import           Data.ASN1.Types
import           Data.Bifunctor             (first)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import           Data.PEM

import           ProtoBuf                   (VaultMsg)

-- OpenSSl notes:
--
-- Make private key:
-- openssl ecparam -name prime256v1 -genkey -noout -out prime256v1-key.pem
--
-- Extract public key from private key:
-- openssl ec -in prime256v1-key.pem -pubout -outform DER -out ecpubkey.der
--
-- Inspect a PEM:
-- openssl asn1parse -inform PEM -in prime256v1-key.pem
--
-- Inspect a DER:
-- openssl asn1parse -inform DER -in ecpubkey.der
--
-- Sign:
-- openssl dgst -sha256 -sign prime256v1-key.pem test.txt > sign.bin

p256 :: Curve
p256 = getCurveByName SEC_p256r1

text :: BS.ByteString
text = "test"

test :: IO ()
test = do
  (pubKey, privKey) <- generate p256
  print pubKey
  print privKey
  sig <- sign privKey SHA256 text
  print sig
  let valid = verify SHA256 pubKey sig text
  if valid then
    putStrLn "Works."
  else
    putStrLn "Failed."
  return ()

-- RFC5480 excerpt:
--
-- The ASN1 format for the public key is:
--
-- SubjectPublicKeyInfo  ::=  SEQUENCE  {
--        algorithm         AlgorithmIdentifier,
--        subjectPublicKey  BIT STRING
--      }
--
-- AlgorithmIdentifier  ::=  SEQUENCE  {
--         algorithm   OBJECT IDENTIFIER,
--         parameters  ANY DEFINED BY algorithm OPTIONAL
--       }
--
-- For the prime256v1 algoritm, parameters is OID for prime256v1.
--
-- The acutal public key is the public point, defined as
--
-- ECPoint ::= OCTET STRING
--
-- It is taken from the subjectPublicKey:
--
-- * The elliptic curve public key (a value of type ECPoint that is
--   an OCTET STRING) is mapped to a subjectPublicKey (a value of
--   type BIT STRING) as follows: the most significant bit of the
--   OCTET STRING value becomes the most significant bit of the BIT
--   STRING value, and so on; the least significant bit of the OCTET
--   STRING becomes the least significant bit of the BIT STRING.
--   Conversion routines are found in Sections 2.3.1 and 2.3.2 of
--   [SEC1].
--
-- * The first octet of the OCTET STRING indicates whether the key is
--   compressed or uncompressed.  The uncompressed form is indicated
--   by 0x04 and the compressed form is indicated by either 0x02 or
--   0x03 (see 2.3.3 in [SEC1]).  The public key MUST be rejected if
--   any other value is included in the first octet.
pubKeyParser :: ParseASN1 PublicKey
pubKeyParser = do
  getNext -- Start Sequence for SubjectPublicKeyInfo
  getNext -- Start Sequence for AlgorithmIdentifier
  getNext >>= \case
    OID [1,2,840,10045,2,1] -> return ()
    _ -> throwParseError "Expecting OID for id-ecPublicKey"
  getNext >>= \case
    OID [1,2,840,10045,3,1,7] -> return ()
    _ -> throwParseError "Expecting OID for prime256v1"
  getNext -- End Sequence for AlgorithmIdentifier
  point <- getNext >>= \case
    BitString (BitArray _ b) -> do
      let uncompressed = BS.head b == 4
          d = BS.tail b
          -- MSB encodings
          x = os2ip $ BS.take 32 d
          y = os2ip $ BS.drop 32 d
      if uncompressed then
        return (Point x y)
      else
        throwParseError "Can only parse uncompressed ECPoints"
    _ -> throwParseError "Error parsing ECPoint"
  getNext -- End Sequence for SubjectPublicKeyInfo
  return (PublicKey p256 point)

pubKeyParserLax :: ParseASN1 PublicKey
pubKeyParserLax = do
  getNext -- Start Sequence for SubjectPublicKeyInfo
  getNext -- Start Sequence for AlgorithmIdentifier
  getNext >>= \case
    OID [1,2,840,10045,2,1] -> return ()
    _ -> throwParseError "Expecting OID for id-ecPublicKey"
  _:end:_ <- reverse <$> hasNext `whileM` getNext
  point <- case end of
    BitString (BitArray _ b) -> do
      let uncompressed = BS.head b == 4
          d = BS.tail b
          -- MSB encodings
          x = os2ip $ BS.take 32 d
          y = os2ip $ BS.drop 32 d
      if uncompressed then
        return (Point x y)
      else
        throwParseError "Can only parse uncompressed ECPoints"
    _ -> throwParseError "Error parsing ECPoint"
  return (PublicKey p256 point)

encodePubKey :: PublicKey -> LBS.ByteString
encodePubKey pk = encodeASN1 DER asn1
  where
    (PublicKey _ (Point x y)) = pk
    -- MSB encodings
    bx = i2osp x
    by = i2osp y
    asn1 =
      [ Start Sequence -- SubjectPublicKeyInfo
      , Start Sequence -- AlgorithmIdentifier
      , OID [1,2,840,10045,2,1]
      , OID [1,2,840,10045,3,1,7]
      , End Sequence -- AlgorithmIdentifier
      -- BitArray has number of bits
      , BitString (BitArray ((1+32+32)*8) (BS.concat [BS.singleton 4, bx, by]))
      , End Sequence -- SubjectPublicKeyInfo
      ]

-- RFC5915 excerpt:
--
-- ECPrivateKey ::= SEQUENCE {
--   version        INTEGER { ecPrivkeyVer1(1) } (ecPrivkeyVer1),
--   privateKey     OCTET STRING,
--   parameters [0] ECParameters {{ NamedCurve }} OPTIONAL,
--   publicKey  [1] BIT STRING OPTIONAL
-- }
privKeyParser :: ParseASN1 PrivateKey
privKeyParser = do
  getNext -- Start Sequence
  getNext -- version
  privNum <- getNext >>= \case
    OctetString b -> return $ os2ip b
    _ -> throwParseError "Expecting octet string"
  getMany getNext-- throw rest away
  return (PrivateKey p256 privNum)

-- RFC5480 excerpt
--
-- ECDSA-Sig-Value ::= SEQUENCE {
--   r  INTEGER,
--   s  INTEGER
-- }
--
-- Originally in ANSI X9.62-1998
sigParser :: ParseASN1 Signature
sigParser = do
  getNext -- Start Sequence
  r <- getNext >>= \case
    IntVal r -> return r
    _ -> throwParseError "Expecting integer"
  s <- getNext >>= \case
    IntVal s -> return s
    _ -> throwParseError "Expecting integer"
  getNext -- End Sequence
  return (Signature r s)

encodeSig :: Signature -> LBS.ByteString
encodeSig sig = encodeASN1 DER asn1
  where
    (Signature r s) = sig
    asn1 =
      [ Start Sequence
      , IntVal r
      , IntVal s
      , End Sequence
      ]

parsePubKey :: LBS.ByteString -> Either String PublicKey
parsePubKey = (first ("ASN1 parse error: " ++ ) . runParseASN1 pubKeyParser)
          <=< (first (const "DER parse error.") . decodeASN1 DER)

parsePubKeyLax :: LBS.ByteString -> Either String PublicKey
parsePubKeyLax = (first ("ASN1 parse error: " ++ ) . runParseASN1 pubKeyParserLax)
             <=< (first (const "DER parse error.") . decodeASN1 DER)

parsePrivKey :: LBS.ByteString -> Either String PrivateKey
parsePrivKey = (first ("ASN1 parse error: " ++ ) . runParseASN1 privKeyParser)
           <=< (first (const "DER parse error.") . decodeASN1 DER . LBS.fromStrict . pemContent)
           <=< ((foldr ((Right .) . const) (Left "No PEMs.") =<<)
            .  (first (const "PEM parse error.") . pemParseLBS))

parseSig :: LBS.ByteString -> Either String Signature
parseSig = (first ("ASN1 parse error: " ++ ) . runParseASN1 sigParser)
       <=< (first (const "DER parse error.") . decodeASN1 DER)


readPubKey :: IO ()
readPubKey = do
  pubKey <- LBS.readFile "ecpubkey.der"
  case parsePubKey pubKey of
    Left err  -> putStrLn err
    Right pub -> do
      putStrLn ("Got: " ++ show pub)
      putStrLn $
        "Checking roundtrip: " ++
        if encodePubKey pub == pubKey then "Success!" else "Failure."

readPrivKey :: IO ()
readPrivKey = do
  privKey <- LBS.readFile "prime256v1-key.pem"
  case parsePrivKey privKey of
    Left  err  -> putStrLn err
    Right priv -> putStrLn ("Got: " ++ show priv)

readSig :: IO ()
readSig = do
  signature <- LBS.readFile "sign.bin"
  case parseSig signature of
    Left err  -> putStrLn $ "ASN1 parse error: " ++ err
    Right sig -> do
      putStrLn ("Got: " ++ show sig)
      putStrLn $
        "Checking roundtrip: " ++
        if encodeSig sig == signature then "Success!" else "Failure."
