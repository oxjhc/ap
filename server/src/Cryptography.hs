{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Cryptography where

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
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import           Data.PEM

p256 :: Curve
p256 = getCurveByName SEC_p256r1

text :: BS.ByteString
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

-- RFC5480 excerpt:
--
-- The ASN1 format for the p ublic key is:
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
parsePubKey :: ParseASN1 PublicKey
parsePubKey = do
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
parsePrivKey :: ParseASN1 PrivateKey
parsePrivKey = do
  getNext -- Start Sequence
  getNext -- version
  privNum <- getNext >>= \case
    OctetString b -> return $ os2ip b
    _ -> throwParseError "Expecting octet string"
  getMany getNext-- throw rest away
  return (PrivateKey p256 privNum)

readPubKey :: IO ()
readPubKey = do
  pubKey <- LBS.readFile "ecpubkey.der"
  case decodeASN1 DER pubKey of
    Left _ -> putStrLn "DER parse error."
    Right asn1s -> do
      case runParseASN1 parsePubKey asn1s of
        Left err  -> putStrLn $ "ASN1 parse error: " ++ err
        Right pub -> do
          putStrLn ("Got: " ++ show pub)
          putStrLn $
            "Checking roundtrip: " ++
            if encodePubKey pub == pubKey then "Success!" else "Failure."

readPrivKey :: IO ()
readPrivKey = do
  privKey <- LBS.readFile "prime256v1-key.pem"
  case pemParseLBS privKey of
    Left _ -> putStrLn "PEM parse error."
    Right [] -> putStrLn "No PEMs."
    Right (pem:_) -> do
      case decodeASN1 DER (LBS.fromStrict $ pemContent pem) of
        Left _ -> putStrLn "DER parse error."
        Right asn1s -> do
          case runParseASN1 parsePrivKey asn1s of
            Left err   -> putStrLn $ "ASN1 parse error: " ++ err
            Right priv -> putStrLn ("Got: " ++ show priv)
