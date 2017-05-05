{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Verification(
    valid,
    genSignedToken,
    checkVaultSignature
) where

import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Crypto.Hash.Algorithms
import           Crypto.KDF.PBKDF2
import           Crypto.MAC.HMAC
import           Crypto.PubKey.ECC.DH
import           Crypto.PubKey.ECC.ECDSA
import           Crypto.Random
import           Data.Binary.Builder.Sized
import           Data.ByteArray            (convert)
import qualified Data.ByteString           as SBS
import qualified Data.ByteString.Lazy      as LBS
import           Data.Hex
import           Data.ProtocolBuffers
import           Data.Serialize.Get
import           Data.Serialize.Put        (runPut)

import           Cryptography
import           Number
import           ProtoBuf                  hiding (Vault)
import           Vault


valid :: LocnProof -> Vault PrimeField -> LBS.ByteString -> IO (Maybe (Polynomial PrimeField))
valid m3 vault sig' = do
  vPrivKey <- getPrivKey
  let key' = getField $ vault_key m3
      apn = getField $ apnonce (m3 :: LocnProof)
      apKey' = LBS.fromStrict $ getField $ ekey (m3 :: LocnProof)
      apKey = either (const Nothing) Just $ parsePubKeyLax apKey'
      a = maybe undefined id apKey
  LBS.writeFile "temp.der" apKey'
  putStrLn (show $ parsePubKeyLax apKey')
  let sharedSecret = getShared (private_curve vPrivKey) (private_d vPrivKey) (public_q a)
      bsSharedSecret = convert sharedSecret :: SBS.ByteString
      pbkSharedSecret = fastPBKDF2_SHA256 Parameters{iterCounts = 30, outputLength = 32} bsSharedSecret apn :: SBS.ByteString
      key = do
        ak <- apKey
        dec <- maybeCryptoError $ decrypt apn ak vPrivKey key'
        return (decodeVaultKey dec)
  putStrLn (show sharedSecret)
  putStrLn (show $ hex bsSharedSecret)
  putStrLn (show $ hex pbkSharedSecret)
  putStrLn (show (hex <$> (maybeCryptoError $ decrypt apn a vPrivKey key')))
  let locnTag = openVault vault <$> key
      hLocnTagM3 = (flip hmac (encode' m3) <$> encodePFs <$> unPoly <$> locnTag) :: Maybe (HMAC SHA256)
      storedSig = either (const Nothing) Just $ parseSig sig'
  if maybe False id $ verify SHA256 <$> apKey <*> storedSig <*> hLocnTagM3
      then return locnTag
      else return Nothing

genSignedToken :: Polynomial PrimeField -> IO SignedToken
genSignedToken locnTag = do
    nonce <- getRandomBytes 16
    let unsignedToken = Token (putField nonce) (putField $ encodePFs $ unPoly locnTag)
    vPrivKey <- getPrivKey
    signature <- sign vPrivKey SHA256 $ encode' unsignedToken
    return $ SignedToken (putField unsignedToken) (putField $ LBS.toStrict $ encodeSig signature)

-- A valid SignedToken which has this as the payload is:
-- 0A090A0461626364120130120130
dummyToken :: Token
dummyToken = Token
  { vnonce = putField "abcd"
  , locn_tag = putField "0"
  }

encodePFs :: [PrimeField] -> SBS.ByteString
encodePFs = LBS.toStrict . toLazyByteString . mconcat . map (putWord16be . toEnum . fromEnum)

decodeVaultKey :: SBS.ByteString -> [PrimeField]
decodeVaultKey = map (toEnum . fromEnum) . either (error "Parsing vault key failed.") id . runGet getWords
    -- For some reason Get is strict, so I have to do the recursion explicitly.
    where getWords = do
              e <- isEmpty
              if e then
                  return []
              else
                  (:) <$> getWord16be <*> getWords

encode' :: Encode a => a -> SBS.ByteString
encode' = LBS.toStrict . toLazyByteString . encodeMessage

getPrivKey :: IO PrivateKey
getPrivKey =  do
  privKey <- LBS.readFile "prime256v1-key.pem"
  case parsePrivKey privKey of
    Left  err  -> error "Could not find private key."
    Right priv -> return priv



decrypt :: SBS.ByteString -> PublicKey -> PrivateKey -> SBS.ByteString -> CryptoFailable SBS.ByteString
decrypt apn pubKey privKey cipherText = do
  cipher :: AES256 <- cipherInit pbkSharedSecret
  iv <- case makeIV @SBS.ByteString @AES256 "000000000000" of
    Just i  -> return i
    Nothing -> CryptoFailed CryptoError_IvSizeInvalid
  aead <- aeadInit AEAD_GCM cipher iv
  -- let (text, tag) = SBS.splitAt (SBS.length cipherText - 16) cipherText
  case aeadSimpleDecrypt aead SBS.empty (SBS.reverse . SBS.drop 16 . SBS.reverse $ cipherText) (AuthTag $ convert $ SBS.reverse . SBS.take 16 . SBS.reverse $ SBS.empty) of
    Just res -> CryptoPassed res
    Nothing  -> CryptoFailed undefined
    where
      sharedSecret = getShared (private_curve privKey) (private_d privKey) (public_q pubKey)
      bsSharedSecret = convert sharedSecret :: SBS.ByteString
      pbkSharedSecret = fastPBKDF2_SHA256 Parameters{iterCounts = 30, outputLength = 32} bsSharedSecret apn :: SBS.ByteString


checkVaultSignature :: VaultMsg -> LBS.ByteString -> Bool
checkVaultSignature vaultMsg sigBS = Just True == do
    let fromEither = either (const Nothing) Just
    apKey <- fromEither $ parsePubKey $ LBS.fromStrict $ getField $ apid (vaultMsg :: VaultMsg)
    sig <- fromEither $ parseSig sigBS
    return $ verify SHA256 apKey sig $ encode' $ vaultMsg
