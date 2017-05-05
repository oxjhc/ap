{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Verification(
    valid,
    genSignedToken,
    checkVaultSignature
) where

import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Crypto.Hash.Algorithms
import           Crypto.MAC.HMAC
import           Crypto.PubKey.ECC.DH
import           Crypto.PubKey.ECC.ECDSA
import           Crypto.Random
import           Data.Binary.Builder.Sized
import           Data.ByteArray            (convert)
import qualified Data.ByteString           as SBS
import qualified Data.ByteString.Lazy      as LBS
import           Data.ProtocolBuffers
import           Data.Serialize.Get
import           Data.Serialize.Put        (runPut)

import           Cryptography
import           Number
import           ProtoBuf                  hiding (Vault)
import           Vault


valid :: LocnProof -> Vault PrimeField -> LBS.ByteString -> IO (Maybe (Polynomial PrimeField))
valid m3 vault sig' = (\vPrivKey ->
    let key' = getField $ vault_key m3
        apKey' = LBS.fromStrict $ getField $ ekey (m3 :: LocnProof)
        key = (\apk -> maybeCryptoError $ decodeVaultKey <$> decrypt apk vPrivKey key') =<< apKey
        apKey = either (const Nothing) Just $ parsePubKey apKey'
        locnTag = openVault vault <$> key
        hLocnTagM3 = (flip hmac (encode' m3) <$> encodePFs <$> unPoly <$> locnTag) :: Maybe (HMAC SHA256)
        storedSig = either (const Nothing) Just $ parseSig sig'
    in if maybe False id $ verify SHA256 <$> apKey <*> storedSig <*> hLocnTagM3
        then locnTag
        else Nothing
    ) <$> getPrivKey

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

decrypt :: PublicKey -> PrivateKey -> SBS.ByteString -> CryptoFailable SBS.ByteString
decrypt pubKey privKey cipherText = flip (flip cbcDecrypt nullIV) cipherText <$> cipher
    where sharedSecret = getShared (private_curve privKey) (private_d privKey) (public_q pubKey)
          bsSharedSecret = convert sharedSecret :: SBS.ByteString
          cipher = cipherInit bsSharedSecret :: CryptoFailable AES128


checkVaultSignature :: VaultMsg -> LBS.ByteString -> Bool
checkVaultSignature vaultMsg sigBS = Just True == do
    let fromEither = either (const Nothing) Just
    apKey <- fromEither $ parsePubKey $ LBS.fromStrict $ getField $ apid (vaultMsg :: VaultMsg)
    sig <- fromEither $ parseSig sigBS
    return $ verify SHA256 apKey sig $ encode' $ vaultMsg
