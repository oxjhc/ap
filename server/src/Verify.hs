module Verify(
    checkVault
) where

import Data.Maybe

import Crypto.Saltine.Class (encode, decode)
import Crypto.Saltine.Core.Auth as Auth
import qualified Crypto.Saltine.Core.Hash as Hash
import qualified Crypto.Saltine.Internal.ByteSizes as Sizes
import Data.ByteString as ByteString
import Data.ByteString.Lazy (toStrict)
import Data.ProtocolBuffers hiding (encode, decode)
import Data.Binary.Builder.Sized hiding (append)

import ProtoBufConverter (wrap)
import ProtoBuf (LocnProof(..), extractM3)
import Vault
import Number

checkVault :: LocnProof -> Vault PrimeField -> Either String (Polynomial PrimeField)
checkVault prf v = case hashValid of
        Nothing    -> Left "Locn_proof authentication failed."
        Just False -> Left "Incorrect vault key."
        Just True  -> Right $ lt
    where lt = openVault v $ vault_key prf
          ltKey = makeHashKey $ wrap lt
          m3 = toStrict $ toLazyByteString $ encodeMessage $ extractM3 prf
          hash = encode $ auth ltKey m3
          -- Is there a standard function of type (Functor f => f (a -> b) -> a -> f b)? It would be easy enough to write, but it's weird that it's not in the Functor module.
          hashValid = verify <$> (decode $ apid prf) <*> (decode $ sig prf) <*> return hash

-- Either the writers of libsodium went too far with hiding the details thus forcing users to write their own implementation for something the library really should deal with, or this is something the user shouldn't have to deal with and the Alice paper is doing it wrong.
-- The HMAC description given on Wikipedia allows any length of key, but the implementation in Saltine only allows 1 length.
makeHashKey :: ByteString -> Auth.Key
makeHashKey s = fromJust $ decode $ if excess <= 0
    then append s $ ByteString.replicate (- excess) 0
    else ByteString.take Sizes.authKey $ Hash.hash s
        where excess = ByteString.length s - Sizes.authKey
