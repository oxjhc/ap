-- Generic functions for converting between the types for which Encode and
-- Decode can be derived, and more friendly types. Writing this was perhaps over
-- the top, but it was more interesting than just writing all of the instances
-- individually.
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE UnicodeSyntax         #-}

module ProtoBufConverter(
    toProto,
    fromProto,
    ProtoIso,
) where

import           Data.Binary.Builder.Sized (putWord16be, toLazyByteString)
import           Data.Binary.Get           (Get, getWord16be, isEmpty, runGet)
import           Data.ByteString           (ByteString)
import           Data.ByteString.Lazy      (fromStrict, toStrict)
import           Data.HashMap.Strict       (HashMap)
import           Data.Maybe
import           Data.ProtocolBuffers
import           Data.Text                 (Text, pack, unpack)
import           GHC.Generics
import           Number
import           Vault                     (Polynomial (..))


-- Wrap/unwrap types that occur in the protobuf library to/from types that we
-- choose for ease of use.
class Wrapper w u where
    wrap   :: u → w
    unwrap :: w → u

instance Wrapper (Fixed x) x where
    wrap = Fixed
    unwrap (Fixed x) = x
instance Wrapper (Signed x) x where
    wrap = Signed
    unwrap (Signed x) = x
instance Wrapper Text String where
    wrap = pack
    unwrap = unpack
instance Wrapper ByteString [PrimeField] where
-- At this point it becomes clear that "Wrapper" was a poor choice.
    wrap = toStrict . toLazyByteString . mconcat . map (putWord16be . toEnum . fromEnum)
    unwrap = map (toEnum . fromEnum) . runGet getWords . fromStrict
        -- For some reason Get is strict, so I have to do the recursion explicitly.
        where getWords = do
                  e ← isEmpty
                  if e then
                      return []
                  else
                      (:) <$> getWord16be <*> getWords
instance Wrapper ByteString (Polynomial PrimeField) where
    wrap = wrap . unPoly
    unwrap = P . unwrap
instance Wrapper x x where
    wrap = id
    unwrap = id


-- Generically convert between the types that we decide to be equivalent. The
-- sum case a :+: b does not need to be done as a proper protobuf datatype
-- cannot have a sum.
class CodableEquivalent (ra :: ★ → ★) (rb :: ★ → ★) where
  toCodable :: rb x → ra x
  fromCodable :: ra x → rb x

instance (HasField a, b' ~ FieldType a, Wrapper b' b) ⇒
  CodableEquivalent (K1 i a) (K1 i b) where
    toCodable   = K1 . putField .   wrap . unK1
    fromCodable = K1 . unwrap . getField . unK1
instance (CodableEquivalent a b, CodableEquivalent a' b') ⇒
  CodableEquivalent (a :*: a') (b :*: b') where
    toCodable   (x :*: x') =   toCodable x :*:   toCodable x'
    fromCodable (x :*: x') = fromCodable x :*: fromCodable x'
instance (CodableEquivalent a b) ⇒
  CodableEquivalent (M1 i c a) (M1 i' c' b) where
    toCodable   = M1 .   toCodable . unM1
    fromCodable = M1 . fromCodable . unM1


-- Enabled by ConstraintKinds, for better type signatures and possible export.
type ProtoIso a b = (Generic a, Generic b, CodableEquivalent (Rep a) (Rep b))

-- To and from a protobuf and the associated type
fromProto :: ∀a b. ProtoIso a b ⇒ a → b
fromProto = to . fromCodable . from
toProto :: ∀a b. ProtoIso a b ⇒ b → a
toProto = to . toCodable . from
