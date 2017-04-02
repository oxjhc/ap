--Generic functions for converting between the types for which Encode and Decode can be derived, and more friendly types.
--Writing this was perhaps over the top, but it was more interesting than just writing all of the instances individually.
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module ProtoBufConverter(
    encodeGen,
    decodeGen,
) where

import GHC.Generics
import Data.ProtocolBuffers
import Data.ProtocolBuffers.Internal (Tag, WireField)
import Data.Binary.Builder.Sized (Builder)
import Data.Binary.Get (Get)
import Data.Text
import Data.HashMap.Strict (HashMap)

class CodableEquivalent a b where
    toCodable :: b -> a
    fromCodable :: a -> b

class Wrapper w u where
    wrap   :: u -> w
    unwrap :: w -> u

instance Wrapper (Fixed x) x where
    wrap = Fixed
    unwrap (Fixed x) = x
instance Wrapper Text String where
    wrap = pack
    unwrap = unpack
instance Wrapper x x where
    wrap = id
    unwrap = id

instance (HasField a, b' ~ FieldType a, Wrapper b' b) => CodableEquivalent (Rec0 a p) (Rec0 b p) where
    toCodable   = K1 . putField .   wrap . unK1
    fromCodable = K1 . unwrap . getField . unK1

instance (CodableEquivalent (a p) (b p), CodableEquivalent (a' p) (b' p)) => CodableEquivalent ((a :*: a') p) ((b :*: b') p) where
    toCodable   (x :*: x') =   toCodable x :*:   toCodable x'
    fromCodable (x :*: x') = fromCodable x :*: fromCodable x'

instance (CodableEquivalent (a p) (b p), CodableEquivalent (a' p) (b' p)) => CodableEquivalent ((a :+: a') p) ((b :+: b') p) where
    toCodable   (L1 x) = L1 $   toCodable x
    toCodable   (R1 x) = R1 $   toCodable x
    fromCodable (L1 x) = L1 $ fromCodable x
    fromCodable (R1 x) = R1 $ fromCodable x

instance (CodableEquivalent (a p) (b p)) => CodableEquivalent (M1 i c a p) (M1 i' c' b p) where
    toCodable   = M1 .   toCodable . unM1
    fromCodable = M1 . fromCodable . unM1

encodeGen :: (Generic a, Generic b, CodableEquivalent (Rep a ()) (Rep b ()), Encode a) => a -> b -> Builder
encodeGen a = encode . flip asTypeOf a . (to :: Generic a => Rep a () -> a) . toCodable . (from :: Generic b => b -> Rep b ())
decodeGen :: (Generic a, Generic b, CodableEquivalent (Rep a ()) (Rep b ()), Decode a) => a -> HashMap Tag [WireField] -> Get b
decodeGen a = fmap ((to :: Generic b => Rep b () -> b) . fromCodable . (from :: Generic a => a -> Rep a ()) . flip asTypeOf a) . decode
