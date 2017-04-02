{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module ProtoBuf where

import           Data.ByteString.Lazy      (fromStrict, toStrict)
import           Data.Binary.Builder.Sized
import           Data.Binary.Get
import           Data.Hex
import           Data.Int
import           Data.ProtocolBuffers
import           Data.Serialize            hiding (encode, decode)
import           Data.Text                 (Text)
import           Data.Word
import           GHC.Generics              (Generic)
import           Network.HTTP.Media
import           Servant                   hiding (Vault)
import           ProtoBufConverter
import           Number
import           Vault

{-
message VaultMsg {
	message Vault {
		message Point {
			required uint32 x = 1;
			required uint32 y = 2;
		}
		repeated Point points = 1;
	}
	required Vault vault = 1;
	required bytes uid = 2;
	required bytes unonce = 3;
	required bytes apid = 4;
	required bytes apnonce = 5;
	required fixed64 time = 6;
	required bytes sig = 7;
}
-}

data Point = Point
  { x :: Required 1 (Enumeration PrimeField)
  , y :: Required 2 (Enumeration PrimeField)
  } deriving (Generic, Show, Eq)

instance Encode Point
instance Decode Point

data Vault' = Vault'
  { points :: Repeated 1 (Message Point)
  } deriving (Generic, Show, Eq)

instance Encode Vault'
instance Decode Vault'

instance Encode (Vault PrimeField) where
    encode (Vault ps) = encode $ Vault' $ putField $ flip map ps $ (\(x, y) -> Point (putField x) (putField y))
instance Decode (Vault PrimeField) where
    decode = fmap (Vault . map (\(Point x y) -> (getField x, getField y)) . getField . points) . decode


data VaultMsg' = VaultMsg'
  { vault   :: Required 1 (Message (Vault PrimeField))
  , uid     :: Required 2 (Value Text)
  , unonce  :: Required 3 (Value Text)
  , apid    :: Required 4 (Value Text)
  , apnonce :: Required 5 (Value Text)
  , time    :: Required 6 (Value (Fixed Word64))
  , sig     :: Required 7 (Value Text)
  } deriving (Generic, Show, Eq)

data VaultMsg = VaultMsg
  { vault   :: Vault PrimeField
  , uid     :: String
  , unonce  :: String
  , apid    :: String
  , apnonce :: String
  , time    :: Word64
  , sig     :: String
  } deriving (Generic, Show, Eq)

instance Encode VaultMsg'
instance Decode VaultMsg'

instance Encode VaultMsg where encode = encodeGen (undefined :: VaultMsg')
instance Decode VaultMsg where decode = decodeGen (undefined :: VaultMsg')

{-
message LocnProof {
	required bytes vault_key = 1;
	required bytes uid = 2;
	required bytes unonce = 3;
	required bytes apid = 4;
	required bytes apnonce = 5;
	required fixed64 time = 6;
	required bytes sig = 7;
}
-}

data LocnProof' = LocnProof'
  { vault_key :: Required 1 (Value Text)
  , uid       :: Required 2 (Value Text)
  , unonce    :: Required 3 (Value Text)
  , apid      :: Required 4 (Value Text)
  , apnonce   :: Required 5 (Value Text)
  , time      :: Required 6 (Value (Fixed Word64))
  , sig       :: Required 7 (Value Text)
  } deriving (Generic, Show, Eq)

data LocnProof = LocnProof
  { vault_key :: String
  , uid       :: String
  , unonce    :: String
  , apid      :: String
  , apnonce   :: String
  , time      :: Word64
  , sig       :: String
  } deriving (Generic, Show, Eq)

instance Encode LocnProof'
instance Decode LocnProof'

instance Encode LocnProof where encode = encodeGen (undefined :: LocnProof')
instance Decode LocnProof where decode = decodeGen (undefined :: LocnProof')

{-
message Token {
	required bytes vnonce = 1;
	required bytes locn_tag = 2;
}
-}

data Token' = Token'
  { vnonce   :: Required 1 (Value Text)
  , locn_tag :: Required 2 (Value Text)
  } deriving (Generic, Show, Eq)

data Token = Token
  { vnonce   :: String
  , locn_tag :: String
  } deriving (Generic, Show, Eq)

instance Encode Token'
instance Decode Token'

instance Encode Token where encode = encodeGen (undefined :: Token')
instance Decode Token where decode = decodeGen (undefined :: Token')

data ProtoBuf

instance Servant.Accept ProtoBuf where
  contentType _ = "application" // "x-protobuf"
instance Encode a => MimeRender ProtoBuf a where
  mimeRender _ = fmap hex toLazyByteString . encodeMessage
instance Decode a => MimeUnrender ProtoBuf a where
  mimeUnrender _ bs =
    case runGetOrFail decodeMessage =<< unhex bs of
      Left (_, _, err)  -> Left err
      Right (_, _, res) -> Right res
