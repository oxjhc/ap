{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module ProtoBuf where

import           Data.Binary.Builder.Sized
import           Data.Binary.Get
import           Data.ByteString               (ByteString)
import           Data.ByteString.Lazy          (fromStrict, toStrict)
import           Data.HashMap.Strict           (HashMap)
import           Data.Hex
import           Data.Int
import           Data.ProtocolBuffers
import           Data.ProtocolBuffers.Internal (Tag, WireField)
import           Data.Serialize                hiding (decode, encode)
import           Data.Word
import           Debug.Trace
import           GHC.Generics                  (Generic)
import           Network.HTTP.Media
import           Number
import           ProtoBufConverter
import           Servant                       hiding (Vault)
import qualified Vault                         as V

-- Useful for defining the Encode and Decode instances
encodeGen :: forall a b. (ProtoIso a b, Encode a) => b -> Builder
encodeGen = encode @a . toProto @a @b
decodeGen :: forall a b. (ProtoIso a b, Decode a) =>
  HashMap Tag [WireField] -> Data.Binary.Get.Get b
decodeGen = fmap (fromProto @a @b). decode @a

{-
//m4
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
}

message SignedVaultMsg {
	required VaultMsg vault_msg = 1;
	required bytes sig = 2;
}
-}

data Point = Point
  { x :: Required 1 (Value Word32)
  , y :: Required 2 (Value Word32)
  } deriving (Generic, Show, Eq)

instance Encode Point
instance Decode Point

data Vault = Vault
  { points :: Repeated 1 (Message Point)
  } deriving (Generic, Show, Eq)

instance Encode Vault
instance Decode Vault

data VaultMsg = VaultMsg
  { vault   :: Required 1 (Message Vault)
  , uid     :: Required 2 (Value ByteString)
  , unonce  :: Required 3 (Value ByteString)
  , apid    :: Required 4 (Value ByteString)
  , apnonce :: Required 5 (Value ByteString)
  , time    :: Required 6 (Value (Fixed Word64))
  } deriving (Generic, Show, Eq)

instance Encode VaultMsg
instance Decode VaultMsg

data SignedVaultMsg = SignedVaultMsg
  { vault_msg :: Required 1 (Message VaultMsg)
  , sig       :: Required 2 (Value ByteString)
  } deriving (Generic, Show, Eq)

instance Encode SignedVaultMsg
instance Decode SignedVaultMsg

{-
// m3
message LocnProof {
	required bytes vault_key = 1;
	required bytes uid = 2;
	required bytes unonce = 3;
	required bytes apid = 4;
	required bytes apnonce = 5;
	required fixed64 time = 6;
}

message SignedLocnProof {
	required LocnProof locnproof = 1;
	required bytes sig = 2;
}
-}

data LocnProof = LocnProof
  { vault_key :: Required 1 (Value ByteString)
  , uid       :: Required 2 (Value ByteString)
  , unonce    :: Required 3 (Value ByteString)
  , apid      :: Required 4 (Value ByteString)
  , apnonce   :: Required 5 (Value ByteString)
  , time      :: Required 6 (Value (Fixed Word64))
  } deriving (Generic, Show, Eq)

instance Encode LocnProof
instance Decode LocnProof

data SignedLocnProof = SignedLocnProof
  { locnproof :: Required 1 (Message LocnProof)
  , sig       :: Required 2 (Value ByteString)
  } deriving (Generic, Show, Eq)

instance Encode SignedLocnProof
instance Decode SignedLocnProof

{-
// m5
message Token {
	required bytes vnonce = 1;
	required bytes locn_tag = 2;
}

message SignedToken {
	required Token token = 1;
	required bytes sig = 2;
}
-}

data Token = Token
  { vnonce   :: Required 1 (Value ByteString)
  , locn_tag :: Required 2 (Value ByteString)
  } deriving (Generic, Show, Eq)

instance Encode Token
instance Decode Token

data SignedToken = SignedToken
  { token :: Required 1 (Message Token)
  , sig   :: Required 2 (Value ByteString)
  } deriving (Generic, Show, Eq)

instance Encode SignedToken
instance Decode SignedToken

data ProtoBuf

instance Servant.Accept ProtoBuf where
  contentType _ = "application" // "x-protobuf"
instance Encode a => MimeRender ProtoBuf a where
  mimeRender _ = toLazyByteString . encodeMessage
instance Decode a => MimeUnrender ProtoBuf a where
  mimeUnrender _ bs =
    case runGetOrFail decodeMessage bs of
      Left (_, _, _)    -> Left "Invalid protocol buffer"
      Right (_, _, res) -> Right res
