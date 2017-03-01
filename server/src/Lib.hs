{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Lib
    ( startApp
    , app
    ) where

import           Data.Aeson               hiding (Value)
import           Data.Aeson.TH
import           Data.ByteString.Lazy
import           Data.Hex
import           Data.Int
import           Data.Monoid
import           Data.ProtocolBuffers
import           Data.Serialize           hiding (Get)
import           Data.Text
import           GHC.Generics             (Generic)
import           GHC.TypeLits
import           Network.HTTP.Media
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

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

data LocnProof = LocnProof
  { vaultKey :: Required 1 (Value Text)
  , uid      :: Required 2 (Value Text)
  , unonce   :: Required 3 (Value Text)
  , apid     :: Required 4 (Value Text)
  , apnonce  :: Required 5 (Value Text)
  , time     :: Required 6 (Value (Fixed Int64))
  , sig      :: Required 7 (Value Text)
  } deriving (Generic, Show)

instance Encode LocnProof
instance Decode LocnProof

data ProtoBuf
instance Servant.Accept ProtoBuf where
  contentType _ = "application" // "x-protobuf"
instance Encode a => MimeRender ProtoBuf a where
  mimeRender _ = fromStrict . fmap hex runPut . encodeMessage
instance Decode a => MimeUnrender ProtoBuf a where
  mimeUnrender _ bs = runGet decodeMessage =<< unhex (toStrict bs)

type API = "proofs" :> Get '[ProtoBuf] LocnProof

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return proof

proof :: LocnProof
proof = LocnProof {vaultKey = putField "key", uid = putField "1234", unonce = putField "x", apid = putField "5678", apnonce = putField "y", time = putField 64, sig = putField "me"}
