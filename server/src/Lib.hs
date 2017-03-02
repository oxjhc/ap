{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Lib
    ( startApp
    , app
    ) where

import           Data.ProtocolBuffers
import           Network.Wai
import           Network.Wai.Handler.Warp
import           ProtoBuf
import           Servant                  hiding (Vault)

type API = "proof" :> Get '[ProtoBuf] LocnProof

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return proof

proof :: LocnProof
proof = LocnProof {vault_key = putField "key", uid = putField "1234", unonce = putField "x", apid = putField "5678", apnonce = putField "y", time = putField 64, sig = putField "me"}
