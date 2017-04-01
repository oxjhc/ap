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

import           Control.Monad.IO.Class
import           Data.ProtocolBuffers
import           Network.Wai
import           Network.Wai.Handler.Warp
import           ProtoBuf
import           Servant                  hiding (Vault)

type API =
       -- Get a LocnProof and check if it is valid, and if so return Token.
       "proof" :> ReqBody '[ProtoBuf] LocnProof :> Post '[ProtoBuf] Token
       -- Receive a VaultMsg, printing it out for now.
  :<|> "vault" :> ReqBody '[ProtoBuf] VaultMsg :> Post '[PlainText] String

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = checkProof
    :<|> receiveVaultMsg
  where
    newLine = putStr "\n"
    checkProof :: LocnProof -> Handler Token
    checkProof prf = do
      liftIO (putStrLn "Checking proof." >> print (showProto prf) >> newLine)
      if prf == proof then
        return token
      else
        throwError (err404 {errBody = "Invalid proof."})
      where
        token = Token
          { vnonce = putField "abcd"
          , locn_tag = putField "efgh"
          }
    receiveVaultMsg :: VaultMsg -> Handler String
    receiveVaultMsg msg = do
      liftIO (putStrLn "Received message." >> print (showProto msg) >> newLine)
      return (show (showProto msg))

-- Serializes to: 0A036B65791204313233341A01782204353637382A01793140000000000000003A026D65
proof :: LocnProof
proof = LocnProof
  { vault_key = putField "key"
  , uid = putField "1234"
  , unonce = putField "x"
  , apid = putField "5678"
  , apnonce = putField "y"
  , time = putField 64
  , sig = putField "me"
  }
