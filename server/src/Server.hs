{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Server where

import           Control.Monad.IO.Class
import           Data.ProtocolBuffers
import           Network.Wai
import           Network.Wai.Handler.Warp
import           ProtoBuf
import           Servant                  hiding (Vault)

newtype VaultResp = VaultResp String deriving (Eq, Show, MimeRender PlainText)
newtype PingResp = PingResp String deriving (Eq, Show, MimeRender PlainText)

type ServerAPI =
       -- Get a SignedLocnProof and check if it is valid, and if so return SignedToken.
       "proof" :> ReqBody '[ProtoBuf] SignedLocnProof :> Post '[ProtoBuf] SignedToken
       -- Receive a SignedVaultMsg, printing it out for now.
  :<|> "vault" :> ReqBody '[ProtoBuf] SignedVaultMsg :> Post '[PlainText] VaultResp
       -- Sanity check that the server is running.
  :<|> "ping" :> Get '[PlainText] PingResp

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy ServerAPI
api = Proxy

server :: Server ServerAPI
server = checkProof
    :<|> receiveVaultMsg
    :<|> return (PingResp "Success!")
  where
    newLine = putStr "\n"
    checkProof :: SignedLocnProof -> Handler SignedToken
    checkProof signedProof = do
      liftIO (putStrLn "Checking proof." >> print signedProof >> newLine)
      if (getField . locnproof) signedProof == proof then
        return SignedToken {token = putField Server.token, sig = putField "0"}
      else
        throwError (err400 {errBody = "Invalid proof."})
    receiveVaultMsg :: SignedVaultMsg -> Handler VaultResp
    receiveVaultMsg msg = do
      liftIO (putStrLn "Received message." >> print msg >> newLine)
      return (VaultResp $ show msg)

-- A valid SignedLocnProof that has this as the payload is:
-- 0A200A033132331204313233341A01782204353637382A0179314000000000000000120130
proof :: LocnProof
proof = LocnProof
  { vault_key = putField "123"
  , uid = putField "1234"
  , unonce = putField "x"
  , apid = putField "5678"
  , apnonce = putField "y"
  , time = putField 64
  }

-- A valid SignedToken which has this as the payload is:
-- 0A090A0461626364120130120130
token :: Token
token = Token
  { vnonce = putField "abcd"
  , locn_tag = putField "0"
  }
