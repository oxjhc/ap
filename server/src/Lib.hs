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
import           Data.ByteString.UTF8 (fromString)
import           Data.ByteString.Lazy (fromStrict)
import           Data.ProtocolBuffers
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant                  hiding (Vault)
import           Crypto.Saltine.Core.Box (newNonce)
import qualified Crypto.Saltine.Class as SaltClass

import           ProtoBuf
import           Vault
import           Verify
import           Number --Only used for the mock retrieveVault.

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
      liftIO (putStrLn "Checking proof." >> print prf >> newLine)
      vault <- liftIO $ retrieveVault prf
      case checkVault prf =<< vault of
        Right lt  -> liftIO $ do
          putStrLn ("Recieved valid proof: " ++ show lt)
          flip Token lt <$> SaltClass.encode <$> newNonce
        Left  err -> do
          liftIO $ putStrLn ("Recieved incorrect proof: "++err)
          throwError (err400 {errBody = fromStrict $ fromString err})
    receiveVaultMsg :: VaultMsg -> Handler String
    receiveVaultMsg msg = do
      liftIO (putStrLn "Received message." >> print msg >> newLine)
      return (show msg)

-- Serializes to: "0A060000000100021204313233341A01782204353637382A01793140000000000000003A026D65"
proof :: LocnProof
proof = LocnProof
  { vault_key = [0,1,2]
  , uid = "1234"
  , unonce = "x"
  , apid = "5678"
  , apnonce = "y"
  , time = 64
  , sig = "me"
  }

-- A mock implementation. Replace with retrieving the vault fom the database.
retrieveVault :: LocnProof -> IO (Either String (Vault PrimeField))
retrieveVault = const $ return $ return $ Vault [(0,0),(1,1),(2,4),(3,3)]
