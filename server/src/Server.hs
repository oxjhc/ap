{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Server where

import           Control.Monad.IO.Class
import           Control.Monad.Logger     (runStderrLoggingT)
import           Data.ProtocolBuffers
import           Data.String.Conversions
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           Models
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

startApp :: FilePath -> IO ()
startApp sqliteFile = run 8080 =<< makeApp sqliteFile

makeApp :: FilePath -> IO Application
makeApp sqliteFile = do
  pool <- runStderrLoggingT $
    createSqlitePool (cs sqliteFile) 5

  runSqlPool (runMigration migrateAll) pool
  return $ app pool

app :: ConnectionPool -> Application
app pool = serve api $ server pool

api :: Proxy ServerAPI
api = Proxy

server :: ConnectionPool -> Server ServerAPI
server pool = checkProof
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
      liftIO . flip runSqlPersistMPool pool $ do
        insert (Models.Message "a" "b" "c" "d" "e" 0)
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
