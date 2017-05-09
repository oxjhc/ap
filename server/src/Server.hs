{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Server where

import           Control.Monad             (guard)
import           Control.Monad.IO.Class
import           Control.Monad.Logger      (runStderrLoggingT)
import           Data.Binary.Builder.Sized
import           Data.Binary.Get           (runGetOrFail)
import           Data.ByteString           (ByteString)
import           Data.ByteString.Lazy      (fromStrict, toStrict)
import           Data.ProtocolBuffers
import           Data.String.Conversions
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           Network.Wai               hiding (vault)
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Servant                   hiding (Vault)

import           Cryptography
import           Models
import           ProtoBuf
import           Verification

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
startApp sqliteFile = do
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 80 $ setLogger aplogger defaultSettings
    runSettings settings =<< makeApp sqliteFile

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
    -- Function to check proof

    checkProof :: SignedLocnProof -> Handler SignedToken
    checkProof signedProof = do
      let prf  = getField $ locnproof signedProof
      liftIO $ putStrLn "Checking proof." >> print (showProto prf) >> newLine
      res <- liftIO $ flip runSqlPersistMPool pool $ do
        let uid'          = getField $ uid     (prf :: LocnProof)
            unonce'       = getField $ unonce  (prf :: LocnProof)
            apid'         = getField $ apid    (prf :: LocnProof)
            apnonce'      = getField $ apnonce (prf :: LocnProof)
            (Fixed time') = getField $ time    (prf :: LocnProof)
        pubKey <- getBy (PublicKeyID apid')
        case pubKey of
          Nothing -> return Nothing
          Just _ -> getBy (MessageID uid' unonce' apid' apnonce' (fromIntegral time'))
      case res of
        -- Not a 404 because that leaks information
        Nothing -> throwError (err400 {errBody = "Invalid proof."})
        Just blob -> case runGetOrFail decodeMessage . fromStrict .
                          messageVault . entityVal $ blob of
          Left (_, _, err)    -> throwError (err400 {errBody = "Corrupt."})
          Right (_, _, vault) -> do
            liftIO $ putStrLn "Got corresponding vault, checking validity."
            let signature = getField $ sig (signedProof :: SignedLocnProof)
            proofResult <- liftIO $ valid prf (convertVault vault) (fromStrict signature)
            case proofResult of
              Just locnTag -> liftIO $ genSignedToken locnTag
              Nothing      -> throwError (err400 {errBody = "Invalid proof."})

    receiveVaultMsg :: SignedVaultMsg -> Handler VaultResp
    receiveVaultMsg signedMsg = do
      let msg = getField . vault_msg $ signedMsg
      liftIO $ putStrLn "Received message: " >> print (showProto msg) >> newLine
      if not $ checkVaultSignature msg $ fromStrict $ getField $ sig (signedMsg :: SignedVaultMsg)
        then throwError (err400 {errBody = "Invalud vault message."})
        else return ()
      res <- liftIO $ do
        flip runSqlPersistMPool pool $ do
          let vault'        = toStrict . toLazyByteString . encodeMessage $
                                (getField . vault $ msg)
              uid'          = getField $ uid     (msg :: VaultMsg)
              unonce'       = getField $ unonce  (msg :: VaultMsg)
              apid'         = getField $ apid    (msg :: VaultMsg)
              apnonce'      = getField $ apnonce (msg :: VaultMsg)
              (Fixed time') = getField $ time    (msg :: VaultMsg)
          pubKey <- getBy (PublicKeyID apid')
          case pubKey of
            Nothing -> return Nothing
            Just _ -> Just <$> insertBy (Message vault' uid' unonce' apid' apnonce' (fromIntegral time'))
      case res of
        Just e -> do
          case e of
            Left _  -> liftIO $ putStrLn "WARNING: Duplicate exists, did not write."
            Right _ -> return ()
          return (VaultResp "Success!")
        Nothing -> throwError (err400 {errBody = "Invalid vault message."})

proof :: LocnProof
proof = LocnProof
  { vault_key = putField "123"
  , ekey = putField "abc"
  , uid = putField "1234"
  , unonce = putField "x"
  , apid = putField "5678"
  , apnonce = putField "y"
  , time = putField 64
  }
