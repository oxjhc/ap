{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Main (main) where

import           Control.Monad.Logger       (runNoLoggingT)
import           Data.ByteString.Lazy       (ByteString)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Functor.Identity      (Identity (..))
import           Data.Hex
import           Data.ProtocolBuffers
import           Data.Proxy
import           Data.String
import qualified Database.Persist           as P
import qualified Database.Persist.Sql       as P
import qualified Database.Persist.Sqlite    as P
import           Models
import           Network.HTTP.Types
import           ProtoBuf
import           Servant                    (mimeRender, mimeUnrender)
import qualified Server                     (app)
import           Test.Hspec
import           Test.Hspec.Expectations
import           Test.Hspec.Wai

main :: IO ()
main = hspec spec

setUpDB :: IO P.ConnectionPool
setUpDB = do
  pool <- runNoLoggingT $
    P.createSqlitePool ":memory:" 5
  P.runSqlPool (P.runMigrationSilent Models.migrateAll) pool
  return pool

render :: Encode a => a -> ByteString
render = mimeRender (Proxy :: Proxy ProtoBuf)
unrender :: Decode a => ByteString -> Either String a
unrender = mimeUnrender (Proxy :: Proxy ProtoBuf)

postProto path proto =
  request methodPost path [(hContentType, "application/x-protobuf")] (render proto)
postRawProto path raw =
  request methodPost path [(hContentType, "application/x-protobuf")] raw

vaultMsg :: VaultMsg
vaultMsg = VaultMsg
  { vault = putField vault
  , uid = putField "1234"
  , unonce = putField "x"
  , apid = putField $ runIdentity $ unhex "3059301306072A8648CE3D020106082A8648CE3D03010703420004B0E53FA5E86FACE6E3CA942B66050819E16965E49C01CCB8ACD90FCCFAA1A9CE5CDD0DA13B73D71AEA17B9BBB53A924CF615E9F90D6F97ED528D0BE966BEA6B7"
  , apnonce = putField "y"
  , time = putField 64
  }
  where
    vault = ProtoBuf.Vault {points = putField [p1, p2]}
    p1 = Point {x = putField 1, y = putField 2}
    p2 = Point {x = putField 3, y = putField 4}

signedVaultMsg :: SignedVaultMsg
signedVaultMsg = SignedVaultMsg
  { vault_msg = putField vaultMsg
  , sig = putField $ runIdentity $ unhex "3045022100863AD2D6495569CBA7A4B074B0BFF7FC0A2A1F1896C8CCCB16F753E66560155A02207C25A6ABFD88241879417FCD3810BE409056A0C75ACE4444AD1B5B6059A55599"
  }

proof :: LocnProof
proof = LocnProof
  { vault_key = putField "123"
  , ekey = putField "5678"
  , uid = putField $ getField $ uid (vaultMsg :: VaultMsg)
  , unonce = putField $ getField $ unonce (vaultMsg :: VaultMsg)
  , apid = putField $ getField $ apid (vaultMsg :: VaultMsg)
  , apnonce = putField $ getField $ apnonce (vaultMsg :: VaultMsg)
  , time = putField $ getField $ time (vaultMsg :: VaultMsg)
  }


signedProof :: SignedLocnProof
signedProof = SignedLocnProof
  { locnproof = putField proof
  , sig = putField "0"
  }

wrongProof :: LocnProof
wrongProof = LocnProof
  { vault_key = putField "123"
  , ekey = putField "abc"
  , uid = putField $ getField $ uid (vaultMsg :: VaultMsg)
  , unonce = putField $ getField $ unonce (vaultMsg :: VaultMsg)
  , apid = putField $ getField $ apid (vaultMsg :: VaultMsg)
  , apnonce = putField $ getField $ apnonce (vaultMsg :: VaultMsg)
  , time = putField . (+1) . getField $ time (vaultMsg :: VaultMsg)
  }

signedWrongProof :: SignedLocnProof
signedWrongProof = SignedLocnProof
  { locnproof = putField wrongProof
  , sig = putField "0"
  }

token :: Token
token = Token
  { vnonce = putField "abcd"
  , locn_tag = putField "0"
  }

signedToken :: SignedToken
signedToken = SignedToken
  { token = putField Main.token
  , sig = putField "0"
  }

-- beforeAll creates the app before all tests and uses the same one
spec :: Spec
spec = beforeAll (Server.app <$> setUpDB) $ do
    describe "GET /ping" $ do
      it "should respond with 200" $ do
        get "/ping" `shouldRespondWith` 200
      it "should respond with 'Success!'" $ do
        get "/ping" `shouldRespondWith` "Success!"
    describe "POST /vault" $ do
      let postVault = postProto "/vault"
      describe "valid vault" $ do
        it "should respond with 200" $ do
          postVault signedVaultMsg `shouldRespondWith` 200
        it "should respond with 'Success!'" $ do
          postVault signedVaultMsg `shouldRespondWith` "Success!"
      describe "invalid vault" $ do
        it "should respond with 400" $ do
          postRawProto "/vault" "" `shouldRespondWith` 400
        it "should respond with 'Invalid protocol buffer'" $ do
          postRawProto "/vault" "" `shouldRespondWith`
            "Invalid protocol buffer" {matchStatus = 400}
    -- Depends on the vault already being submitted
    describe "POST /proof" $ do
      let postProof = postProto "/proof"
      describe "valid proof" $ do
        it "should respond with 200" $ do
          postProof signedProof `shouldRespondWith` 200
        it "should respond with correct token" $ do
          let res = fromString $ unpack $ render signedToken
          postProof signedProof `shouldRespondWith` res
      describe "invalid proof (protocol buffer)" $ do
        it "should respond with 400" $ do
          postRawProto "/proof" "" `shouldRespondWith` 400
        it "should respond with 'Invalid protocol buffer'" $ do
          postRawProto "/proof" "" `shouldRespondWith`
            "Invalid protocol buffer" {matchStatus = 400}
      describe "invalid proof (no corresponding vault message)" $ do
        it "should respond with 400" $ do
          postProof signedWrongProof `shouldRespondWith` 400
        it "should respond with 'Invalid proof.'" $ do
          postProof signedWrongProof `shouldRespondWith`
            "Invalid proof." {matchStatus = 400}
