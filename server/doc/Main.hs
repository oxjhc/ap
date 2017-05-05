{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Control.Lens
import           Data.Monoid          ((<>))
import           Data.ProtocolBuffers
import           Data.Proxy
import           Data.Text            (Text, pack)
import           ProtoBuf
import           Servant
import           Servant.Docs
import           Server               hiding (proof, token)
import           Vault                (Vault (..))


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

signedProof :: SignedLocnProof
signedProof = SignedLocnProof
  { locnproof = putField proof
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

vaultMsg :: VaultMsg
vaultMsg = VaultMsg
  { vault = putField vault
  , uid = putField "1234"
  , unonce = putField "x"
  , apid = putField "5678"
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
  , sig = putField "0"
  }

instance ToSample SignedLocnProof where
  toSamples _ = singleSample signedProof

instance ToSample SignedToken where
  toSamples _ = singleSample signedToken

instance ToSample SignedVaultMsg where
  toSamples _ = singleSample signedVaultMsg

instance ToSample VaultResp where
  toSamples _ = singleSample (VaultResp (show vaultMsg))

instance ToSample PingResp where
  toSamples _ = singleSample (PingResp "Success!")


proofInfo :: ExtraInfo ServerAPI
proofInfo = extraInfo (Proxy :: Proxy ("proof" :> ReqBody '[ProtoBuf] SignedLocnProof :> Post '[ProtoBuf] SignedToken)) $
  defAction & notes <>~
    [ DocNote "Relevant Schemas"
      [ "SignedLocnProof is the protobuf in the request body and follows:\n\n```\n" ++
        show (schemaProto @SignedLocnProof) ++ "\n```\n\n```\n" ++
        show (schemaProto @LocnProof) ++ "\n```"
      , "SignedToken is the protobuf in the response and follows:\n\n```\n" ++
        show (schemaProto @SignedToken) ++ "\n```\n\n```\n" ++
        show (schemaProto @Token) ++ "\n```"
      ]
    ]

vaultInfo :: ExtraInfo ServerAPI
vaultInfo = extraInfo (Proxy :: Proxy ("vault" :> ReqBody '[ProtoBuf] SignedVaultMsg :> Post '[PlainText] VaultResp)) $
  defAction & notes <>~
    [ DocNote "Relevant Schemas"
      [ "SignedVaultMsg is the protobuf in the request body and follows:\n\n```\n" ++
        show (schemaProto @SignedVaultMsg) ++ "\n```\n\n```\n" ++
        show (schemaProto @VaultMsg) ++ "\n```\n\n```\n" ++
        show (schemaProto @ProtoBuf.Vault) ++ "\n```\n\n```\n" ++
        show (schemaProto @Point) ++ "\n```"
      ]
    ]

apiDocs :: API
apiDocs = docsWith defaultDocOptions [] (proofInfo <> vaultInfo) api

main :: IO ()
main = putStr (markdown apiDocs)
