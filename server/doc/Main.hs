{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
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
import           ProtoBuf             (LocnProof, LocnProof', Point, ProtoBuf,
                                       Token, Token', Vault', VaultMsg (..),
                                       VaultMsg')
import           ProtoBufConverter
import           Servant
import           Servant.Docs
import           Server
import           Vault                (Vault (..))


vaultMsg :: VaultMsg
vaultMsg = VaultMsg
  { vault = Vault [(1,2), (3,4)]
  , uid = "1234"
  , unonce = "x"
  , apid = "5678"
  , apnonce = "y"
  , time = 64
  , sig = "me"
  }


instance ToSample LocnProof where
  toSamples _ = singleSample proof

instance ToSample Token where
  toSamples _ = singleSample token

instance ToSample VaultMsg where
  toSamples _ = singleSample vaultMsg

instance ToSample VaultResp where
  toSamples _ = singleSample (VaultResp (show vaultMsg))

instance ToSample PingResp where
  toSamples _ = singleSample (PingResp "Success!")


proofInfo :: ExtraInfo ServerAPI
proofInfo = extraInfo (Proxy :: Proxy ("proof" :> ReqBody '[ProtoBuf] LocnProof :> Post '[ProtoBuf] Token)) $
  defAction & notes <>~
    [ DocNote "Relevant Schemas"
      [ "LocnProof is the protobuf in the request body and follows:\n\n```\n" ++
        show (schemaProto @LocnProof') ++ "\n```"
      , "Token is the protobuf in the response and follows:\n\n```\n" ++
        show (schemaProto @Token') ++ "\n```"
      ]
    ]

vaultInfo :: ExtraInfo ServerAPI
vaultInfo = extraInfo (Proxy :: Proxy ("vault" :> ReqBody '[ProtoBuf] VaultMsg :> Post '[PlainText] VaultResp)) $
  defAction & notes <>~
    [ DocNote "Relevant Schemas"
      [ "VaultMsg is the protobuf in the request body and follows:\n\n```\n" ++
        show (schemaProto @VaultMsg') ++ "\n```\n\n```\n" ++
        show (schemaProto @Vault') ++ "\n```" -- \n\n```\n" ++
        -- show (schemaProto @Point) ++ "\n```"
      ]
    ]

apiDocs :: API
apiDocs = docsWith defaultDocOptions [] (proofInfo <> vaultInfo) api

main :: IO ()
main = putStr (markdown apiDocs)
