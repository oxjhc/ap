{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import           Data.Aeson
import           Data.ByteString     (ByteString)
import           Data.Text

import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Message
  vault ByteString
  uid ByteString
  unonce ByteString
  apid ByteString
  apnonce ByteString
  time Int
  MessageID uid unonce apid apnonce time
  deriving Eq Read Show
PublicKey
  value ByteString
  PublicKeyID value
  deriving Eq Read Show
|]
