{- This file was auto-generated from src/messages.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Proto.Src.Messages where
import qualified Prelude
import qualified Data.Int
import qualified Data.Word
import qualified Data.ProtoLens.Reexport.Data.ProtoLens
       as Data.ProtoLens
import qualified
       Data.ProtoLens.Reexport.Data.ProtoLens.Message.Enum
       as Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Reexport.Lens.Family2
       as Lens.Family2
import qualified Data.ProtoLens.Reexport.Lens.Family2.Unchecked
       as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Reexport.Data.Default.Class
       as Data.Default.Class
import qualified Data.ProtoLens.Reexport.Data.Text as Data.Text
import qualified Data.ProtoLens.Reexport.Data.Map as Data.Map
import qualified Data.ProtoLens.Reexport.Data.ByteString
       as Data.ByteString

data LocnProof = LocnProof{_LocnProof'vaultKey ::
                           !Data.ByteString.ByteString,
                           _LocnProof'uid :: !Data.ByteString.ByteString,
                           _LocnProof'unonce :: !Data.ByteString.ByteString,
                           _LocnProof'apid :: !Data.ByteString.ByteString,
                           _LocnProof'apnonce :: !Data.ByteString.ByteString,
                           _LocnProof'time :: !Data.Word.Word64,
                           _LocnProof'sig :: !Data.ByteString.ByteString}
               deriving (Prelude.Show, Prelude.Eq)

type instance Data.ProtoLens.Field "vaultKey" LocnProof =
     Data.ByteString.ByteString

instance Data.ProtoLens.HasField "vaultKey" LocnProof LocnProof
         where
        field _
          = Lens.Family2.Unchecked.lens _LocnProof'vaultKey
              (\ x__ y__ -> x__{_LocnProof'vaultKey = y__})

type instance Data.ProtoLens.Field "uid" LocnProof =
     Data.ByteString.ByteString

instance Data.ProtoLens.HasField "uid" LocnProof LocnProof where
        field _
          = Lens.Family2.Unchecked.lens _LocnProof'uid
              (\ x__ y__ -> x__{_LocnProof'uid = y__})

type instance Data.ProtoLens.Field "unonce" LocnProof =
     Data.ByteString.ByteString

instance Data.ProtoLens.HasField "unonce" LocnProof LocnProof where
        field _
          = Lens.Family2.Unchecked.lens _LocnProof'unonce
              (\ x__ y__ -> x__{_LocnProof'unonce = y__})

type instance Data.ProtoLens.Field "apid" LocnProof =
     Data.ByteString.ByteString

instance Data.ProtoLens.HasField "apid" LocnProof LocnProof where
        field _
          = Lens.Family2.Unchecked.lens _LocnProof'apid
              (\ x__ y__ -> x__{_LocnProof'apid = y__})

type instance Data.ProtoLens.Field "apnonce" LocnProof =
     Data.ByteString.ByteString

instance Data.ProtoLens.HasField "apnonce" LocnProof LocnProof
         where
        field _
          = Lens.Family2.Unchecked.lens _LocnProof'apnonce
              (\ x__ y__ -> x__{_LocnProof'apnonce = y__})

type instance Data.ProtoLens.Field "time" LocnProof =
     Data.Word.Word64

instance Data.ProtoLens.HasField "time" LocnProof LocnProof where
        field _
          = Lens.Family2.Unchecked.lens _LocnProof'time
              (\ x__ y__ -> x__{_LocnProof'time = y__})

type instance Data.ProtoLens.Field "sig" LocnProof =
     Data.ByteString.ByteString

instance Data.ProtoLens.HasField "sig" LocnProof LocnProof where
        field _
          = Lens.Family2.Unchecked.lens _LocnProof'sig
              (\ x__ y__ -> x__{_LocnProof'sig = y__})

instance Data.Default.Class.Default LocnProof where
        def
          = LocnProof{_LocnProof'vaultKey = Data.ProtoLens.fieldDefault,
                      _LocnProof'uid = Data.ProtoLens.fieldDefault,
                      _LocnProof'unonce = Data.ProtoLens.fieldDefault,
                      _LocnProof'apid = Data.ProtoLens.fieldDefault,
                      _LocnProof'apnonce = Data.ProtoLens.fieldDefault,
                      _LocnProof'time = Data.ProtoLens.fieldDefault,
                      _LocnProof'sig = Data.ProtoLens.fieldDefault}

instance Data.ProtoLens.Message LocnProof where
        descriptor
          = let vaultKey__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "vault_key"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required vaultKey)
                uid__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "uid"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required uid)
                unonce__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "unonce"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required unonce)
                apid__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "apid"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required apid)
                apnonce__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "apnonce"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required apnonce)
                time__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "time"
                      (Data.ProtoLens.Fixed64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required time)
                sig__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "sig"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required sig)
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, vaultKey__field_descriptor),
                    (Data.ProtoLens.Tag 2, uid__field_descriptor),
                    (Data.ProtoLens.Tag 3, unonce__field_descriptor),
                    (Data.ProtoLens.Tag 4, apid__field_descriptor),
                    (Data.ProtoLens.Tag 5, apnonce__field_descriptor),
                    (Data.ProtoLens.Tag 6, time__field_descriptor),
                    (Data.ProtoLens.Tag 7, sig__field_descriptor)])
                (Data.Map.fromList
                   [("vault_key", vaultKey__field_descriptor),
                    ("uid", uid__field_descriptor),
                    ("unonce", unonce__field_descriptor),
                    ("apid", apid__field_descriptor),
                    ("apnonce", apnonce__field_descriptor),
                    ("time", time__field_descriptor), ("sig", sig__field_descriptor)])

data ProofReq = ProofReq{_ProofReq'uid ::
                         !Data.ByteString.ByteString,
                         _ProofReq'unonce :: !Data.ByteString.ByteString,
                         _ProofReq'seqid :: !Data.Int.Int64,
                         _ProofReq'vid :: !Data.ByteString.ByteString,
                         _ProofReq'sig :: !Data.ByteString.ByteString}
              deriving (Prelude.Show, Prelude.Eq)

type instance Data.ProtoLens.Field "uid" ProofReq =
     Data.ByteString.ByteString

instance Data.ProtoLens.HasField "uid" ProofReq ProofReq where
        field _
          = Lens.Family2.Unchecked.lens _ProofReq'uid
              (\ x__ y__ -> x__{_ProofReq'uid = y__})

type instance Data.ProtoLens.Field "unonce" ProofReq =
     Data.ByteString.ByteString

instance Data.ProtoLens.HasField "unonce" ProofReq ProofReq where
        field _
          = Lens.Family2.Unchecked.lens _ProofReq'unonce
              (\ x__ y__ -> x__{_ProofReq'unonce = y__})

type instance Data.ProtoLens.Field "seqid" ProofReq =
     Data.Int.Int64

instance Data.ProtoLens.HasField "seqid" ProofReq ProofReq where
        field _
          = Lens.Family2.Unchecked.lens _ProofReq'seqid
              (\ x__ y__ -> x__{_ProofReq'seqid = y__})

type instance Data.ProtoLens.Field "vid" ProofReq =
     Data.ByteString.ByteString

instance Data.ProtoLens.HasField "vid" ProofReq ProofReq where
        field _
          = Lens.Family2.Unchecked.lens _ProofReq'vid
              (\ x__ y__ -> x__{_ProofReq'vid = y__})

type instance Data.ProtoLens.Field "sig" ProofReq =
     Data.ByteString.ByteString

instance Data.ProtoLens.HasField "sig" ProofReq ProofReq where
        field _
          = Lens.Family2.Unchecked.lens _ProofReq'sig
              (\ x__ y__ -> x__{_ProofReq'sig = y__})

instance Data.Default.Class.Default ProofReq where
        def
          = ProofReq{_ProofReq'uid = Data.ProtoLens.fieldDefault,
                     _ProofReq'unonce = Data.ProtoLens.fieldDefault,
                     _ProofReq'seqid = Data.ProtoLens.fieldDefault,
                     _ProofReq'vid = Data.ProtoLens.fieldDefault,
                     _ProofReq'sig = Data.ProtoLens.fieldDefault}

instance Data.ProtoLens.Message ProofReq where
        descriptor
          = let uid__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "uid"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required uid)
                unonce__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "unonce"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required unonce)
                seqid__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "seqid"
                      (Data.ProtoLens.Int64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required seqid)
                vid__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "vid"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required vid)
                sig__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "sig"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required sig)
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, uid__field_descriptor),
                    (Data.ProtoLens.Tag 2, unonce__field_descriptor),
                    (Data.ProtoLens.Tag 3, seqid__field_descriptor),
                    (Data.ProtoLens.Tag 4, vid__field_descriptor),
                    (Data.ProtoLens.Tag 5, sig__field_descriptor)])
                (Data.Map.fromList
                   [("uid", uid__field_descriptor),
                    ("unonce", unonce__field_descriptor),
                    ("seqid", seqid__field_descriptor), ("vid", vid__field_descriptor),
                    ("sig", sig__field_descriptor)])

data ProofResp = ProofResp{_ProofResp'uid ::
                           !Data.ByteString.ByteString,
                           _ProofResp'unonce :: !Data.ByteString.ByteString,
                           _ProofResp'sig :: !Data.ByteString.ByteString}
               deriving (Prelude.Show, Prelude.Eq)

type instance Data.ProtoLens.Field "uid" ProofResp =
     Data.ByteString.ByteString

instance Data.ProtoLens.HasField "uid" ProofResp ProofResp where
        field _
          = Lens.Family2.Unchecked.lens _ProofResp'uid
              (\ x__ y__ -> x__{_ProofResp'uid = y__})

type instance Data.ProtoLens.Field "unonce" ProofResp =
     Data.ByteString.ByteString

instance Data.ProtoLens.HasField "unonce" ProofResp ProofResp where
        field _
          = Lens.Family2.Unchecked.lens _ProofResp'unonce
              (\ x__ y__ -> x__{_ProofResp'unonce = y__})

type instance Data.ProtoLens.Field "sig" ProofResp =
     Data.ByteString.ByteString

instance Data.ProtoLens.HasField "sig" ProofResp ProofResp where
        field _
          = Lens.Family2.Unchecked.lens _ProofResp'sig
              (\ x__ y__ -> x__{_ProofResp'sig = y__})

instance Data.Default.Class.Default ProofResp where
        def
          = ProofResp{_ProofResp'uid = Data.ProtoLens.fieldDefault,
                      _ProofResp'unonce = Data.ProtoLens.fieldDefault,
                      _ProofResp'sig = Data.ProtoLens.fieldDefault}

instance Data.ProtoLens.Message ProofResp where
        descriptor
          = let uid__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "uid"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required uid)
                unonce__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "unonce"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required unonce)
                sig__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "sig"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required sig)
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, uid__field_descriptor),
                    (Data.ProtoLens.Tag 2, unonce__field_descriptor),
                    (Data.ProtoLens.Tag 3, sig__field_descriptor)])
                (Data.Map.fromList
                   [("uid", uid__field_descriptor),
                    ("unonce", unonce__field_descriptor),
                    ("sig", sig__field_descriptor)])

data Token = Token{_Token'vnonce :: !Data.ByteString.ByteString,
                   _Token'locnTag :: !Data.ByteString.ByteString}
           deriving (Prelude.Show, Prelude.Eq)

type instance Data.ProtoLens.Field "vnonce" Token =
     Data.ByteString.ByteString

instance Data.ProtoLens.HasField "vnonce" Token Token where
        field _
          = Lens.Family2.Unchecked.lens _Token'vnonce
              (\ x__ y__ -> x__{_Token'vnonce = y__})

type instance Data.ProtoLens.Field "locnTag" Token =
     Data.ByteString.ByteString

instance Data.ProtoLens.HasField "locnTag" Token Token where
        field _
          = Lens.Family2.Unchecked.lens _Token'locnTag
              (\ x__ y__ -> x__{_Token'locnTag = y__})

instance Data.Default.Class.Default Token where
        def
          = Token{_Token'vnonce = Data.ProtoLens.fieldDefault,
                  _Token'locnTag = Data.ProtoLens.fieldDefault}

instance Data.ProtoLens.Message Token where
        descriptor
          = let vnonce__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "vnonce"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required vnonce)
                locnTag__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "locn_tag"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required locnTag)
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, vnonce__field_descriptor),
                    (Data.ProtoLens.Tag 2, locnTag__field_descriptor)])
                (Data.Map.fromList
                   [("vnonce", vnonce__field_descriptor),
                    ("locn_tag", locnTag__field_descriptor)])

data VaultMsg = VaultMsg{_VaultMsg'vault :: !VaultMsg'Vault,
                         _VaultMsg'uid :: !Data.ByteString.ByteString,
                         _VaultMsg'unonce :: !Data.ByteString.ByteString,
                         _VaultMsg'apid :: !Data.ByteString.ByteString,
                         _VaultMsg'apnonce :: !Data.ByteString.ByteString,
                         _VaultMsg'time :: !Data.Word.Word64,
                         _VaultMsg'sig :: !Data.ByteString.ByteString}
              deriving (Prelude.Show, Prelude.Eq)

type instance Data.ProtoLens.Field "vault" VaultMsg =
     VaultMsg'Vault

instance Data.ProtoLens.HasField "vault" VaultMsg VaultMsg where
        field _
          = Lens.Family2.Unchecked.lens _VaultMsg'vault
              (\ x__ y__ -> x__{_VaultMsg'vault = y__})

type instance Data.ProtoLens.Field "uid" VaultMsg =
     Data.ByteString.ByteString

instance Data.ProtoLens.HasField "uid" VaultMsg VaultMsg where
        field _
          = Lens.Family2.Unchecked.lens _VaultMsg'uid
              (\ x__ y__ -> x__{_VaultMsg'uid = y__})

type instance Data.ProtoLens.Field "unonce" VaultMsg =
     Data.ByteString.ByteString

instance Data.ProtoLens.HasField "unonce" VaultMsg VaultMsg where
        field _
          = Lens.Family2.Unchecked.lens _VaultMsg'unonce
              (\ x__ y__ -> x__{_VaultMsg'unonce = y__})

type instance Data.ProtoLens.Field "apid" VaultMsg =
     Data.ByteString.ByteString

instance Data.ProtoLens.HasField "apid" VaultMsg VaultMsg where
        field _
          = Lens.Family2.Unchecked.lens _VaultMsg'apid
              (\ x__ y__ -> x__{_VaultMsg'apid = y__})

type instance Data.ProtoLens.Field "apnonce" VaultMsg =
     Data.ByteString.ByteString

instance Data.ProtoLens.HasField "apnonce" VaultMsg VaultMsg where
        field _
          = Lens.Family2.Unchecked.lens _VaultMsg'apnonce
              (\ x__ y__ -> x__{_VaultMsg'apnonce = y__})

type instance Data.ProtoLens.Field "time" VaultMsg =
     Data.Word.Word64

instance Data.ProtoLens.HasField "time" VaultMsg VaultMsg where
        field _
          = Lens.Family2.Unchecked.lens _VaultMsg'time
              (\ x__ y__ -> x__{_VaultMsg'time = y__})

type instance Data.ProtoLens.Field "sig" VaultMsg =
     Data.ByteString.ByteString

instance Data.ProtoLens.HasField "sig" VaultMsg VaultMsg where
        field _
          = Lens.Family2.Unchecked.lens _VaultMsg'sig
              (\ x__ y__ -> x__{_VaultMsg'sig = y__})

instance Data.Default.Class.Default VaultMsg where
        def
          = VaultMsg{_VaultMsg'vault = Data.Default.Class.def,
                     _VaultMsg'uid = Data.ProtoLens.fieldDefault,
                     _VaultMsg'unonce = Data.ProtoLens.fieldDefault,
                     _VaultMsg'apid = Data.ProtoLens.fieldDefault,
                     _VaultMsg'apnonce = Data.ProtoLens.fieldDefault,
                     _VaultMsg'time = Data.ProtoLens.fieldDefault,
                     _VaultMsg'sig = Data.ProtoLens.fieldDefault}

instance Data.ProtoLens.Message VaultMsg where
        descriptor
          = let vault__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "vault"
                      (Data.ProtoLens.MessageField ::
                         Data.ProtoLens.FieldTypeDescriptor VaultMsg'Vault)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required vault)
                uid__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "uid"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required uid)
                unonce__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "unonce"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required unonce)
                apid__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "apid"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required apid)
                apnonce__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "apnonce"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required apnonce)
                time__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "time"
                      (Data.ProtoLens.Fixed64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required time)
                sig__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "sig"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required sig)
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, vault__field_descriptor),
                    (Data.ProtoLens.Tag 2, uid__field_descriptor),
                    (Data.ProtoLens.Tag 3, unonce__field_descriptor),
                    (Data.ProtoLens.Tag 4, apid__field_descriptor),
                    (Data.ProtoLens.Tag 5, apnonce__field_descriptor),
                    (Data.ProtoLens.Tag 6, time__field_descriptor),
                    (Data.ProtoLens.Tag 7, sig__field_descriptor)])
                (Data.Map.fromList
                   [("vault", vault__field_descriptor),
                    ("uid", uid__field_descriptor),
                    ("unonce", unonce__field_descriptor),
                    ("apid", apid__field_descriptor),
                    ("apnonce", apnonce__field_descriptor),
                    ("time", time__field_descriptor), ("sig", sig__field_descriptor)])

data VaultMsg'Vault = VaultMsg'Vault{_VaultMsg'Vault'points ::
                                     ![VaultMsg'Vault'Point]}
                    deriving (Prelude.Show, Prelude.Eq)

type instance Data.ProtoLens.Field "points" VaultMsg'Vault =
     [VaultMsg'Vault'Point]

instance Data.ProtoLens.HasField "points" VaultMsg'Vault
         VaultMsg'Vault where
        field _
          = Lens.Family2.Unchecked.lens _VaultMsg'Vault'points
              (\ x__ y__ -> x__{_VaultMsg'Vault'points = y__})

instance Data.Default.Class.Default VaultMsg'Vault where
        def = VaultMsg'Vault{_VaultMsg'Vault'points = []}

instance Data.ProtoLens.Message VaultMsg'Vault where
        descriptor
          = let points__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "points"
                      (Data.ProtoLens.MessageField ::
                         Data.ProtoLens.FieldTypeDescriptor VaultMsg'Vault'Point)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked points)
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, points__field_descriptor)])
                (Data.Map.fromList [("points", points__field_descriptor)])

data VaultMsg'Vault'Point = VaultMsg'Vault'Point{_VaultMsg'Vault'Point'x
                                                 :: !Data.Word.Word32,
                                                 _VaultMsg'Vault'Point'y :: !Data.Word.Word32}
                          deriving (Prelude.Show, Prelude.Eq)

type instance Data.ProtoLens.Field "x" VaultMsg'Vault'Point =
     Data.Word.Word32

instance Data.ProtoLens.HasField "x" VaultMsg'Vault'Point
         VaultMsg'Vault'Point where
        field _
          = Lens.Family2.Unchecked.lens _VaultMsg'Vault'Point'x
              (\ x__ y__ -> x__{_VaultMsg'Vault'Point'x = y__})

type instance Data.ProtoLens.Field "y" VaultMsg'Vault'Point =
     Data.Word.Word32

instance Data.ProtoLens.HasField "y" VaultMsg'Vault'Point
         VaultMsg'Vault'Point where
        field _
          = Lens.Family2.Unchecked.lens _VaultMsg'Vault'Point'y
              (\ x__ y__ -> x__{_VaultMsg'Vault'Point'y = y__})

instance Data.Default.Class.Default VaultMsg'Vault'Point where
        def
          = VaultMsg'Vault'Point{_VaultMsg'Vault'Point'x =
                                   Data.ProtoLens.fieldDefault,
                                 _VaultMsg'Vault'Point'y = Data.ProtoLens.fieldDefault}

instance Data.ProtoLens.Message VaultMsg'Vault'Point where
        descriptor
          = let x__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "x"
                      (Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required x)
                y__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "y"
                      (Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required y)
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, x__field_descriptor),
                    (Data.ProtoLens.Tag 2, y__field_descriptor)])
                (Data.Map.fromList
                   [("x", x__field_descriptor), ("y", y__field_descriptor)])

apid ::
     forall msg msg' . Data.ProtoLens.HasField "apid" msg msg' =>
       Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "apid" msg)
         (Data.ProtoLens.Field "apid" msg')
apid
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "apid")

apnonce ::
        forall msg msg' . Data.ProtoLens.HasField "apnonce" msg msg' =>
          Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "apnonce" msg)
            (Data.ProtoLens.Field "apnonce" msg')
apnonce
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "apnonce")

locnTag ::
        forall msg msg' . Data.ProtoLens.HasField "locnTag" msg msg' =>
          Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "locnTag" msg)
            (Data.ProtoLens.Field "locnTag" msg')
locnTag
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "locnTag")

points ::
       forall msg msg' . Data.ProtoLens.HasField "points" msg msg' =>
         Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "points" msg)
           (Data.ProtoLens.Field "points" msg')
points
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "points")

seqid ::
      forall msg msg' . Data.ProtoLens.HasField "seqid" msg msg' =>
        Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "seqid" msg)
          (Data.ProtoLens.Field "seqid" msg')
seqid
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "seqid")

sig ::
    forall msg msg' . Data.ProtoLens.HasField "sig" msg msg' =>
      Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "sig" msg)
        (Data.ProtoLens.Field "sig" msg')
sig
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "sig")

time ::
     forall msg msg' . Data.ProtoLens.HasField "time" msg msg' =>
       Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "time" msg)
         (Data.ProtoLens.Field "time" msg')
time
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "time")

uid ::
    forall msg msg' . Data.ProtoLens.HasField "uid" msg msg' =>
      Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "uid" msg)
        (Data.ProtoLens.Field "uid" msg')
uid
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "uid")

unonce ::
       forall msg msg' . Data.ProtoLens.HasField "unonce" msg msg' =>
         Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "unonce" msg)
           (Data.ProtoLens.Field "unonce" msg')
unonce
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "unonce")

vault ::
      forall msg msg' . Data.ProtoLens.HasField "vault" msg msg' =>
        Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "vault" msg)
          (Data.ProtoLens.Field "vault" msg')
vault
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "vault")

vaultKey ::
         forall msg msg' . Data.ProtoLens.HasField "vaultKey" msg msg' =>
           Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "vaultKey" msg)
             (Data.ProtoLens.Field "vaultKey" msg')
vaultKey
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "vaultKey")

vid ::
    forall msg msg' . Data.ProtoLens.HasField "vid" msg msg' =>
      Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "vid" msg)
        (Data.ProtoLens.Field "vid" msg')
vid
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "vid")

vnonce ::
       forall msg msg' . Data.ProtoLens.HasField "vnonce" msg msg' =>
         Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "vnonce" msg)
           (Data.ProtoLens.Field "vnonce" msg')
vnonce
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "vnonce")

x ::
  forall msg msg' . Data.ProtoLens.HasField "x" msg msg' =>
    Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "x" msg)
      (Data.ProtoLens.Field "x" msg')
x = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "x")

y ::
  forall msg msg' . Data.ProtoLens.HasField "y" msg msg' =>
    Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "y" msg)
      (Data.ProtoLens.Field "y" msg')
y = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "y")