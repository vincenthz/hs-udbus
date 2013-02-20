{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- |
-- Module      : Network.DBus.Type
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DBus.Type
    ( ObjectPath
    , DBusValue(..)
    , DBusTypeable(..)
    , putValue
    , getValue
    , sigType
    ) where

import Data.Word
import Data.Data
import Data.Int
import Data.String
import qualified Data.ByteString as B
import Network.DBus.Wire
import Network.DBus.Signature
import Network.DBus.Internal
import qualified Network.DBus.IEEE754 as IEEE754
import Control.Applicative ((<$>))
import Control.Monad (liftM)

-- | DBus Types
data DBusValue =
      DBusByte       Word8
    | DBusBoolean    Bool
    | DBusInt16      Int16
    | DBusUInt16     Word16
    | DBusInt32      Int32
    | DBusUInt32     Word32
    | DBusInt64      Int64
    | DBusUInt64     Word64
    | DBusDouble     Double
    | DBusString     PackedString
    | DBusObjectPath ObjectPath
    | DBusSignature  Signature
    | DBusByteArray  B.ByteString       -- special case of the DBusArray
    | DBusArray      Type [DBusValue]
    | DBusStruct     Signature [DBusValue]
    | DBusDict       DBusValue DBusValue
    | DBusVariant    DBusValue
    | DBusUnixFD     Word32
    deriving (Show,Eq,Data,Typeable)

class DBusTypeable a where
    toSignature   :: a -> Type
    toDBusValue   :: a -> DBusValue
    fromDBusValue :: DBusValue -> Maybe a

#define SIMPLE_DBUS_INSTANCE(hsType,constructor) \
    instance DBusTypeable hsType where \
    { toSignature = sigType . constructor \
    ; toDBusValue = constructor \
    ; fromDBusValue (constructor x) = Just x \
    ; fromDBusValue _               = Nothing \
    }

instance DBusTypeable DBusValue where
    toSignature   = sigType
    toDBusValue   = id
    fromDBusValue = Just

SIMPLE_DBUS_INSTANCE(Word8, DBusByte)
SIMPLE_DBUS_INSTANCE(Bool, DBusBoolean)
SIMPLE_DBUS_INSTANCE(Int16, DBusInt16)
SIMPLE_DBUS_INSTANCE(Word16, DBusUInt16)
SIMPLE_DBUS_INSTANCE(Int32, DBusInt32)
SIMPLE_DBUS_INSTANCE(Word32, DBusUInt32)
SIMPLE_DBUS_INSTANCE(Int64, DBusInt64)
SIMPLE_DBUS_INSTANCE(Word64, DBusUInt64)
SIMPLE_DBUS_INSTANCE(Double, DBusDouble)
SIMPLE_DBUS_INSTANCE(ObjectPath, DBusObjectPath)

instance DBusTypeable String where
    toSignature _ = SigString
    toDBusValue = DBusString . fromString
    fromDBusValue (DBusString s) = Just (show s)
    fromDBusValue _              = Nothing

-- | return signature element of a dbus type
sigType :: DBusValue -> Type
sigType (DBusByte _)       = SigByte
sigType (DBusBoolean _)    = SigBool
sigType (DBusInt16 _)      = SigInt16
sigType (DBusUInt16 _)     = SigUInt16
sigType (DBusInt32 _)      = SigInt32
sigType (DBusUInt32 _)     = SigUInt32
sigType (DBusInt64 _)      = SigInt64
sigType (DBusUInt64 _)     = SigUInt64
sigType (DBusDouble _)     = SigDouble
sigType (DBusString _)     = SigString
sigType (DBusObjectPath _) = SigObjectPath
sigType (DBusSignature _)  = SigSignature
sigType (DBusStruct s _)   = SigStruct s
sigType (DBusVariant _)    = SigVariant
sigType (DBusByteArray _)  = SigArray SigByte
sigType (DBusArray s _)    = SigArray s
sigType (DBusDict k v)     = SigDict (sigType k) (sigType v)
sigType (DBusUnixFD _)     = SigUnixFD

-- | return the alignement required for a specific signature element.
alignSigElement :: Type -> Int
alignSigElement SigByte = 1
alignSigElement SigBool = 1
alignSigElement SigInt16 = 2
alignSigElement SigUInt16 = 2
alignSigElement SigInt32 = 4
alignSigElement SigUInt32 = 4
alignSigElement SigInt64 = 8
alignSigElement SigUInt64 = 8
alignSigElement SigDouble = 8
alignSigElement SigString = 4
alignSigElement SigObjectPath = 4
alignSigElement SigSignature = 1
alignSigElement (SigDict _ _) = 8
alignSigElement (SigStruct _) = 8
alignSigElement SigVariant = 1
alignSigElement (SigArray _) = 4
alignSigElement SigUnixFD = 4

-- | serialize a dbus type
putValue :: DBusValue -> PutWire
putValue (DBusByte w)       = putw8 w
putValue (DBusBoolean b)    = putw32 (if b then 1 else 0)
putValue (DBusInt16 i)      = putw16 $ fromIntegral i
putValue (DBusUInt16 w)     = putw16 w
putValue (DBusInt32 i)      = putw32 $ fromIntegral i
putValue (DBusUInt32 w)     = putw32 w
putValue (DBusInt64 i)      = putw64 $ fromIntegral i
putValue (DBusUInt64 w)     = putw64 w
putValue (DBusDouble d)     = putw64 $ IEEE754.encode d
putValue (DBusString s)     = putString s
putValue (DBusObjectPath s) = putObjectPath s
putValue (DBusSignature s)  = putSignature s
putValue (DBusUnixFD fd)    = putw32 fd
putValue (DBusStruct _ l)   = alignWrite 8 >> mapM_ putValue l
putValue (DBusDict k v)     = putValue (DBusStruct [] [k,v])
putValue (DBusVariant t)    = putSignature [sigType t] >> putValue t
putValue (DBusByteArray l)  =
    putw32 (fromIntegral $ B.length l) >> alignWrite alignElement >> putBytes l
    where alignElement = alignSigElement SigByte
putValue (DBusArray s l)    = do
    pos <- putWireGetPosition
    let alignmentStart = pos + alignWriteCalculate 4 pos + 4
    let alignmentEnd   = alignmentStart + alignWriteCalculate alignElement alignmentStart
    let content = putWireAt alignmentEnd [mapM_ putValue l]
    putw32 (fromIntegral $ B.length content) >> alignWrite alignElement >> putBytes content
    where
        alignElement = alignSigElement s

-- | unserialize a dbus type from a signature Element
getValue :: Type -> GetWire DBusValue
getValue SigByte   = DBusByte <$> getw8
getValue SigBool   = DBusBoolean . iToB <$> getw32 where iToB i = i == 1
getValue SigInt16  = DBusInt16 . fromIntegral <$> getw16
getValue SigUInt16 = DBusUInt16 <$> getw16
getValue SigInt32  = DBusInt32 . fromIntegral <$> getw32
getValue SigUInt32 = DBusUInt32 <$> getw32
getValue SigInt64  = DBusInt64 . fromIntegral <$> getw64
getValue SigUInt64 = DBusUInt64 <$> getw64
getValue SigDouble = DBusDouble . IEEE754.decode <$> getw64
getValue SigString = DBusString <$> getString
getValue SigObjectPath = DBusObjectPath <$> getObjectPath
getValue SigSignature  = DBusSignature <$> getSignature
getValue (SigDict k v) = do
    alignRead 8
    key <- getValue k
    val <- getValue v
    return $ DBusDict key val
getValue SigUnixFD     = DBusUnixFD <$> getw32
getValue SigVariant    = liftM DBusVariant (getVariant >>= getValue)
getValue (SigStruct sigs) =
    liftM (DBusStruct sigs) (alignRead 8 >> mapM getValue sigs)
getValue (SigArray t)  = do
    len <- getw32
    alignRead (alignSigElement t)
    case t of
        SigByte -> DBusByteArray <$> getBytes (fromIntegral len)
        _       -> DBusArray t <$> getMultiple (fromIntegral len) (getValue t)
