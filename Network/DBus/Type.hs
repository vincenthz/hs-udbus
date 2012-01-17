{-# LANGUAGE Rank2Types #-}
-- |
-- Module      : Network.DBus.Type
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DBus.Type
	(
	  ObjectPath
	, DBusType(..)
	, putType
	, getType
	, sigType
	) where

import Data.Word
import Data.Int
import Data.ByteString (ByteString)
import Network.DBus.Wire
import Network.DBus.Signature
import qualified Network.DBus.IEEE754 as IEEE754
import Control.Applicative ((<$>))

-- | DBus ObjectPath
type ObjectPath = ByteString

-- | DBus Types
data DBusType =
	  DBusByte       Word8
	| DBusBoolean    Bool
	| DBusInt16      Int16
	| DBusUInt16     Word16
	| DBusInt32      Int32
	| DBusUInt32     Word32
	| DBusInt64      Int64
	| DBusUInt64     Word64
	| DBusDouble     Double
	| DBusString     ByteString
	| DBusObjectPath ObjectPath
	| DBusSignature  Signature
	| DBusArray      SignatureElem [DBusType]
	| DBusStruct     Signature [DBusType]
	| DBusDict       DBusType DBusType
	| DBusVariant    DBusType
	| DBusUnixFD     Word32
	deriving (Show,Eq)

-- | return signature element of a dbus type
sigType :: DBusType -> SignatureElem
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
sigType (DBusArray s _)    = SigArray s
sigType (DBusDict k v)     = SigDict (sigType k) (sigType v)
sigType (DBusUnixFD _)     = SigUnixFD

-- | return the alignement required for a specific signature element.
alignSigElement :: SignatureElem -> Int
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
putType :: DBusType -> PutWire
putType (DBusByte w)       = putw8 w
putType (DBusBoolean b)    = putw32 (if b then 1 else 0)
putType (DBusInt16 i)      = putw16 $ fromIntegral i
putType (DBusUInt16 w)     = putw16 w
putType (DBusInt32 i)      = putw32 $ fromIntegral i
putType (DBusUInt32 w)     = putw32 w
putType (DBusInt64 i)      = putw64 $ fromIntegral i
putType (DBusUInt64 w)     = putw64 w
putType (DBusDouble d)     = putw64 $ IEEE754.encode d
putType (DBusString s)     = putString s
putType (DBusObjectPath s) = putString s
putType (DBusSignature s)  = putSignature s
putType (DBusUnixFD fd)    = putw32 fd
putType (DBusStruct _ l)   = alignWrite 8 >> mapM_ putType l
putType (DBusDict k v)     = putType (DBusStruct [] [k,v])
putType (DBusVariant t)    = putSignature [sigType t] >> putType t
putType (DBusArray s l)    = putw32 (fromIntegral len) >> alignWrite alignElement >> mapM_ putType l
	where
		len           = length l * sizeofElement
		sizeofElement = 4
		alignElement  = alignSigElement s

-- | unserialize a dbus type from a signature Element
getType :: SignatureElem -> GetWire DBusType
getType SigByte   = DBusByte <$> getw8
getType SigBool   = DBusBoolean . iToB <$> getw32 where iToB i = i == 1
getType SigInt16  = DBusInt16 . fromIntegral <$> getw16
getType SigUInt16 = DBusUInt16 <$> getw16
getType SigInt32  = DBusInt32 . fromIntegral <$> getw32
getType SigUInt32 = DBusUInt32 <$> getw32
getType SigInt64  = DBusInt64 . fromIntegral <$> getw64
getType SigUInt64 = DBusUInt64 <$> getw64
getType SigDouble = DBusDouble . IEEE754.decode <$> getw64
getType SigString = DBusString <$> getString
getType SigObjectPath = DBusObjectPath <$> getObjectPath
getType SigSignature  = DBusSignature <$> getSignature
getType (SigDict k v) = getType (SigStruct [k,v])
getType SigUnixFD     = DBusUnixFD <$> getw32
getType SigVariant    = getVariant >>= getType >>= return . DBusVariant
getType (SigStruct sigs) =
	alignRead 8 >> mapM getType sigs >>= return . DBusStruct sigs
getType (SigArray t)  = do
	len <- getw32
	alignRead (alignSigElement t)
	l <- getMultiple (fromIntegral len) (getType t)
	return $ DBusArray t l
