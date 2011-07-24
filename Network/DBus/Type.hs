{-# LANGUAGE Rank2Types #-}
module Network.DBus.Type
	(
	  ObjectPath
	, DbusType(..)
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

-- | Dbus Types
data DbusType =
	  DbusByte       Word8
	| DbusBoolean    Bool
	| DbusInt16      Int16
	| DbusUInt16     Word16
	| DbusInt32      Int32
	| DbusUInt32     Word32
	| DbusInt64      Int64
	| DbusUInt64     Word64
	| DbusDouble     Double
	| DbusString     ByteString
	| DbusObjectPath ObjectPath
	| DbusSignature  Signature
	| DbusArray      SignatureElem [DbusType]
	| DbusStruct     Signature [DbusType]
	| DbusDict       DbusType DbusType
	| DbusVariant    DbusType
	| DbusUnixFD     Word32
	deriving (Show,Eq)

-- | return signature element of a dbus type
sigType :: DbusType -> SignatureElem
sigType (DbusByte _)       = SigByte
sigType (DbusBoolean _)    = SigBool
sigType (DbusInt16 _)      = SigInt16
sigType (DbusUInt16 _)     = SigUInt16
sigType (DbusInt32 _)      = SigInt32
sigType (DbusUInt32 _)     = SigUInt32
sigType (DbusInt64 _)      = SigInt64
sigType (DbusUInt64 _)     = SigUInt64
sigType (DbusDouble _)     = SigDouble
sigType (DbusString _)     = SigString
sigType (DbusObjectPath _) = SigObjectPath
sigType (DbusSignature _)  = SigSignature
sigType (DbusStruct s _)   = SigStruct s
sigType (DbusVariant _)    = SigVariant
sigType (DbusArray s _)    = SigArray s
sigType (DbusDict k v)     = SigDict (sigType k) (sigType v)
sigType (DbusUnixFD _)     = SigUnixFD

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
putType :: DbusType -> PutWire
putType (DbusByte w)       = putw8 w
putType (DbusBoolean b)    = putw32 (if b then 1 else 0)
putType (DbusInt16 i)      = putw16 $ fromIntegral i
putType (DbusUInt16 w)     = putw16 w
putType (DbusInt32 i)      = putw32 $ fromIntegral i
putType (DbusUInt32 w)     = putw32 w
putType (DbusInt64 i)      = putw64 $ fromIntegral i
putType (DbusUInt64 w)     = putw64 w
putType (DbusDouble d)     = putw64 $ IEEE754.encode d
putType (DbusString s)     = putString s
putType (DbusObjectPath s) = putString s
putType (DbusSignature s)  = putSignature s
putType (DbusUnixFD fd)    = putw32 fd
putType (DbusStruct _ l)   = alignWrite 8 >> mapM_ putType l
putType (DbusDict k v)     = putType (DbusStruct [] [k,v])
putType (DbusVariant t)    = putSignature [sigType t] >> putType t
putType (DbusArray s l)    = putw32 (fromIntegral len) >> alignWrite alignElement >> mapM_ putType l
	where
		len           = length l * sizeofElement
		sizeofElement = 4
		alignElement  = alignSigElement s

-- | unserialize a dbus type from a signature Element
getType :: SignatureElem -> GetWire DbusType
getType SigByte   = DbusByte <$> getw8
getType SigBool   = DbusBoolean . iToB <$> getw32 where iToB i = i == 1
getType SigInt16  = DbusInt16 . fromIntegral <$> getw16
getType SigUInt16 = DbusUInt16 <$> getw16
getType SigInt32  = DbusInt32 . fromIntegral <$> getw32
getType SigUInt32 = DbusUInt32 <$> getw32
getType SigInt64  = DbusInt64 . fromIntegral <$> getw64
getType SigUInt64 = DbusUInt64 <$> getw64
getType SigDouble = DbusDouble . IEEE754.decode <$> getw64
getType SigString = DbusString <$> getString
getType SigObjectPath = DbusObjectPath <$> getObjectPath
getType SigSignature  = DbusSignature <$> getSignature
getType (SigDict k v) = getType (SigStruct [k,v])
getType SigUnixFD     = DbusUnixFD <$> getw32
getType SigVariant    = getVariant >>= getType >>= return . DbusVariant
getType (SigStruct sigs) =
	alignRead 8 >> mapM getType sigs >>= return . DbusStruct sigs
getType (SigArray t)  = do
	len <- getw32
	alignRead (alignSigElement t)
	l <- getMultiple (fromIntegral len) (getType t)
	return $ DbusArray t l
