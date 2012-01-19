{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Network.DBus.Message
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DBus.Message
	(
	  MessageType(..)
	, MessageFlag(..)
	, Header(..)
	, Field(..)
	, Message(..)
	, Serial
	-- * create new message
	, msgMethodCall
	, msgMethodReturn
	, msgError
	, msgSignal
	-- * Parsing and serializing functions
	, headerFromMessage
	, messageFromHeader
	, readHeader
	, writeHeader
	, readFields
	, writeFields
	, readBody
	, readBodyWith
	) where

import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 ()
import Control.Applicative ((<$>))
import Control.Monad.State

import Network.DBus.Wire
import Network.DBus.Type
import Network.DBus.Signature

-- | dbus message types
data MessageType =
	  TypeInvalid
	| TypeMethodCall
	| TypeMethodReturn
	| TypeError
	| TypeSignal
	deriving (Show,Eq,Enum)

-- | dbus message flags
data MessageFlag =
	  FlagNoReplyExpected
	| FlagNoAutoStart
        deriving (Show,Eq)

-- | dbus serial number
type Serial = Word32

data Header = Header
	{ headerEndian       :: DbusEndian
	, headerMessageType  :: !MessageType
	, headerVersion      :: !Int
	, headerFlags        :: !Int
	, headerBodyLength   :: !Int
	, headerSerial       :: !Serial
	, headerFieldsLength :: !Int
	} deriving (Show,Eq)

type Body = [DBusType]

type Interface = ByteString
type Member    = ByteString
type BusName   = ByteString
type ErrorName = ByteString

data Field =
	  FieldPath        ObjectPath
	| FieldInterface   Interface
	| FieldMember      Member
	| FieldErrorName   ErrorName
	| FieldReplySerial Serial
	| FieldDestination BusName
	| FieldSender      ByteString
	| FieldSignature   Signature
	| FieldUnixFds     Word32
	deriving (Show,Eq)

fieldVal :: Field -> Int
fieldVal (FieldPath        _) = 1
fieldVal (FieldInterface   _) = 2
fieldVal (FieldMember      _) = 3
fieldVal (FieldErrorName   _) = 4
fieldVal (FieldReplySerial _) = 5
fieldVal (FieldDestination _) = 6
fieldVal (FieldSender      _) = 7
fieldVal (FieldSignature   _) = 8
fieldVal (FieldUnixFds     _) = 9
	
data Message = Message
	{ msgEndian  :: DbusEndian
	, msgType    :: !MessageType
	, msgVersion :: !Int
	, msgFlags   :: !Int
	, msgSerial  :: !Serial
	, msgFields  :: [Field]
	, msgBody    :: ByteString
	} deriving (Show,Eq)

defaultMessage :: Message
defaultMessage = Message
	{ msgEndian  = LE
	, msgType    = TypeInvalid
	, msgVersion = 1
	, msgFlags   = 0
	, msgSerial  = 0
	, msgFields  = []
	, msgBody    = B.empty
	}

headerFromMessage :: Message -> Header
headerFromMessage msg = Header
	{ headerEndian       = msgEndian msg
	, headerMessageType  = msgType msg
	, headerVersion      = msgVersion msg
	, headerFlags        = msgFlags msg
	, headerBodyLength   = 0
	, headerSerial       = msgSerial msg
	, headerFieldsLength = 0
	}

messageFromHeader :: Header -> Message
messageFromHeader hdr = Message
	{ msgEndian   = headerEndian hdr
	, msgType     = headerMessageType hdr
	, msgVersion  = headerVersion hdr
	, msgFlags    = headerFlags hdr
	, msgSerial   = headerSerial hdr
	, msgFields   = []
	, msgBody     = B.empty
	}

-- | create a new method call message
msgMethodCall :: BusName -> ObjectPath -> Interface -> Member -> Body -> Message
msgMethodCall destination path interface method body = defaultMessage
	{ msgType   = TypeMethodCall
	, msgFields =
		[ FieldPath path
		, FieldDestination destination
		, FieldInterface interface
		, FieldMember method
		] ++ if null body then [] else [ FieldSignature $ signatureBody body ]
	, msgBody   = writeBody body
	}

-- | create a new signal message
msgSignal :: ObjectPath -> Interface -> Member -> Body -> Message
msgSignal path interface method body = defaultMessage
	{ msgType   = TypeSignal
	, msgFields =
		[ FieldPath path
		, FieldInterface interface
		, FieldMember method
		] ++ if null body then [] else [ FieldSignature $ signatureBody body ]
	, msgBody   = writeBody body
	}

-- | create a new method return message
msgMethodReturn :: Serial -> Body -> Message
msgMethodReturn replySerial body = defaultMessage
	{ msgType   = TypeMethodReturn
	, msgFields =
		[ FieldReplySerial replySerial
		] ++ if null body then [] else [ FieldSignature $ signatureBody body ]
	, msgBody   = writeBody body
	}

-- | create a new error message
msgError :: ErrorName -> Serial -> Body -> Message
msgError errorName replySerial body = defaultMessage
	{ msgType   = TypeError
	, msgFields =
		[ FieldErrorName errorName
		, FieldReplySerial replySerial
		] ++ if null body then [] else [ FieldSignature $ signatureBody body ]
	, msgBody   = writeBody body
	}

-- | unserialize a dbus header (16 bytes)
readHeader :: ByteString -> Header
readHeader b = getWire LE 0 getHeader b
	where getHeader = do
		e      <- getw8
		let bswap32 = id -- FIXME
		let swapf = if fromIntegral e /= fromEnum 'l' then bswap32 else id
		mt     <- toEnum . fromIntegral <$> getw8
		flags  <- fromIntegral          <$> getw8
		ver    <- fromIntegral          <$> getw8
		blen   <- fromIntegral . swapf  <$> getw32
		serial <- swapf                 <$> getw32
		flen   <- fromIntegral . swapf  <$> getw32

		return $! Header
			{ headerEndian       = if fromIntegral e /= fromEnum 'l' then BE else LE
			, headerMessageType  = mt
			, headerVersion      = ver
			, headerFlags        = flags
			, headerBodyLength   = blen
			, headerSerial       = serial
			, headerFieldsLength = flen
			}

-- | serialize a dbus header
writeHeader :: Header -> ByteString
writeHeader hdr = putWire [putHeader]
	where putHeader = do
		putw8 $ fromIntegral $ fromEnum $ if headerEndian hdr == BE then 'b' else 'l'
		putw8 $ fromIntegral $ fromEnum $ headerMessageType hdr
		putw8 $ fromIntegral $ headerFlags hdr
		putw8 $ fromIntegral $ headerVersion hdr
		putw32 $ fromIntegral $ headerBodyLength hdr
		putw32 $ fromIntegral $ headerSerial hdr
		putw32 $ fromIntegral $ headerFieldsLength hdr

-- | unserialize dbus message fields
readFields :: ByteString -> [Field]
readFields b = getWire LE 16 getFields b
	where
		getFields :: GetWire [Field]
		getFields = isWireEmpty >>= \empty -> if empty then return [] else liftM2 (:) getField getFields

		getField :: GetWire Field
		getField = do
			ty        <- fromIntegral <$> getw8
			signature <- getVariant
			when (getSigVal ty /= signature) $ error "field type invalid"
			t         <- getFieldVal ty
			alignRead 8
			return t

		getSigVal 1 = SigObjectPath
		getSigVal 2 = SigString
		getSigVal 3 = SigString
		getSigVal 4 = SigString
		getSigVal 5 = SigUInt32
		getSigVal 6 = SigString
		getSigVal 7 = SigString
		getSigVal 8 = SigSignature
		getSigVal 9 = SigUnixFD
		getSigVal n = error ("unknown field: " ++ show n)

		getFieldVal :: Int -> GetWire Field
		getFieldVal 1 = FieldPath        <$> getObjectPath
		getFieldVal 2 = FieldInterface   <$> getString
		getFieldVal 3 = FieldMember      <$> getString
		getFieldVal 4 = FieldErrorName   <$> getString
		getFieldVal 5 = FieldReplySerial <$> getw32
		getFieldVal 6 = FieldDestination <$> getString
		getFieldVal 7 = FieldSender      <$> getString
		getFieldVal 8 = FieldSignature   <$> getSignature
		getFieldVal 9 = FieldUnixFds     <$> getw32
		getFieldVal n = error ("unknown field: " ++ show n)
		
-- | serialize dbus message fields
-- this doesn't include the necessary padding at the end.
writeFields :: [Field] -> ByteString
writeFields fields = putWire (putFields fields)
	where
		putFields :: [Field] -> [PutWire]
		putFields l = map putField l
		putField f = alignWrite 8 >> putw8 (fromIntegral $ fieldVal f) >> putFieldVal f

		putFieldVal :: Field -> PutWire
		putFieldVal (FieldPath        s) = putVariant SigObjectPath >> putObjectPath s
		putFieldVal (FieldInterface   s) = putVariant SigString >> putString s
		putFieldVal (FieldMember      s) = putVariant SigString >> putString s
		putFieldVal (FieldErrorName   s) = putVariant SigString >> putString s
		putFieldVal (FieldReplySerial s) = putVariant SigUInt32 >> putw32 s
		putFieldVal (FieldDestination s) = putVariant SigString >> putString s
		putFieldVal (FieldSender      s) = putVariant SigString >> putString s
		putFieldVal (FieldSignature   s) = putVariant SigSignature >> putSignature s
		putFieldVal (FieldUnixFds     _) = putVariant SigUInt32 >> putw32 0

-- | serialize body
writeBody :: Body -> ByteString
writeBody els = putWire (map putType els)

signatureBody :: Body -> Signature
signatureBody body = map sigType body

-- | read message's body with a defined signature
readBodyWith :: Message -> Signature -> Body
readBodyWith m sigs = getWire (msgEndian m) 0 (mapM getType sigs) (msgBody m)

-- | read message's body using the signature field as reference
readBody :: Message -> Body
readBody m = readBodyWith m (getFieldSig $ msgFields m)
	where
		getFieldSig fields = case filter isFieldSignature fields of
			[FieldSignature s] -> s
			_                  -> []

		isFieldSignature (FieldSignature _) = True
		isFieldSignature _                  = False
