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
	-- * serializing structure for message
	, Header(..)
	, Fields(..)
	-- * lowlevel type representation
	, DBusMessage(..)
	, Body
	, Serial
	, Member
	, Interface
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

data DBusHeader = DBusHeader
	{ headerEndian       :: DbusEndian
	, headerMessageType  :: !MessageType
	, headerVersion      :: !Int
	, headerFlags        :: !Int
	, headerBodyLength   :: !Int
	, headerSerial       :: !Serial
	, headerFieldsLength :: !Int
	} deriving (Show,Eq)

type BodyRaw = (Signature,ByteString)
type Body = [DBusType]

type Interface = ByteString
type Member    = ByteString
type BusName   = ByteString
type ErrorName = ByteString
type UnixFD    = Word32

data DBusFields = DBusFields
	{ fieldsPath        :: Maybe ObjectPath
	, fieldsInterface   :: Maybe Interface
	, fieldsMember      :: Maybe Member
	, fieldsErrorName   :: Maybe ErrorName
	, fieldsReplySerial :: Maybe Serial
	, fieldsDestination :: Maybe BusName
	, fieldsSender      :: Maybe ByteString
	, fieldsSignature   :: Signature
	, fieldsUnixFD      :: Maybe UnixFD
	} deriving (Show,Eq)

data DBusMessage = DBusMessage
	{ msgEndian  :: DBusEndian
	, msgType    :: !MessageType
	, msgVersion :: !Int
	, msgFlags   :: !Int
	, msgSerial  :: !Serial
	, msgFields  :: Fields
	, msgBodyRaw :: ByteString
	} deriving (Show,Eq)

fieldsSetPath :: ObjectPath -> Fields -> Fields
fieldsSetPath v fields = fields { fieldsPath = Just v }

fieldsSetInterface :: Interface -> Fields -> Fields
fieldsSetInterface v fields = fields { fieldsInterface = Just v }

fieldsSetMember :: Member -> Fields -> Fields
fieldsSetMember v fields = fields { fieldsMember = Just v }

fieldsSetErrorName :: ErrorName -> Fields -> Fields
fieldsSetErrorName v fields = fields { fieldsErrorName = Just v }

fieldsSetReplySerial :: Serial -> Fields -> Fields
fieldsSetReplySerial v fields = fields { fieldsReplySerial = Just v }

fieldsSetDestination :: BusName -> Fields -> Fields
fieldsSetDestination v fields = fields { fieldsDestination = Just v }

fieldsSetSender :: ByteString -> Fields -> Fields
fieldsSetSender v fields = fields { fieldsSender = Just v }

fieldsSetSignature :: Signature -> Fields -> Fields
fieldsSetSignature v fields = fields { fieldsSignature = v }

fieldsSetUnixFD :: UnixFD -> Fields -> Fields
fieldsSetUnixFD v fields = fields { fieldsUnixFD = Just v }

emptyFields = Fields Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing

emptyFieldsWithBody body = emptyFields
	{ fieldsSignature = if null body then [] else signatureBody body }

defaultMessage :: DBusMessage
defaultMessage = DBusMessage
	{ msgEndian  = LE
	, msgType    = TypeInvalid
	, msgVersion = 1
	, msgFlags   = 0
	, msgSerial  = 0
	, msgFields  = emptyFields
	, msgBodyRaw = B.empty
	}

headerFromMessage :: DBusMessage -> Header
headerFromMessage msg = Header
	{ headerEndian       = msgEndian msg
	, headerMessageType  = msgType msg
	, headerVersion      = msgVersion msg
	, headerFlags        = msgFlags msg
	, headerBodyLength   = 0
	, headerSerial       = msgSerial msg
	, headerFieldsLength = 0
	}

messageFromHeader :: Header -> DBusMessage
messageFromHeader hdr = DBusMessage
	{ msgEndian   = headerEndian hdr
	, msgType     = headerMessageType hdr
	, msgVersion  = headerVersion hdr
	, msgFlags    = headerFlags hdr
	, msgSerial   = headerSerial hdr
	, msgFields   = emptyFields
	, msgBodyRaw  = B.empty
	}

-- | create a new method call message
msgMethodCall :: BusName -> ObjectPath -> Maybe Interface -> Member -> Body -> DBusMessage
msgMethodCall destination path interface method body = defaultMessage
	{ msgType   = TypeMethodCall
	, msgFields = fieldsSetPath path
		$ fieldsSetDestination destination
		$ maybe id fieldsSetInterface interface
		$ fieldsSetMember method
		$ emptyFieldsWithBody body
	, msgBodyRaw = writeBody body
	}

-- | create a new signal message
msgSignal :: ObjectPath -> Interface -> Member -> Body -> DBusMessage
msgSignal path interface method body = defaultMessage
	{ msgType   = TypeSignal
	, msgFields = fieldsSetPath path
		$ fieldsSetInterface interface
		$ fieldsSetMember method
		$ emptyFieldsWithBody body
	, msgBodyRaw = writeBody body
	}

-- | create a new method return message
msgMethodReturn :: Serial -> Body -> DBusMessage
msgMethodReturn replySerial body = defaultMessage
	{ msgType   = TypeMethodReturn
	, msgFields = fieldsSetReplySerial replySerial
		$ emptyFieldsWithBody body
	, msgBodyRaw = writeBody body
	}

-- | create a new error message
msgError :: ErrorName -> Serial -> Body -> DBusMessage
msgError errorName replySerial body = defaultMessage
	{ msgType   = TypeError
	, msgFields = fieldsSetErrorName errorName
		$ fieldsSetReplySerial replySerial
		$ emptyFieldsWithBody body
	, msgBodyRaw = writeBody body
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
readFields :: ByteString -> Fields
readFields b = getWire LE 16 (getFields emptyFields) b
	where
		getFields :: Fields -> GetWire Fields
		getFields fields = isWireEmpty >>= \empty -> if empty then return fields else (getField fields >>= getFields)

		getField :: Fields -> GetWire Fields
		getField fields = do
			ty        <- fromIntegral <$> getw8
			signature <- getVariant
			when (getSigVal ty /= signature) $ error "field type invalid"
			setter    <- getFieldVal ty
			alignRead 8
			return (setter fields)

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

		getFieldVal :: Int -> GetWire (Fields -> Fields)
		getFieldVal 1 = fieldsSetPath <$> getObjectPath
		getFieldVal 2 = fieldsSetInterface <$> getString
		getFieldVal 3 = fieldsSetMember <$> getString
		getFieldVal 4 = fieldsSetErrorName  <$> getString
		getFieldVal 5 = fieldsSetReplySerial <$> getw32
		getFieldVal 6 = fieldsSetDestination <$> getString
		getFieldVal 7 = fieldsSetSender     <$> getString
		getFieldVal 8 = fieldsSetSignature  <$> getSignature
		getFieldVal 9 = fieldsSetUnixFD    <$> getw32
		getFieldVal n = error ("unknown field: " ++ show n)
		
-- | serialize dbus message fields
-- this doesn't include the necessary padding at the end.
writeFields :: Fields -> ByteString
writeFields fields = putWire . (:[]) $ do
	putField 1 SigObjectPath putObjectPath $ fieldsPath fields
	putField 2 SigString putString $ fieldsInterface fields
	putField 3 SigString putString $ fieldsMember fields
	putField 4 SigString putString $ fieldsErrorName fields
	putField 5 SigUInt32 putw32 $ fieldsReplySerial fields
	putField 6 SigString putString $ fieldsDestination fields
	putField 7 SigString putString $ fieldsSender fields
	putField 8 SigSignature putSignature $ if null (fieldsSignature fields) then Nothing else Just $ fieldsSignature fields
	putField 9 SigUInt32 putw32 $ fieldsUnixFD fields
	where
		putField :: Word8 -> SignatureElem -> (a -> PutWire) -> Maybe a -> PutWire
		putField _ _ _      Nothing  = return ()
		putField w s putter (Just v) =
			alignWrite 8 >> putw8 w >> putVariant s >> putter v

-- | serialize body
writeBody :: Body -> ByteString
writeBody els = putWire (map putType els)

signatureBody :: Body -> Signature
signatureBody body = map sigType body

-- | read message's body with a defined signature
readBodyWith :: DBusMessage -> Signature -> Body
readBodyWith m sigs = getWire (msgEndian m) 0 (mapM getType sigs) (msgBodyRaw m)

-- | read message's body using the signature field as reference
readBody :: DBusMessage -> Body
readBody m = readBodyWith m (fieldsSignature $ msgFields m)
