{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
    -- * Serializing header for message
    , DBusHeader(..)
    -- * Fields type and accessor
    , DBusFields(..)
    , fieldsNew
    , fieldsNewWithBody
    , fieldsSetPath
    , fieldsSetInterface
    , fieldsSetMember
    , fieldsSetErrorName
    , fieldsSetReplySerial
    , fieldsSetDestination
    , fieldsSetSender
    , fieldsSetSignature
    , fieldsSetUnixFD
    -- * Message type
    , DBusMessage(..)
    , BusName(..)
    , Body
    , Serial
    , ErrorName(..)
    , Member(..)
    , Interface(..)
    , messageNew
    , messageMapFields
    -- * Parsing and serializing functions
    , headerFromMessage
    , messageFromHeader
    , readHeader
    , writeHeader
    , readFields
    , writeFields
    , writeBody
    , readBody
    , readBodyWith
    , readBodyRaw
    ) where

import Data.Data
import Data.Word
import Data.String
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 ()
import Control.Applicative ((<$>))
import Control.Monad.State

import Network.DBus.Internal
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
    deriving (Eq,Enum)

instance Show MessageType where
    show TypeInvalid      = "invalid"
    show TypeMethodCall   = "method_call"
    show TypeMethodReturn = "method_return"
    show TypeError        = "error"
    show TypeSignal       = "signal"

-- | dbus message flags
data MessageFlag =
      FlagNoReplyExpected
    | FlagNoAutoStart
    deriving (Show,Eq)

-- | dbus serial number
type Serial = Word32

data DBusHeader = DBusHeader
    { headerEndian       :: DBusEndian
    , headerMessageType  :: !MessageType
    , headerVersion      :: !Int
    , headerFlags        :: !Int
    , headerBodyLength   :: !Int
    , headerSerial       :: !Serial
    , headerFieldsLength :: !Int
    } deriving (Show,Eq)

type BodyRaw = (Signature,ByteString)
type Body = [DBusValue]

newtype Interface = Interface { unInterface :: String }
    deriving (Show,Eq,Ord)
newtype Member    = Member { unMember :: String }
    deriving (Show,Eq,Ord)
newtype BusName   = BusName { unBusName :: String }
    deriving (Show,Eq,Ord)
newtype ErrorName = ErrorName { unErrorName :: String }
    deriving (Show,Eq,Ord,Data,Typeable)

instance IsString Interface where
    fromString = Interface

instance IsString Member where
    fromString = Member

instance IsString BusName where
    fromString = BusName

instance IsString ErrorName where
    fromString = ErrorName

type UnixFD    = Word32

data DBusFields = DBusFields
    { fieldsPath        :: Maybe ObjectPath
    , fieldsInterface   :: Maybe Interface
    , fieldsMember      :: Maybe Member
    , fieldsErrorName   :: Maybe ErrorName
    , fieldsReplySerial :: Maybe Serial
    , fieldsDestination :: Maybe BusName
    , fieldsSender      :: Maybe BusName
    , fieldsSignature   :: Signature
    , fieldsUnixFD      :: Maybe UnixFD
    } deriving (Show,Eq)

data DBusMessage = DBusMessage
    { msgEndian  :: DBusEndian
    , msgType    :: !MessageType
    , msgVersion :: !Int
    , msgFlags   :: !Int
    , msgSerial  :: !Serial
    , msgFields  :: DBusFields
    , msgBodyRaw :: ByteString
    } deriving (Show,Eq)

fieldsSetPath :: ObjectPath -> DBusFields -> DBusFields
fieldsSetPath v fields = fields { fieldsPath = Just v }

fieldsSetInterface :: Interface -> DBusFields -> DBusFields
fieldsSetInterface v fields = fields { fieldsInterface = Just v }

fieldsSetMember :: Member -> DBusFields -> DBusFields
fieldsSetMember v fields = fields { fieldsMember = Just v }

fieldsSetErrorName :: ErrorName -> DBusFields -> DBusFields
fieldsSetErrorName v fields = fields { fieldsErrorName = Just v }

fieldsSetReplySerial :: Serial -> DBusFields -> DBusFields
fieldsSetReplySerial v fields = fields { fieldsReplySerial = Just v }

fieldsSetDestination :: BusName -> DBusFields -> DBusFields
fieldsSetDestination v fields = fields { fieldsDestination = Just v }

fieldsSetSender :: BusName -> DBusFields -> DBusFields
fieldsSetSender v fields = fields { fieldsSender = Just v }

fieldsSetSignature :: Signature -> DBusFields -> DBusFields
fieldsSetSignature v fields = fields { fieldsSignature = v }

fieldsSetUnixFD :: UnixFD -> DBusFields -> DBusFields
fieldsSetUnixFD v fields = fields { fieldsUnixFD = Just v }

fieldsNew = DBusFields Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing

fieldsNewWithBody body = fieldsNew { fieldsSignature = if null body then [] else signatureBody body }

messageNew :: MessageType -> Body -> (DBusFields -> DBusFields) -> DBusMessage
messageNew ty body fieldsSetter = DBusMessage
                                    { msgEndian  = LE
                                    , msgType    = ty
                                    , msgVersion = 1
                                    , msgFlags   = 0
                                    , msgSerial  = 0
                                    , msgFields  = fieldsSetter $ fieldsNewWithBody body
                                    , msgBodyRaw = writeBody body
                                    }

messageMapFields :: (DBusFields -> DBusFields) -> DBusMessage -> DBusMessage
messageMapFields f msg = msg { msgFields = f $ msgFields msg }

headerFromMessage :: DBusMessage -> DBusHeader
headerFromMessage msg = DBusHeader
                            { headerEndian       = msgEndian msg
                            , headerMessageType  = msgType msg
                            , headerVersion      = msgVersion msg
                            , headerFlags        = msgFlags msg
                            , headerBodyLength   = 0
                            , headerSerial       = msgSerial msg
                            , headerFieldsLength = 0
                            }

messageFromHeader :: DBusHeader -> DBusMessage
messageFromHeader hdr = DBusMessage
                            { msgEndian   = headerEndian hdr
                            , msgType     = headerMessageType hdr
                            , msgVersion  = headerVersion hdr
                            , msgFlags    = headerFlags hdr
                            , msgSerial   = headerSerial hdr
                            , msgFields   = fieldsNew
                            , msgBodyRaw  = B.empty
                            }

-- | unserialize a dbus header (16 bytes)
readHeader :: ByteString -> DBusHeader
readHeader b = getWire endianness 1 getHeader remainingBytes where
    (firstByte,remainingBytes) = B.splitAt 1 b
    endianness = if (fromIntegral $ B.head firstByte) /= fromEnum 'l' then BE else LE
    getHeader = do
        mt     <- toEnum . fromIntegral <$> getw8
        flags  <- fromIntegral          <$> getw8
        ver    <- fromIntegral          <$> getw8
        blen   <- fromIntegral <$> getw32
        serial <- getw32
        flen   <- fromIntegral <$> getw32

        return DBusHeader
            { headerEndian       = endianness
            , headerMessageType  = mt
            , headerVersion      = ver
            , headerFlags        = flags
            , headerBodyLength   = blen
            , headerSerial       = serial
            , headerFieldsLength = flen
            }

-- | serialize a dbus header
writeHeader :: DBusHeader -> ByteString
writeHeader hdr = putWire [putHeader]
    where putHeader = do
            putw8 $ fromIntegral $ fromEnum $ if headerEndian hdr == BE then 'B' else 'l'
            putw8 $ fromIntegral $ fromEnum $ headerMessageType hdr
            putw8 $ fromIntegral $ headerFlags hdr
            putw8 $ fromIntegral $ headerVersion hdr
            putw32 $ fromIntegral $ headerBodyLength hdr
            putw32 $ fromIntegral $ headerSerial hdr
            putw32 $ fromIntegral $ headerFieldsLength hdr

-- | unserialize dbus message fields
readFields :: DBusEndian -> ByteString -> DBusFields
readFields endianness = getWire endianness 16 (getFields fieldsNew)
    where
        getFields :: DBusFields -> GetWire DBusFields
        getFields fields = isWireEmpty >>= \empty -> if empty then return fields else getField fields >>= getFields

        getField :: DBusFields -> GetWire DBusFields
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

        getFieldVal :: Int -> GetWire (DBusFields -> DBusFields)
        getFieldVal 1 = fieldsSetPath <$> getObjectPath
        getFieldVal 2 = fieldsSetInterface . fromString . packedStringToString <$> getString
        getFieldVal 3 = fieldsSetMember . fromString . packedStringToString <$> getString
        getFieldVal 4 = fieldsSetErrorName . fromString . packedStringToString <$> getString
        getFieldVal 5 = fieldsSetReplySerial <$> getw32
        getFieldVal 6 = fieldsSetDestination . fromString . packedStringToString <$> getString
        getFieldVal 7 = fieldsSetSender . fromString . packedStringToString <$> getString
        getFieldVal 8 = fieldsSetSignature <$> getSignature
        getFieldVal 9 = fieldsSetUnixFD    <$> getw32
        getFieldVal n = error ("unknown field: " ++ show n)
        
-- | serialize dbus message fields
-- this doesn't include the necessary padding at the end.
writeFields :: DBusFields -> ByteString
writeFields fields = putWire . (:[]) $ do
    putField 1 SigObjectPath putObjectPath $ fieldsPath fields
    putField 2 SigString (putUString . unInterface) $ fieldsInterface fields
    putField 3 SigString (putUString . unMember) $ fieldsMember fields
    putField 4 SigString (putUString . unErrorName) $ fieldsErrorName fields
    putField 5 SigUInt32 putw32 $ fieldsReplySerial fields
    putField 6 SigString (putUString . unBusName) $ fieldsDestination fields
    putField 7 SigString (putUString . unBusName) $ fieldsSender fields
    putField 8 SigSignature putSignature $ if null (fieldsSignature fields) then Nothing else Just $ fieldsSignature fields
    putField 9 SigUInt32 putw32 $ fieldsUnixFD fields
    where
        putUString = putString . fromString

        putField :: Word8 -> Type -> (a -> PutWire) -> Maybe a -> PutWire
        putField _ _ _      Nothing  = return ()
        putField w s putter (Just v) =
            alignWrite 8 >> putw8 w >> putVariant s >> putter v

-- | serialize body
writeBody :: Body -> ByteString
writeBody els = putWire (map putValue els)

signatureBody :: Body -> Signature
signatureBody = map sigType

-- | process a raw body (byteString) with the specified endianness and signature.
readBodyRaw :: DBusEndian -> Signature -> ByteString -> Body
readBodyRaw endian sig = getWire endian 0 (mapM getValue sig)

-- | read message's body with a defined signature
readBodyWith :: DBusMessage -> Signature -> Body
readBodyWith m sigs = readBodyRaw (msgEndian m) sigs (msgBodyRaw m)

-- | read message's body using the signature field as reference
readBody :: DBusMessage -> Body
readBody m = readBodyWith m (fieldsSignature $ msgFields m)
