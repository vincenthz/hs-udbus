{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Network.DBus.Actions
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DBus.Actions
    ( DBusContext
    , DBusTransport(..)
    , authenticate
    , authenticateUID

    , connectSystem
    , connectSession
    , connectTo
    , connectOver
    , connectUnix
    , contextNew
    , contextNewWith

    , busGetSession
    , busGetSystem
    , busGetNextSerial
    , busClose

    , messageSend
    , messageSendWithSerial
    , messageRecv

    -- * from Message module
    , MessageType(..)
    , MessageFlag(..)
    , DBusFields(..)
    , DBusMessage(..)
    , Serial

    -- * read a message body
    , readBody
    , readBodyWith

    -- * from Signature module
    , Type(..)
    , SignatureElem
    , Signature
    , serializeSignature
    , unserializeSignature

    -- * from Type module
    , ObjectPath(..)
    , PackedString(..)
    , packedStringToString
    , DBusValue(..)
    , DBusTypeable(..)
    ) where

import Numeric (showHex)
import Data.Char (ord)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BC

import Control.Arrow
import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Monad.State

import System.Environment
import System.IO hiding (hGetLine)
import Network.Socket
import Network.DBus.Message
import Network.DBus.Type
import Network.DBus.Internal
import Network.DBus.Signature

data DBusTransport = DBusTransport
    { transportGet   :: Int -> IO ByteString
    , transportPut   :: ByteString -> IO ()
    , transportClose :: IO ()
    }

data DBusContext = DBusContext
    { contextTransport :: DBusTransport
    , contextSerial    :: MVar Serial
    }

withTransport :: DBusContext -> (DBusTransport -> IO a) -> IO a
withTransport ctx f = f $ contextTransport ctx

transportHandle :: Handle -> DBusTransport
transportHandle h = DBusTransport
                        { transportGet   = BC.hGet h
                        , transportPut   = BC.hPut h
                        , transportClose = hClose h
                        }

hGet :: DBusContext -> Int -> IO ByteString
hGet ctx i = withTransport ctx (\t -> transportGet t i)

hPut :: DBusContext -> ByteString -> IO ()
hPut ctx b = withTransport ctx (\t -> transportPut t b)

hPuts :: DBusContext -> [ByteString] -> IO ()
hPuts ctx bs = withTransport ctx (\t -> mapM_ (transportPut t) bs)

hGetLine :: DBusContext -> IO ()
hGetLine ctx = withTransport ctx getTillEOL
    where getTillEOL transport = do
              v <- transportGet transport 1
              if BC.singleton '\n' == v then return () else getTillEOL transport

-- | authenticate to DBus using a UID.
authenticateUID :: DBusContext -> Int -> IO ()
authenticateUID ctx uid = authenticate ctx hexencoded_uid
    where hexencoded_uid = BC.pack $ concatMap (hex2 . ord) $ show uid
          hex2 a
             | a < 0x10  = '0' : showHex a ""
             | otherwise = showHex a ""

-- | authenticate to DBus using a raw bytestring.
authenticate :: DBusContext -> ByteString -> IO ()
authenticate ctx auth = do
    hPut ctx $ BC.concat ["\0AUTH EXTERNAL ", auth, "\r\n"]
    _ <- hGetLine ctx
    hPut ctx "BEGIN\r\n"

close :: DBusTransport -> IO ()
close = transportClose

connectUnix :: ByteString -> IO Handle
connectUnix addr = do
    let sockaddr = SockAddrUnix $ BC.unpack addr
    sock <- socket AF_UNIX Stream 0
    connect sock sockaddr
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h NoBuffering
    return h

connectOver :: ByteString -> [(ByteString, ByteString)] -> IO Handle
connectOver "unix" flags = do
    let abstract = lookup "abstract" flags
    case abstract of
        Nothing   -> error "no abstract path, use the normal path ..."
        Just path -> connectUnix $ BC.concat ["\x00", path]

connectOver _ _ = error "not implemented yet"

connectTo :: ByteString -> IO Handle
connectTo addr = do
    let (domain, flagstr) = second BC.tail $ BC.breakSubstring ":" addr
    let flags = map (\x -> let (k:v:[]) = BC.split '=' x in (k,v)) $ BC.split ',' flagstr
    connectOver domain flags

-- | connect to the dbus session bus define by the environment variable DBUS_SESSION_BUS_ADDRESS
connectSession :: IO Handle
connectSession = BC.pack <$> getEnv "DBUS_SESSION_BUS_ADDRESS" >>= connectTo

-- | connect to the dbus system bus
connectSystem :: IO Handle
connectSystem = connectUnix "/var/run/dbus/system_bus_socket"

-- | create a new DBus context from an handle
contextNew :: Handle -> IO DBusContext
contextNew h = contextNewWith (transportHandle h)

-- | create a new DBus context from a transport
contextNewWith :: DBusTransport -> IO DBusContext
contextNewWith transport = liftM (DBusContext transport) (newMVar 1)

-- | create a new DBus context on session bus
busGetSession :: IO DBusContext 
busGetSession = connectSession >>= contextNew

-- | create a new DBus context on system bus
busGetSystem :: IO DBusContext
busGetSystem = connectSystem >>= contextNew

-- | close this DBus context
busClose :: DBusContext -> IO ()
busClose = transportClose . contextTransport

-- | get the next serial usable, and increment the serial state.
busGetNextSerial :: DBusContext -> IO Serial
busGetNextSerial ctx =
    modifyMVar (contextSerial ctx) (\v -> return $! (v+1, v))

-- | send one message to the bus with a predefined serial number.
messageSendWithSerial :: DBusContext -> Serial -> DBusMessage -> IO ()
messageSendWithSerial ctx serial msg = do
    let fieldstr = writeFields (msgFields msg)
    let fieldlen = BC.length fieldstr
    let alignfields = alignVal 8 fieldlen - fieldlen
    let header = (headerFromMessage msg)
                    { headerBodyLength   = BC.length $ msgBodyRaw msg
                    , headerFieldsLength = fieldlen
                    , headerSerial       = serial }
    hPuts ctx [ writeHeader header, fieldstr, BC.replicate alignfields '\0', msgBodyRaw msg ]

-- | send one message to the bus
-- note that the serial of the message sent is allocated here.
messageSend :: DBusContext -> DBusMessage -> IO Serial
messageSend ctx msg = do
    serial <- busGetNextSerial ctx
    messageSendWithSerial ctx serial msg
    return serial

-- | receive one single message from the bus
-- it is not necessarily the reply from a previous sent message.
messageRecv :: DBusContext -> IO DBusMessage
messageRecv ctx = do
    hdr    <- readHeader <$> hGet ctx 16
    fields <- readFields (headerEndian hdr) <$> hGet ctx (alignVal 8 $ headerFieldsLength hdr)
    body   <- hGet ctx (headerBodyLength hdr)
    return $ (messageFromHeader hdr) { msgFields = fields, msgBodyRaw = body }

alignVal :: Int -> Int -> Int
alignVal n x
    | x `mod` n == 0 = x
    | otherwise      = x + (n - (x `mod` n))
