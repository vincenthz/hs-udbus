{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Network.DBus.Actions
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DBus.Actions
	( DBusHandle
	, DBusContext
	, authenticate
	, authenticateUID

	, connectSession
	, connectSystem
	, connectHandle

	, busGetSession
	, busGetSystem
	, busGetNextSerial

	, messageSend
	, messageSendWithSerial
	, messageRecv

	-- * from Message module
	, MessageType(..)
	, MessageFlag(..)
	, Field(..)
	, Message(..)
	, Serial
	, msgMethodCall
	, msgMethodReturn
	, msgError
	, msgSignal

	-- * read a message body
	, readBody
	, readBodyWith

	-- * from Signature module
	, SignatureElem(..)
	, Signature

	-- * from Type module
	, ObjectPath
	, DBusType(..)
	) where

import Numeric (showHex)
import Data.Char (ord)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L

import Control.Arrow
import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Exception
import Control.Monad.State

import System.Environment
import System.IO hiding (hGetLine)
import Network.Socket
import Network.DBus.Message
import Network.DBus.Type
import Network.DBus.Signature

-- | Represent an open access to dbus. for now only based on system handle.
newtype DBusHandle = DBusHandle Handle

data DBusContext = DBusContext
	{ contextHandle :: DBusHandle
	, contextSerial :: MVar Serial
	}

withHandle :: DBusContext -> (Handle -> IO a) -> IO a
withHandle (contextHandle -> (DBusHandle h)) f = f h

hGet :: DBusContext -> Int -> IO ByteString
hGet ctx i = withHandle ctx (\h -> BC.hGet h i)

hPut :: DBusContext -> ByteString -> IO ()
hPut ctx b = withHandle ctx (\h -> BC.hPut h b)

hPuts :: DBusContext -> [ByteString] -> IO ()
hPuts ctx bs = withHandle ctx (\h -> L.hPut h $ L.fromChunks bs)

hGetLine :: DBusContext -> IO ()
hGetLine ctx = withHandle ctx BC.hGetLine >> return ()

authenticateUID :: DBusContext -> Int -> IO ()
authenticateUID ctx uid = authenticate ctx hexencoded_uid
	where
		hexencoded_uid = BC.pack $ concatMap (hex2 . ord) $ show uid
		hex2 a
			| a < 0x10  = "0" ++ showHex a ""
			| otherwise = showHex a ""

authenticate :: DBusContext -> ByteString -> IO ()
authenticate ctx auth = do
	hPut ctx $ BC.concat ["\0AUTH EXTERNAL ", auth, "\r\n"]
	_ <- hGetLine ctx
	hPut ctx "BEGIN\r\n"

close :: DBusHandle -> IO ()
close (DBusHandle h) = hClose h

connectUnix :: ByteString -> IO DBusHandle
connectUnix addr = do
	let sockaddr = SockAddrUnix $ BC.unpack addr
	sock <- socket AF_UNIX Stream 0
	connect sock sockaddr
	h <- socketToHandle sock ReadWriteMode
	hSetBuffering h NoBuffering
	return $ DBusHandle h

connectOver :: ByteString -> [(ByteString, ByteString)] -> IO DBusHandle
connectOver "unix" flags = do
	let abstract = lookup "abstract" flags
	case abstract of
		Nothing   -> error "no abstract path, use the normal path ..."
		Just path -> connectUnix $ BC.concat ["\x00", path]

connectOver _ _ = do
	error "not implemented yet"

connectSessionAt :: ByteString -> IO DBusHandle
connectSessionAt addr = do
	let (domain, flagstr) = second BC.tail $ BC.breakSubstring ":" addr
	let flags = map (\x -> let (k:v:[]) = BC.split '=' x in (k,v)) $ BC.split ',' flagstr
	connectOver domain flags

-- | connect to the dbus session bus define by the environment variable DBUS_SESSION_BUS_ADDRESS
connectSession :: IO DBusHandle
connectSession = BC.pack <$> getEnv "DBUS_SESSION_BUS_ADDRESS" >>= connectSessionAt

-- | connect to the dbus system bus
connectSystem :: IO DBusHandle
connectSystem = connectUnix "/var/run/dbus/system_bus_socket"

-- | connect onto a previously open handle
connectHandle :: Handle -> IO DBusHandle
connectHandle h = return $ DBusHandle h

-- | create a new DBus context from a ini function to create a dbusHandle.
--withContext :: IO DBusHandle -> DBusContext a -> IO a
--withContext ini f = bracket ini close (\h -> evalStateT f (h,1))
contextNew :: DBusHandle -> IO DBusContext
contextNew h = liftM (DBusContext h) (newMVar 1)

-- | create a new DBus context on session bus
busGetSession :: IO DBusContext 
busGetSession = connectSession >>= contextNew

-- | create a new DBus context on system bus
busGetSystem :: IO DBusContext
busGetSystem = connectSystem >>= contextNew

-- | get the next serial usable, and increment the serial state.
busGetNextSerial :: DBusContext -> IO Serial
busGetNextSerial ctx =
	modifyMVar (contextSerial ctx) (\v -> return $! (v+1, v))

-- | send one message to the bus with a predefined serial number.
messageSendWithSerial :: DBusContext -> Serial -> Message -> IO ()
messageSendWithSerial ctx serial msg = do
	let fieldstr = writeFields (msgFields msg)
	let fieldlen = BC.length fieldstr
	let alignfields = alignVal 8 fieldlen - fieldlen
	let header = (headerFromMessage msg)
		{ headerBodyLength   = BC.length $ msgBody msg
		, headerFieldsLength = fieldlen
		, headerSerial       = serial }
	hPuts ctx [ writeHeader header, fieldstr, BC.replicate alignfields '\0', msgBody msg ]

-- | send one message to the bus
-- note that the serial of the message sent is allocated here.
messageSend :: DBusContext -> Message -> IO Serial
messageSend ctx msg = do
	serial <- busGetNextSerial ctx
	messageSendWithSerial ctx serial msg
	return serial

-- | receive one single message from the bus
-- it is not necessarily the reply from a previous sent message.
messageRecv :: DBusContext -> IO Message
messageRecv ctx = do
	hdr    <- readHeader <$> hGet ctx 16
	fields <- readFields <$> hGet ctx (alignVal 8 $ headerFieldsLength hdr)
	body   <- hGet ctx (headerBodyLength hdr)
	return $ (messageFromHeader hdr) { msgFields = fields, msgBody = body }

alignVal :: Int -> Int -> Int
alignVal n x
	| x `mod` n == 0 = x
	| otherwise      = x + (n - (x `mod` n))
