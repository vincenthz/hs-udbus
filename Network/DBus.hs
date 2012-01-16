{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Network.DBus
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DBus
	( DBusHandle
	, authenticate
	, authenticateUID

	, connectSession
	, connectSystem
	, connectHandle

	, withContext
	, withSession
	, withSystem

	, messageSend
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
	, DbusType(..)
	) where

import Numeric (showHex)
import Data.Char (ord)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L

import Control.Arrow
import Control.Applicative ((<$>))
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

type DBusContext a = StateT (DBusHandle, Serial) IO a

withHandle :: (Handle -> IO a) -> DBusContext a
withHandle f = do
	(DBusHandle h) <- fst <$> get
	liftIO (f h)

hGet :: Int -> DBusContext ByteString
hGet i = withHandle (\h -> BC.hGet h i)

hPut :: ByteString -> DBusContext ()
hPut b = withHandle (\h -> BC.hPut h b)

hPuts :: [ByteString] -> DBusContext ()
hPuts bs = withHandle (\h -> L.hPut h $ L.fromChunks bs)

hGetLine :: DBusContext ()
hGetLine = withHandle BC.hGetLine >> return ()

authenticateUID :: Int -> DBusContext ()
authenticateUID uid = authenticate hexencoded_uid
	where
		hexencoded_uid = BC.pack $ concatMap (hex2 . ord) $ show uid
		hex2 a
			| a < 0x10  = "0" ++ showHex a ""
			| otherwise = showHex a ""

authenticate :: ByteString -> DBusContext ()
authenticate auth = do
	hPut $ BC.concat ["\0AUTH EXTERNAL ", auth, "\r\n"]
	_ <- hGetLine
	hPut "BEGIN\r\n"

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

-- | create a new Dbus context from a ini function to create a dbusHandle.
withContext :: IO DBusHandle -> DBusContext a -> IO a
withContext ini f = bracket ini close (\h -> evalStateT f (h,1))

-- | create a new Dbus context on session bus
withSession :: DBusContext a -> IO a
withSession = withContext connectSession

-- | create a new Dbus context on system bus
withSystem :: DBusContext a -> IO a
withSystem = withContext connectSystem

-- | send one message to the bus
-- note that the serial of the message sent is allocated here.
messageSend :: Message -> DBusContext Serial
messageSend msg = do
	serial <- snd <$> get
	modify (\(h,_) -> (h, serial+1))
	let fieldstr = writeFields (msgFields msg)
	let fieldlen = BC.length fieldstr
	let alignfields = alignVal 8 fieldlen - fieldlen
	let header = (headerFromMessage msg)
		{ headerBodyLength   = BC.length $ msgBody msg
		, headerFieldsLength = fieldlen
		, headerSerial       = serial }
	hPuts [ writeHeader header, fieldstr, BC.replicate alignfields '\0', msgBody msg ]
	return serial

-- | receive one single message from the bus
-- it is not necessarily the reply from a previous sent message.
messageRecv :: DBusContext Message
messageRecv = do
	hdr    <- readHeader <$> hGet 16
	fields <- readFields <$> hGet (alignVal 8 $ headerFieldsLength hdr)
	body   <- hGet (headerBodyLength hdr)
	return $ (messageFromHeader hdr) { msgFields = fields, msgBody = body }

alignVal :: Int -> Int -> Int
alignVal n x
	| x `mod` n == 0 = x
	| otherwise      = x + (n - (x `mod` n))
