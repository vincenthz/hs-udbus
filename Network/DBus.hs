{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Network.DBus
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DBus
	( runMainLoop
	, runMainLoopCatchall
	, call
	, calltableFromList
	, registerPath
	, unregisterPath
	) where

import Network.DBus.Actions
import Network.DBus.Message
import Network.DBus.MessageType
import Network.DBus.StdMessage
import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import qualified Data.Map as M
import System.Posix.User (getRealUserID)

type MessageVar  = MVar DBusMessage

newMessageVar = newEmptyMVar

type Callback = Serial -> Signature -> Body -> IO ()
type CallTable = M.Map Member [(Interface, Callback)]

calltableFromList :: [ (Member, Interface, Callback) ] -> CallTable
calltableFromList = foldl f M.empty where
	f acc (member, intf, callback) = M.alter (appendOrCreate intf callback) member acc
	appendOrCreate intf callback Nothing  = Just [(intf,callback)]
	appendOrCreate intf callback (Just l) = Just ((intf,callback) : l)

-- | opaque type representing a connection to DBus and a receiving dispatcher thread.
-- maintain table to route message between handlers.
data DBusConnection = DBusConnection
	{ connectionContext         :: DBusContext
	, connectionCallbacks       :: MVar (M.Map Serial MessageVar)
	, connectionPaths           :: MVar (M.Map ObjectPath CallTable)
	, connectionSignals         :: DBusSignal -> IO ()
	, connectionDefaultCallback :: DBusMessage -> IO ()
	, connectionMainLoop        :: MVar ThreadId
	}

sendLock con f = f
--recvLock con f = f

call :: DBusConnection -> BusName -> DBusCall -> IO DBusReturn
call con destination c = do
	let msg = messageMapFields (fieldsSetDestination destination) $ toDBusMessage c
	mvar <- sendLock con $ do
		serial <- busGetNextSerial (connectionContext con)
		mvar <- registerCallback con serial
		messageSendWithSerial (connectionContext con) serial msg
		return mvar
	result <- takeMVar mvar
	case msgType result of
		TypeError  ->
			case errorFromDBusMessage result of
				Nothing  -> throwIO invalidException
				Just err -> throwIO err
		TypeMethodReturn ->
			case fromDBusMessage result of
				Nothing  -> throwIO invalidException
				Just ret -> return ret
		_                -> throwIO invalidException
	where
		invalidException = DBusError 0 "org.freedesktop.dbus.invalidpacket" [DBusString "invalid packet received. missing fields"]
		errorFromDBusMessage :: DBusMessage -> Maybe DBusError
		errorFromDBusMessage = fromDBusMessage

reply con rep = do
	let msg = toDBusMessage rep
	sendLock con $ messageSend (connectionContext con) msg

registerCallback con serial = do
	mvar <- newMessageVar
	modifyMVar_ (connectionCallbacks con) (return . M.insert serial mvar)
	return mvar

registerPath con path callTable =
	modifyMVar_ (connectionPaths con) (return . M.insert path callTable)

unregisterPath con path =
	modifyMVar_ (connectionPaths con) (return . M.delete path)

runMainLoop = runMainLoopCatchall (\_ -> return ())

runMainLoopCatchall catchAll context = do
	callbacks <- newMVar M.empty
	paths     <- newMVar M.empty
	mainloopPid <- newEmptyMVar

	let con = DBusConnection
		{ connectionContext         = context
		, connectionCallbacks       = callbacks
		, connectionPaths           = paths
		, connectionSignals         = \_ -> return ()
		, connectionDefaultCallback = catchAll
		, connectionMainLoop        = mainloopPid
		}
	pid <- forkIO (dispatcher con)
	putMVar mainloopPid pid

	call con dbusDestination msgDBusHello
	return con

dispatcher con = forever loop where
	loop = do
		msg      <- messageRecv (connectionContext con)
		fallback <- dispatch msg
		when fallback $ notifyUser msg
	dispatch msg@(msgType -> TypeMethodCall)   = callCallback msg
	dispatch msg@(msgType -> TypeSignal)       = signalCallback msg
	dispatch msg@(msgType -> TypeMethodReturn) = withReply msg
	dispatch msg@(msgType -> TypeError)        = withReply msg
	dispatch _                                 = return False

	withReply     (fieldsReplySerial . msgFields -> Nothing) = return False
	withReply msg@(fieldsReplySerial . msgFields -> Just serial) = do
		callback <- modifyMVar (connectionCallbacks con) $ \m ->
			case M.lookup serial m of
				Just c  -> return (M.delete serial m, Just c)
				Nothing -> return (m, Nothing)
		case callback of
			Nothing -> return True
			Just c  -> putMVar c msg >> return False
	withReply _ = return False

	callCallback msg@(msgFields -> fields) =
		case (fieldsPath fields, fieldsMember fields) of
			(Just path, Just member) -> do
				calltables   <- readMVar (connectionPaths con)
				let mcallback = M.lookup path calltables >>= M.lookup member >>= getCall
				case mcallback of
					Nothing -> return True
					Just c  -> c (msgSerial msg) (fieldsSignature fields) (readBody msg) >> return False
			-- method call is not valid, so just ignore
			_                        -> return False
		where
			getCall callbackList =
				case fieldsInterface fields of
					Nothing   -> safeHead $ map snd callbackList
					Just intf -> lookup intf callbackList

	signalCallback _ =
		return True

	safeHead []    = Nothing
	safeHead (x:_) = Just x

	notifyUser = connectionDefaultCallback con

-- | use the real user UID to authenticate to DBus.
authenticateWithRealUID :: DBusContext -> IO ()
authenticateWithRealUID ctx = getRealUserID >>= authenticateUID ctx . fromIntegral
