{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Network.DBus
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DBus
	(
	-- * handle connections to DBus
	  establish
	, establishWithCatchall
	, DBusConnection
	-- * Types
	, DBusMessageable(..)
	, DBusCall(..)
	, DBusReturn(..)
	, DBusError(..)
	, DBusSignal(..)
	, DBusValue(..)
	, DBusType(..)
	, ObjectPath(..)
	, PackedString(..)
	, Signature
	, SignatureElem(..)
	, DBusMatchRules(..)
	, defaultDBusMatchRules
	, MessageType(..)
	-- * standard way to interact with dbus
	, addMatch
	-- * main loop creation
	, runMainLoop
	, runMainLoopCatchall
	-- * interact with the connection
	, call
	, reply
	, calltableFromList
	, registerPath
	, unregisterPath
	, registerCall
	, unregisterCall
	, registerSignal
	, unregisterSignal
	-- * create a new context on system or session bus
	, busGetSystem
	, busGetSession
	-- * authenticate methods available
	, authenticate
	, authenticateUID
	, authenticateWithRealUID
	) where

import Network.DBus.Actions
import Network.DBus.Message
import Network.DBus.MessageType
import Network.DBus.StdMessage
import Network.DBus.Internal
import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.MVar
import Control.Exception
import Data.Maybe
import Control.Monad
import qualified Data.Map as M
import Data.List (intercalate)
import System.Posix.User (getRealUserID)

type MessageVar  = MVar DBusMessage

newMessageVar = newEmptyMVar

type Signalback = BusName -> Signature -> Body -> IO ()
type Callback   = Serial -> Signature -> Body -> IO ()
type DispatchTable a = M.Map Member [(Interface, a)]

calltableFromList :: [ (Member, Interface, a) ] -> DispatchTable a
calltableFromList = foldl f M.empty where
	f acc (member, intf, callback) = M.alter (appendOrCreate intf callback) member acc
	appendOrCreate intf callback Nothing  = Just [(intf,callback)]
	appendOrCreate intf callback (Just l) = Just ((intf,callback) : l)

-- | opaque type representing a connection to DBus and a receiving dispatcher thread.
-- maintain table to route message between handlers.
data DBusConnection = DBusConnection
	{ connectionContext         :: DBusContext
	, connectionSendLock        :: MVar ()
	, connectionCallbacks       :: MVar (M.Map Serial MessageVar)
	, connectionPaths           :: MVar (M.Map ObjectPath (DispatchTable Callback))
	, connectionSignals         :: MVar (M.Map ObjectPath (DispatchTable Signalback))
	, connectionDefaultCallback :: DBusMessage -> IO ()
	, connectionMainLoop        :: MVar ThreadId
	}

data DBusMatchRules = DBusMatchRules
	{ matchType        :: Maybe MessageType
	, matchSender      :: Maybe BusName
	, matchInterface   :: Maybe Interface
	, matchMember      :: Maybe Member
	, matchPath        :: Maybe ObjectPath
	, matchDestination :: Maybe BusName
	}

defaultDBusMatchRules = DBusMatchRules Nothing Nothing Nothing Nothing Nothing Nothing

sendLock con f = withMVar (connectionSendLock con) $ \() -> f

-- | Establish a new connection to dbus, using the two functions to
-- first establish a new context, and second to authenticate to the bus.
-- this will automatically create a mainloop thread.
establish :: IO DBusContext         -- ^ function to create a new dbus context (busGetSystem or busGetSession)
          -> (DBusContext -> IO ()) -- ^ function to authenticate to dbus
          -> IO DBusConnection
establish createContext auth = do
	ctx <- createContext
	auth ctx
	runMainLoop ctx

establishWithCatchall catchall createContext auth = do
	ctx <- createContext
	auth ctx
	runMainLoopCatchall catchall ctx

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
		invalidException = DBusError 0 "org.freedesktop.dbus.invalidpacket"
			[DBusString "invalid packet received. missing fields"]
		errorFromDBusMessage :: DBusMessage -> Maybe DBusError
		errorFromDBusMessage = fromDBusMessage

addMatch :: DBusConnection -> DBusMatchRules -> IO ()
addMatch con mr = call con dbusDestination (msgDBusAddMatch serialized) >> return ()
	where
		serialized = intercalate "," $ filter (not . null)
			[ mm "type"        show $ matchType mr
			, mm "sender"      id $ matchSender mr
			, mm "interface"   id $ matchInterface mr
			, mm "member"      id $ matchMember mr
			, mm "path"        unObjectPath $ matchPath mr
			, mm "destination" id $ matchDestination mr
			]
		mm key f = maybe "" (surroundQuote key . f)
		surroundQuote key v = concat [ key, "='",  v, "'" ]

reply con rep = do
	let msg = toDBusMessage rep
	sendLock con $ messageSend (connectionContext con) msg

registerCallback con serial = do
	mvar <- newMessageVar
	modifyMVar_ (connectionCallbacks con) (return . M.insert serial mvar)
	return mvar

registerPath_ f con path callTable =
	modifyMVar_ (f con) (return . M.insert path callTable)
unregisterPath_ f con path =
	modifyMVar_ (f con) (return . M.delete path)

registerCall = registerPath_ connectionPaths
unregisterCall = unregisterPath_ connectionPaths

{-# DEPRECATED registerPath "use registerCall" #-}
registerPath = registerPath_ connectionPaths
{-# DEPRECATED unregisterPath "use unregisterCall" #-}
unregisterPath = unregisterPath_ connectionPaths

registerSignal = registerPath_ connectionSignals
unregisterSignal = unregisterPath_ connectionSignals

runMainLoop = runMainLoopCatchall (\_ -> return ())

runMainLoopCatchall catchAll context = do
	callbacks <- newMVar M.empty
	callPaths   <- newMVar M.empty
	signalPaths <- newMVar M.empty
	mainloopPid <- newEmptyMVar
	sendLockVar <- newMVar ()

	let con = DBusConnection
		{ connectionContext         = context
		, connectionCallbacks       = callbacks
		, connectionPaths           = callPaths
		, connectionSignals         = signalPaths
		, connectionDefaultCallback = catchAll
		, connectionMainLoop        = mainloopPid
		, connectionSendLock        = sendLockVar
		}
	pid <- forkIO (dispatcher con)
	putMVar mainloopPid pid

	_ <- call con dbusDestination msgDBusHello
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
				let mcallback = findDispatchTable calltables path member (fieldsInterface fields)
				case mcallback of
					Nothing -> return True
					Just c  -> c (msgSerial msg) (fieldsSignature fields) (readBody msg) >> return False
			-- method call is not valid, so just ignore
			_                        -> return False

	signalCallback msg@(msgFields -> fields) =
		case (fieldsPath fields, fieldsMember fields) of
			(Just path, Just member) -> do
				signaltables <- readMVar (connectionSignals con)
				let mcallback = findDispatchTable signaltables path member (fieldsInterface fields)
				case mcallback of
					Nothing -> return True
					Just c  -> c (fromJust $ fieldsSender fields) (fieldsSignature fields) (readBody msg) >> return False
			-- signal is not valid, so just ignore
			_                        -> return False

	findDispatchTable table path member intf = M.lookup path table >>= M.lookup member >>= getCall where
		getCall callbackList = case intf of
			Nothing   -> safeHead $ map snd callbackList
			Just i    -> lookup i callbackList

	safeHead []    = Nothing
	safeHead (x:_) = Just x

	notifyUser = connectionDefaultCallback con

-- | use the real user UID to authenticate to DBus.
authenticateWithRealUID :: DBusContext -> IO ()
authenticateWithRealUID ctx = getRealUserID >>= authenticateUID ctx . fromIntegral
