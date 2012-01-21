{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Network.DBus.MessageType
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--

module Network.DBus.MessageType
	(
	-- * high level message type
	  DBusCall(..)
	, DBusReturn(..)
	, DBusError(..)
	, DBusSignal(..)
	-- serialization functions to and from DBusMessage
	, toDBusMessage
	) where

import Network.DBus.Message
import Network.DBus.Type
import Network.DBus.Signature

class DBusMessageable a where
	toDBusMessage   :: a -> DBusMessage

data DBusSignal = DBusSignal
	{ signalPath      :: ObjectPath
	, signalMember    :: Member
	, signalInterface :: Interface
	, signalBody      :: Body
	} deriving (Show,Eq)

data DBusCall = DBusCall
	{ callPath      :: ObjectPath
	, callMember    :: Member
	, callInterface :: Maybe Interface
	, callBody      :: Body
	} deriving (Show,Eq)

data DBusReturn = DBusReturn
	{ returnReplySerial :: Serial
	, returnBody        :: Body
	} deriving (Show,Eq)

data DBusError = DBusError
	{ errorReplySerial :: Serial
	, errorName        :: ErrorName
	, errorBody        :: Body
	} deriving (Show,Eq)

instance DBusMessageable DBusCall where
	toDBusMessage call = messageNew TypeMethodCall (callBody call) $
		(fieldsSetPath (callPath call) .
		maybe id fieldsSetInterface (callInterface call) .
		fieldsSetMember (callMember call))

instance DBusMessageable DBusSignal where
	toDBusMessage signal = messageNew TypeMethodCall (signalBody signal) $
		(fieldsSetPath (signalPath signal) .
		fieldsSetInterface (signalInterface signal) .
		fieldsSetMember (signalMember signal))

instance DBusMessageable DBusReturn where
	toDBusMessage return = messageNew TypeMethodCall (returnBody return) $
		fieldsSetReplySerial (returnReplySerial return)

instance DBusMessageable DBusError where
	toDBusMessage error = messageNew TypeMethodCall (errorBody error) $
		(fieldsSetReplySerial (errorReplySerial error)
		. fieldsSetErrorName (errorName error))
