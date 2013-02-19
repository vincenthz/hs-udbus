{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
    , DBusMessageable(..)
    ) where

import Control.Exception
import Data.Data
import Network.DBus.Message
import Network.DBus.Type

class DBusMessageable a where
    toDBusMessage   :: a -> DBusMessage
    fromDBusMessage :: DBusMessage -> Maybe a

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
    } deriving (Show,Eq,Data,Typeable)

instance Exception DBusError

instance DBusMessageable DBusCall where
    toDBusMessage call = messageNew TypeMethodCall (callBody call) $
        (fieldsSetPath (callPath call) .
        maybe id fieldsSetInterface (callInterface call) .
        fieldsSetMember (callMember call))
    fromDBusMessage msg@(msgFields -> fields) =
        case (fieldsPath fields, fieldsMember fields) of
            (Just path, Just member) -> Just $ DBusCall path member (fieldsInterface fields) (readBody msg)
            _                        -> Nothing

instance DBusMessageable DBusSignal where
    toDBusMessage signal = messageNew TypeSignal (signalBody signal) $
        (fieldsSetPath (signalPath signal) .
        fieldsSetInterface (signalInterface signal) .
        fieldsSetMember (signalMember signal))
    fromDBusMessage msg@(msgFields -> fields) =
        case (fieldsPath fields, fieldsMember fields, fieldsInterface fields) of
            (Just path, Just member, Just intf) -> Just $ DBusSignal path member intf (readBody msg)
            _                                   -> Nothing

instance DBusMessageable DBusReturn where
    toDBusMessage r = messageNew TypeMethodReturn (returnBody r) $
        fieldsSetReplySerial (returnReplySerial r)
    fromDBusMessage msg@(msgFields -> fields) =
        case fieldsReplySerial fields of
            Just rserial -> Just $ DBusReturn rserial (readBody msg)
            _            -> Nothing

instance DBusMessageable DBusError where
    toDBusMessage e = messageNew TypeError (errorBody e) $
        (fieldsSetReplySerial (errorReplySerial e)
        . fieldsSetErrorName (errorName e))
    fromDBusMessage msg@(msgFields -> fields) =
        case (fieldsReplySerial fields, fieldsErrorName fields) of
            (Just rserial, Just errname) -> Just $ DBusError rserial errname (readBody msg)
            _                            -> Nothing
