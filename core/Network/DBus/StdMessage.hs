{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Network.DBus.StdMessage
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DBus.StdMessage
    (
    -- * dbus server standard interface
      dbusDestination
    , dbusPath
    , dbusInterface
    -- * dbus standard message
    , msgDBusHello
    , msgDBusListNames
    , msgDBusAddMatch
    ) where

import Network.DBus.Message
import Network.DBus.MessageType
import Network.DBus.Type
import Data.String

dbusDestination :: BusName
dbusDestination = "org.freedesktop.DBus"
dbusPath        = "/org/freedesktop/DBus"
dbusInterface   = "org.freedesktop.DBus"

msgDBusHello = DBusCall dbusPath "Hello" (Just dbusInterface) []
msgDBusListNames = DBusCall dbusPath "ListNames" (Just dbusInterface) []

msgDBusAddMatch matchingRule = DBusCall dbusPath "AddMatch" (Just dbusInterface) [ DBusString $ fromString matchingRule ]
