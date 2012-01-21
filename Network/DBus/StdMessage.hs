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
	) where

import Network.DBus.Message

dbusDestination = "org.freedesktop.DBus"
dbusPath        = "/org/freedesktop/DBus"
dbusInterface   = "org.freedesktop.DBus"

msgDBusHello = msgMethodCall dbusDestination dbusPath (Just dbusInterface) "Hello" []
msgDBusListNames = msgMethodCall dbusDestination dbusPath (Just dbusInterface) "ListNames" []
