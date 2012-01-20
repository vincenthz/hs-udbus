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
	, msgDbusHello
	) where

import Network.DBus.Message

dbusDestination = "org.freedesktop.DBus"
dbusPath        = "/org/freedesktop/DBus"
dbusInterface   = "org.freedesktop.DBus"

msgDbusHello = msgMethodCall dbusDestination dbusPath dbusInterface "Hello" []
