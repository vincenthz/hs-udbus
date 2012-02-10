{-# LANGUAGE OverloadedStrings #-}

import Network.DBus
import Network.DBus.Actions
import Network.DBus.Message
import Network.DBus.MessageType
import Network.DBus.StdMessage
import Network.DBus.Type
import Network.DBus.Signature
import Data.Word
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent


main = do
	con <- establish busGetSession authenticateWithRealUID
	registerCall con "/" $ calltableFromList
		[ ("xyz", "example.test.dbus", \s sig b -> putStrLn (show s ++ ": " ++ show b))
		]

	forkIO $ forever $ do
		r <- call con dbusDestination msgDBusListNames
		putStrLn $ show r
		threadDelay 100000000

	call con "org.freedesktop.Notifications" $ DBusCall "/org/freedesktop/Notifications" "Notify" (Just "org.freedesktop.Notifications")
		[ DBusString "y"
		, DBusUInt32 1
		, DBusString "x"
		, DBusString "this is a string"
		, DBusString "this is a o----"
		, DBusArray (SigString) []
		, DBusArray (SigDict SigString SigVariant) []
		, DBusInt32 4000
		]


	forever $ threadDelay 10000
