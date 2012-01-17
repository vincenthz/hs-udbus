{-# LANGUAGE OverloadedStrings #-}

import System.Posix.User
import Network.DBus
import Network.DBus.Message
import Network.DBus.Type
import Network.DBus.Signature
import Data.Word
import Control.Monad.Trans

dbusDestination = "org.freedesktop.DBus"
dbusPath        = "/org/freedesktop/DBus"
dbusInterface   = "org.freedesktop.DBus"

mainDbus uid = do
	ctx <- busGetSession
	authenticateUID ctx uid
	let msg = msgMethodCall dbusDestination dbusPath dbusInterface "Hello" []
	messageSend ctx msg
	messageRecv ctx
	liftIO $ putStrLn "spurious"
	messageRecv ctx

	let msg = msgMethodCall dbusDestination dbusPath dbusInterface "ListNames" []
	messageSend ctx msg
	msg <- messageRecv ctx
	let b = readBody msg
	liftIO $ putStrLn $ show b

	let msg = msgMethodCall "org.freedesktop.Notifications" "/org/freedesktop/Notifications" "org.freedesktop.Notifications" "Notify"
		[ DbusString "y"
		, DbusUInt32 1
		, DbusString "x"
		, DbusString "this is a string"
		, DbusString "this is a o----"
		, DbusArray (SigString) []
		, DbusArray (SigDict SigString SigVariant) []
		, DbusInt32 4000
		]

	liftIO $ putStrLn $ show msg
	messageSend ctx msg
	msgR <- messageRecv ctx
	liftIO $ putStrLn $ show msgR

main = getRealUserID >>= mainDbus . fromIntegral
