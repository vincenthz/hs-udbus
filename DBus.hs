{-# LANGUAGE OverloadedStrings #-}

import System.Posix.User
import Network.DBus
import Network.DBus.Actions
import Network.DBus.Message
import Network.DBus.StdMessage
import Network.DBus.Type
import Network.DBus.Signature
import Data.Word
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent


mainDBus uid = do
	ctx <- busGetSession
	authenticateUID ctx uid

	con <- runMainLoopCatchall (\msg -> putStrLn $ show msg) ctx
	registerPath con "/" $ calltableFromList
		[ ("xyz", "example.test.dbus", \s sig b -> putStrLn (show s ++ ": " ++ show b))
		]

	forkIO $ forever $ do
		r <- call con dbusDestination msgDBusListNames
		putStrLn $ show r
		threadDelay 100000000

	forever $ threadDelay 10000

main = getRealUserID >>= mainDBus . fromIntegral
