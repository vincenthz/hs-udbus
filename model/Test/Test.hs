module Main where

import Control.Applicative

import qualified Network.DBus as DBus
import qualified Network.DBus.Actions as DBus

import qualified Network.DBus.Model as Model

import Text.Groom
import System.Environment

main = do
    args  <- getArgs
    model <- Model.fromXML <$> readFile (args !! 0)
    case model of
        Nothing -> putStrLn "parse failed"
        Just m  -> putStrLn $ groom m
