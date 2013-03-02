{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : Network.DBus.Signature
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DBus.Signature
    ( Signature
    , SignatureElem
    , Type(..)
    -- * serialization
    , serializeSignature
    , unserializeSignature
    ) where

import Data.Char (chr, ord)
import Data.Data
import Data.Serialize.Get
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Monad
import Control.Applicative ((<$>))

-- | One possible signature element
data Type =
      SigByte
    | SigBool
    | SigInt16
    | SigUInt16
    | SigInt32
    | SigUInt32
    | SigInt64
    | SigUInt64
    | SigDouble
    | SigString
    | SigObjectPath
    | SigSignature
    | SigArray Type
    | SigStruct [Type]
    | SigVariant
    | SigDict Type Type
    | SigUnixFD
    deriving (Show,Eq,Data,Typeable)

{-# DEPRECATED SignatureElem "use Type instead" #-}
type SignatureElem = Type

-- | A list of signature element
type Signature = [Type]

marshallString :: String -> ByteString
marshallString = B.pack . map (fromIntegral . ord)

marshallSignatureElem :: Type -> ByteString
marshallSignatureElem SigByte       = marshallString "y"
marshallSignatureElem SigBool       = marshallString "b"
marshallSignatureElem SigInt16      = marshallString "n"
marshallSignatureElem SigUInt16     = marshallString "q"
marshallSignatureElem SigInt32      = marshallString "i"
marshallSignatureElem SigUInt32     = marshallString "u"
marshallSignatureElem SigInt64      = marshallString "x"
marshallSignatureElem SigUInt64     = marshallString "t"
marshallSignatureElem SigDouble     = marshallString "d"
marshallSignatureElem SigString     = marshallString "s"
marshallSignatureElem SigObjectPath = marshallString "o"
marshallSignatureElem SigSignature  = marshallString "g"
marshallSignatureElem SigVariant    = marshallString "v"
marshallSignatureElem SigUnixFD     = marshallString "h"
marshallSignatureElem (SigArray t)  = B.concat [marshallString "a", marshallSignatureElem t]
marshallSignatureElem (SigStruct l) = B.concat ([marshallString "("] ++ map marshallSignatureElem l ++ [marshallString ")"])
marshallSignatureElem (SigDict k v) = B.concat [marshallString "{", marshallSignatureElem k, marshallSignatureElem v, marshallString "}"]

-- | serialize a signature
serializeSignature :: Signature -> ByteString
serializeSignature elements = B.concat $ map marshallSignatureElem elements

data SigStop = StopStruct | StopDict deriving (Eq)

-- | unserialize a signature
unserializeSignature :: ByteString -> Either String Signature
unserializeSignature = runGet loop where
    loop :: Get Signature
    loop = do
        r <- remaining
        if r > 0
            then getOneNoCollection >>= \o -> liftM (o :) loop
            else return []
    getOneNoCollection = getOne >>= \o -> case o of
        Left _  -> error "parsing error, end of collection not in a collection"
        Right z -> return z
    getOne :: Get (Either SigStop SignatureElem)
    getOne = do
        t <- chr . fromIntegral <$> getWord8    
        case t of
            ')' -> return $ Left StopStruct
            '}' -> return $ Left StopDict
            'y' -> return $ Right SigByte
            'b' -> return $ Right SigBool
            'n' -> return $ Right SigInt16
            'q' -> return $ Right SigUInt16
            'i' -> return $ Right SigInt32
            'u' -> return $ Right SigUInt32
            'x' -> return $ Right SigInt64
            't' -> return $ Right SigUInt64
            'd' -> return $ Right SigDouble
            's' -> return $ Right SigString
            'o' -> return $ Right SigObjectPath
            'g' -> return $ Right SigSignature
            'a' -> Right <$> getArray
            '(' -> Right <$> getStruct
            'v' -> return $ Right SigVariant
            '{' -> Right <$> getDict
            'h' -> return $ Right SigUnixFD
            _   -> error ("unknown signature element: " ++ (show $ ord t))
    getArray = SigArray <$> getOneNoCollection
    getDict = do
        l <- loopTill StopDict
        case l of
            [k,v] -> return $ SigDict k v
            _     -> error "dictionary wrong size"
    getStruct = do
        l <- loopTill StopStruct
        case l of
            [] -> error "structure empty"
            _  -> return $ SigStruct l
    loopTill stop = do
        o <- getOne
        case o of
            Left s ->  if stop == s then return [] else error "collection not terminated properly"
            Right z -> liftM (z :) (loopTill stop)
