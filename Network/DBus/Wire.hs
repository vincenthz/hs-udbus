{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module      : Network.DBus.Wire
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DBus.Wire
    ( DBusEndian(..)
    -- * getter
    , GetWire
    , getWire
    , isWireEmpty
    , alignRead
    , getw8
    , getw16
    , getw32
    , getw64
    , getString
    , getSignature
    , getVariant
    , getObjectPath
    , getBytes
    , getMultiple
    -- * putter
    , PutWire
    , putWire
    , putWireAt
    , putWireGetPosition
    , putBytes
    , alignWrite
    , alignWriteCalculate
    , putw8
    , putw16
    , putw32
    , putw64
    , putString
    , putSignature
    , putVariant
    , putObjectPath
    ) where

import Data.Word
import Data.Bits
import Data.Binary.Get hiding (getBytes)
import Data.ByteString (ByteString)
import Data.String
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Control.Applicative ((<$>))
import Control.Monad.Reader
import Control.Monad.State
import Network.DBus.Signature
import Network.DBus.Internal

data DBusEndian = LE | BE deriving (Show,Eq)
type DBusGet = (DBusEndian, Int) -- Specified endianness and alignment of this context.

newtype GetWire a = GetWire { runGW :: ReaderT DBusGet Get a }
    deriving (Monad, MonadReader DBusGet, Functor)

getWire :: DBusEndian -> Int -> GetWire a -> ByteString -> a
getWire endian align f b = runGet (runReaderT (runGW f) (endian,align)) (L.fromChunks [b])

liftGet :: Get a -> GetWire a
liftGet = GetWire . lift

isWireEmpty :: GetWire Bool
isWireEmpty = liftGet isEmpty

onEndian :: GetWire a -> GetWire a -> GetWire a
onEndian lef bef = ask >>= \(e, _) -> if e == LE then lef else bef

alignRead :: Int -> GetWire ()
alignRead n = do
    (_, start) <- ask
    br         <- liftGet (fromIntegral <$> bytesRead)
    case (br + start) `mod` n of
        0 -> return ()
        i -> liftGet (skip $ n - i)

getw8 :: GetWire Word8
getw8 = liftGet getWord8

getw16 :: GetWire Word16
getw16 = alignRead 2 >> onEndian (liftGet getWord16le) (liftGet getWord16be)

getw32 :: GetWire Word32
getw32 = alignRead 4 >> onEndian (liftGet getWord32le) (liftGet getWord32be)

getw64 :: GetWire Word64
getw64 = alignRead 8 >> onEndian (liftGet getWord64le) (liftGet getWord64be)

getSignatureOne :: GetWire Type
getSignatureOne = do
    sigs <- getSignature
    case sigs of
        [s] -> return s
        _   -> error "one signature with wrong format"

getSignature :: GetWire Signature
getSignature = do
    len   <- fromIntegral <$> getw8
    sigBS <- liftGet $ getByteString len
    _     <- getw8
    case unserializeSignature sigBS of
        Left err  -> error err
        Right sig -> return sig

getVariant :: GetWire Type
getVariant = getSignatureOne

getBytes = liftGet . getByteString

getString :: GetWire PackedString
getString = do
    nbBytes <- fromIntegral <$> getw32
    s       <- liftGet $ getByteString nbBytes
    _       <- getw8
    return $ PackedString s

getObjectPath :: GetWire ObjectPath
getObjectPath = ObjectPath . packedStringToString <$> getString

getMultiple :: Show a => Int -> GetWire a -> GetWire [a]
getMultiple 0 _ = return []
getMultiple n f = do
    r1 <- liftGet remaining
    a <- f
    r2 <- liftGet remaining
    let r = fromIntegral (r1-r2)
    liftM (a :) (getMultiple (n-r) f)

type PutWireM a = State (Int, [ByteString]) a
type PutWire = PutWireM ()

putWireGetPosition :: PutWireM Int
putWireGetPosition = gets fst

putWireAt :: Int -> [PutWire] -> ByteString
putWireAt i f = B.concat $ reverse $ snd $ execState (sequence_ f) (i, [])

putWire :: [PutWire] -> ByteString
putWire = putWireAt 0

putBytes :: ByteString -> PutWire
putBytes s = modify (\(i, l) -> (i + B.length s, s : l))

alignWriteCalculate :: Int -> Int -> Int
alignWriteCalculate n pos = negMod $ pos `mod` n
    where
        negMod 0 = 0
        negMod x = n - x

alignWrite :: Int -> PutWire
alignWrite n = gets (alignWriteCalculate n . fst) >>= \l -> putBytes $ B.replicate l 0

putw8 :: Word8 -> PutWire
putw8 = putBytes . B.singleton

putw16 :: Word16 -> PutWire
putw16 w = alignWrite 2 >> putBytes (B.pack le)
    where
        le = [p2,p1]
        --be = [p1,p2]
        p1 = fromIntegral $ w `shiftR` 8
        p2 = fromIntegral w

putw32 :: Word32 -> PutWire
putw32 w = alignWrite 4 >> putBytes (B.pack le)
    where
        le = [p4,p3,p2,p1]
        --be = [p1,p2,p3,p4]
        p1 = fromIntegral $ w `shiftR` 24
        p2 = fromIntegral $ w `shiftR` 16
        p3 = fromIntegral $ w `shiftR` 8
        p4 = fromIntegral w

putw64 :: Word64 -> PutWire
putw64 w = alignWrite 8 >> putBytes (B.pack le)
    where
        le = [p8,p7,p6,p5,p4,p3,p2,p1]
        --be = [p1,p2,p3,p4,p5,p6,p7,p8]
        p1 = fromIntegral $ w `shiftR` 56
        p2 = fromIntegral $ w `shiftR` 48
        p3 = fromIntegral $ w `shiftR` 40
        p4 = fromIntegral $ w `shiftR` 32
        p5 = fromIntegral $ w `shiftR` 24
        p6 = fromIntegral $ w `shiftR` 16
        p7 = fromIntegral $ w `shiftR` 8
        p8 = fromIntegral w

putString :: PackedString -> PutWire
putString (PackedString b) = do
    putw32 (fromIntegral $ B.length b)
    putBytes b
    putw8 0

putSignature :: Signature -> PutWire
putSignature sig = do
    putw8 (fromIntegral $ B.length b)
    putBytes b
    putw8 0
    where b = serializeSignature sig

putVariant :: Type -> PutWire
putVariant = putSignature . (:[])

putObjectPath :: ObjectPath -> PutWire
putObjectPath = putString . fromString . unObjectPath
