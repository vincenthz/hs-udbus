{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : Network.DBus.Internal
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--

module Network.DBus.Internal
	( ObjectPath(..)
	, PackedString(..)
	) where

import Data.Data
import Data.String
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8

newtype ObjectPath = ObjectPath { unObjectPath :: String }
	deriving (Eq,Ord,Data,Typeable)
instance Show ObjectPath where
	show = unObjectPath
instance IsString ObjectPath where
	fromString = ObjectPath

newtype PackedString = PackedString { ustringToBS :: ByteString }
	deriving (Eq,Ord,Data,Typeable)
instance IsString PackedString where
	fromString = PackedString . UTF8.fromString
instance Show PackedString where
	show = UTF8.toString . ustringToBS
