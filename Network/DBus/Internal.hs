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
	) where

import Data.Data
import Data.String
import qualified Data.ByteString.UTF8 as UTF8
import Data.ByteString (ByteString)

newtype ObjectPath = ObjectPath { unObjectPath :: ByteString }
	deriving (Eq,Ord,Data,Typeable)
instance Show ObjectPath where
	show = show . UTF8.toString . unObjectPath
instance IsString ObjectPath where
	fromString = ObjectPath . UTF8.fromString
