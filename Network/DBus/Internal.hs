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

newtype ObjectPath = ObjectPath { unObjectPath :: String }
	deriving (Eq,Ord,Data,Typeable)
instance Show ObjectPath where
	show = unObjectPath
instance IsString ObjectPath where
	fromString = ObjectPath
