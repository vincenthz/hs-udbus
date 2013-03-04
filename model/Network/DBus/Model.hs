-- |
-- Module      : Network.DBus.Model
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Model parsing for introspections and definition
-- of DBus interfaces.
module Network.DBus.Model
    ( module Network.DBus.Model.Types
    , fromXML
    ) where

import Network.DBus.Model.Types
import Network.DBus.Model.Parse
