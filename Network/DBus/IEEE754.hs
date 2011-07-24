{-# LANGUAGE MagicHash #-}
module Network.DBus.IEEE754 (encode, decode) where

import GHC.Prim
import GHC.Types
import GHC.Word

-- | encode a double to a IEEE754 format
encode :: Double -> Word64
encode (D# x) = W64# (unsafeCoerce# x)

-- | decode a double from a IEEE754 format
decode :: Word64 -> Double
decode (W64# x) = D# (unsafeCoerce# x)
