import Text.Printf
import Data.Word
import Test.QuickCheck
import Test.QuickCheck.Test

import qualified Data.ByteString as B
import Network.DBus.Types
import Network.DBus.Wire
import Control.Monad
import Control.Applicative ((<$>))
import System.IO

genSig 0         = oneof genSimpleSig
genSig n | n > 0 = oneof (genSimpleSig ++ [ liftM SigArray subSig, liftM SigStruct (replicateM 2 subSig) ])
	where
		subSig :: Gen Sig
		subSig = genSig (n `div` 2)

genSimpleSig =
	[ return SigByte
	, return SigBoolean
	, return SigInt16
	, return SigUInt16
	, return SigInt32
	, return SigUInt32
	, return SigInt64
	, return SigUInt64
	, return SigDouble
	, return SigString
	, return SigObjectPath
	, return SigSignature
	, return SigVariant
	]

instance Arbitrary Sig where
	arbitrary = sized genSig

prop_signature_marshalling_id x = (decodeSignature $ encodeSignature x) == x

args = stdArgs { replay = Nothing, maxSuccess = 1000, maxDiscard = 2000, maxSize = 1000 }

run_test n t = putStr ("   " ++ n ++ " ... ") >> hFlush stdout >> quickCheckWith args t

main = do
	run_test "marshalling signature = id" prop_signature_marshalling_id
