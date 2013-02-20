import Text.Printf
import Data.String
import Test.QuickCheck
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Data.ByteString as B ()
import Network.DBus.Type
import Network.DBus.Signature
import Network.DBus.Internal
import Network.DBus.Message
import Network.DBus.Wire
import Control.Monad
import Control.Applicative ((<$>))

-----------------------------------------------------------------------------------
genSimpleSig =
    [ return SigByte
    , return SigBool
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

genSigElem :: Int -> Gen Type
genSigElem 0 = oneof genSimpleSig
genSigElem n = oneof (genSimpleSig ++ [ liftM SigArray subSig, liftM SigStruct (replicateM 2 subSig) ])
    where
        subSig :: Gen Type
        subSig = genSigElem (n `div` 2)

genSig :: Gen Signature
genSig = replicateM 4 (resize 2 $ sized genSigElem)

-- just there to make it instance of arbitrary without having typesynonym
newtype DSignature = DSignature Signature
    deriving (Eq)

instance Show DSignature where
    show (DSignature x) = show x
instance Arbitrary DSignature where
    arbitrary = DSignature <$> genSig
-----------------------------------------------------------------------------------

instance Arbitrary PackedString where
    arbitrary = fromString <$> arbitrary

instance Arbitrary ObjectPath where
    arbitrary = ObjectPath <$> arbitrary

genBody :: Gen (Signature, [DBusValue])
genBody = genSig >>= \sig -> (mapM sigToBody sig >>= \body ->return (sig,body)) where
    sigToBody SigByte       = DBusByte <$> arbitrary
    sigToBody SigBool       = DBusBoolean <$> arbitrary
    sigToBody SigInt16      = DBusInt16 <$> arbitrary
    sigToBody SigUInt16     = DBusUInt16 <$> arbitrary
    sigToBody SigInt32      = DBusInt32 <$> arbitrary
    sigToBody SigUInt32     = DBusUInt32 <$> arbitrary
    sigToBody SigInt64      = DBusInt64 <$> arbitrary
    sigToBody SigUInt64     = DBusUInt64 <$> arbitrary
    sigToBody SigDouble     = DBusDouble <$> arbitrary
    sigToBody SigObjectPath = DBusObjectPath <$> arbitrary
    sigToBody SigString     = DBusString <$> arbitrary
    sigToBody SigVariant    = sized genSigElem >>= sigToBody >>= return . DBusVariant
    sigToBody SigSignature  = DBusSignature <$> genSig
    sigToBody (SigStruct sigs) = mapM sigToBody sigs >>= return . DBusStruct sigs
    sigToBody (SigArray sig) = mapM sigToBody (replicate 3 sig) >>= return . DBusArray sig
    sigToBody _              = DBusString <$> arbitrary

data BodyContent = BodyContent Signature [DBusValue]
    deriving (Show,Eq)
instance Arbitrary BodyContent where
    arbitrary = genBody >>= \(sig, val) -> return $ BodyContent sig val
-----------------------------------------------------------------------------------

property_signature_marshalling (DSignature x) = (unserializeSignature $ serializeSignature x) == Right x
property_body_marshalling (BodyContent sig c) = (readBodyRaw LE sig $ writeBody c) `assertEq` c

assertEq :: (Show a, Eq a) => a -> a -> Bool
assertEq a b
    | a == b    = True
    | otherwise = error ("equality failed:\ngot: " ++ show a ++ "\nexpected " ++ show b)

main = defaultMain tests where
    tests = [testMarshalling]
    testMarshalling = testGroup "Marshalling"
        [ testProperty "Signature" property_signature_marshalling
        , testProperty "Values" property_body_marshalling
        ]
