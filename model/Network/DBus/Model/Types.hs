-- |
-- Module      : Network.DBus.Model.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Basic 1-to-1 mapping of XML types defined for
-- XML introspection and definition of DBus interfaces.
--
module Network.DBus.Model.Types
    ( Model(..)
    , Interface(..)
    , Enumeration(..)
    , EnumValue(..)
    , Flags(..)
    , Flag(..)
    , Struct(..)
    , Member(..)
    , Method(..)
    , Annotation(..)
    , Signal(..)
    , Property(..)
    , Arg(..)
    , Access(..)
    , Doc
    ) where

import qualified Network.DBus as DBus

-- | The whole XML model
data Model = Model
    { interfaces :: [Interface]
    } deriving (Show,Eq)

-- | An DBus XML interface containing methods, signals and properties
data Interface = Interface
    { interfaceName       :: String
    , interfaceMethods    :: [Method]
    , interfaceSignals    :: [Signal]
    , interfaceProperties :: [Property]
    , interfaceEnums      :: [Enumeration]
    , interfaceFlagss     :: [Flags]       -- ^ List of list of flag
    , interfaceStructs    :: [Struct]
    } deriving (Show,Eq)

-- | DBus Enumeration (Telepathy extension)
data Enumeration = Enumeration
    { enumName   :: String
    , enumType   :: DBus.Type
    , enumValues :: [EnumValue]
    } deriving (Show,Eq)

-- | DBus Enumeration value
data EnumValue = EnumValue
    { enumSuffix :: String
    , enumValue  :: String
    } deriving (Show,Eq)

-- | DBus Flags (Telepathy extension)
data Flags = Flags
    { flagsName        :: String
    , flagsValuePrefix :: String
    , flagsType        :: DBus.Type
    , flagsFlags       :: [Flag]
    , flagsDoc         :: Maybe Doc
    } deriving (Show,Eq)

data Flag = Flag
    { flagSuffix :: String
    , flagValue  :: String
    , flagDoc    :: Maybe Doc
    } deriving (Show,Eq)

-- | DBus Struct (Telepathy extension)
data Struct = Struct
    { structName    :: String
    , structMembers :: [Member]
    } deriving (Show,Eq)

-- | DBus Struct Member
data Member = Member
    { memberName    :: String
    , memberType    :: DBus.Type
    , memberRawType :: Maybe String
    , memberDoc     :: Maybe Doc
    } deriving (Show,Eq)

-- | Represent a DBus Method
data Method = Method
    { methodName        :: String
    , methodAnnotations :: [Annotation]
    , methodParamsIn    :: [Arg]
    , methodParamsOut   :: [Arg]
    , methodDoc         :: Maybe Doc
    } deriving (Show,Eq)

-- | Represent a DBus Method's Annotation
data Annotation = Annotation
    { annotationName  :: String
    , annotationValue :: String
    } deriving (Show,Eq)

-- | Represent a DBus Signal
data Signal = Signal
    { signalName   :: String
    , signalParams :: [Arg]
    , signalDoc    :: Maybe Doc
    } deriving (Show,Eq)

-- | Represent a DBus Property
data Property = Property
    { propertyName    :: String
    , propertyType    :: DBus.Type
    , propertyAccess  :: Access
    , propertyRawType :: Maybe String
    } deriving (Show,Eq)

-- | Represent a DBus Arg (Parameter)
data Arg = Arg
    { argName :: String
    , argType :: DBus.Type
    , argDoc  :: Maybe Doc
    } deriving (Show,Eq)

-- | Property access type
data Access = Read | Write | ReadWrite
    deriving (Show,Eq)

type Doc = String
