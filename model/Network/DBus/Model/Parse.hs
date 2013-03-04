-- |
-- Module      : Network.DBus.Model.Parse
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DBus.Model.Parse
    ( fromXML
    ) where

import Control.Applicative

import qualified Text.XML.HaXml as X
--import Text.XML.HaXml (o)
--import qualified Network.DBus as DBus
import qualified Network.DBus.Actions as DBus (unserializeSignature)
import Network.DBus.Model.Types

import qualified Data.ByteString.Char8 as BC
import Data.List (partition)
import Data.Maybe (catMaybes)

fromXML :: String -> Maybe Model
fromXML s = Model <$> mapM parseInterface (childElems "interface" root)
    where X.Document _ _ root _ = X.xmlParse "" s

parseInterface :: X.Element i -> Maybe Interface
parseInterface e =
    Interface <$> parseName e
              <*> mapM parseMethod (childElems "method" e)
              <*> mapM parseSignal (childElems "signal" e)
              <*> mapM parseProperty (childElems "property" e)
              <*> mapM parseEnumeration (childElems "tp:enum" e)
              <*> mapM parseFlags (childElems "tp:flags" e)
              <*> mapM parseStruct (childElems "tp:struct" e)

parseStruct e =
    Struct <$> parseName e
           <*> mapM parseMember (childElems "tp:member" e)

parseMember e =
    Member <$> parseName e
           <*> parseType e
           <*> pure (parseRawType e)
           <*> parseDoc e

parseEnumeration e =
    Enumeration <$> parseName e
                <*> parseType e
                <*> mapM parseEnumValue (childElems "tp:enumvalue" e)

parseEnumValue e =
    EnumValue <$> attr "suffix" e
              <*> attr "value" e

parseFlags e =
    Flags <$> parseName e
          <*> attr "value-prefix" e
          <*> parseType e
          <*> mapM parseFlag (childElems "flag" e)
          <*> parseDoc e

parseFlag e =
    Flag <$> attr "suffix" e
         <*> attr "value" e
         <*> parseDoc e

parseProperty e =
    Property <$> parseName e
             <*> parseType e
             <*> parseAccess e
             <*> pure (parseRawType e)

parseMethod e =
    Method <$> parseName e
           <*> mapM parseAnnotation (childElems "annotation" e)
           <*> mapM parseArg inElems
           <*> mapM parseArg outElems
           <*> parseDoc e
    where argElems = childElems "arg" e
          (inElems,outElems) = partition inOrOut argElems
          inOrOut a = case attr "direction" a of
                           Nothing    -> True
                           Just "in"  -> True
                           Just "out" -> False
                           Just z     -> error ("unexpected direction string: " ++ z)

parseSignal e =
    Signal <$> parseName e
           <*> mapM parseArg (childElems "arg" e)
           <*> parseDoc e

parseAnnotation e =
    Annotation <$> parseName e
               <*> attr "value" e

parseArg e =
    Arg <$> parseName e
        <*> parseType e
        <*> parseDoc e

parseType e = attr "type" e >>= parseSignature
    where parseSignature s = do
              typsig <- either (const Nothing) Just $ DBus.unserializeSignature (BC.pack s)
              case typsig of
                [t] -> Just t
                _   -> Nothing

parseAccess e = attr "access" e >>= parse
    where parse "read"      = Just Read
          parse "write"     = Just Write
          parse "readwrite" = Just ReadWrite
          parse _           = Nothing

parseRawType e = attrFQ (Just $ X.Namespace "tp" "") "type" e

parseName e = attr "name" e

parseDoc :: X.Element i -> Maybe (Maybe Doc)
parseDoc e = pure $ case childElems "tp:docstring" e of
                         [(X.Elem _ _ [X.CString _ cd _])] -> Just cd
                         _   -> Nothing

------------------------------------------------------
-- XML helpers

childElemsWith :: X.CFilter i -> X.Element i -> [X.Element i]
childElemsWith elemFilter (X.Elem _ _ contents) =
    catMaybes . map select . concatMap elemFilter $ contents
    where
          select (X.CElem e _) = Just e
          select _             = Nothing

childElems :: String -> X.Element i -> [X.Element i]
childElems name = childElemsWith (X.tag name)

attrFQ :: Maybe X.Namespace -> String -> X.Element i -> Maybe String
attrFQ ns name (X.Elem _ attrs _) = show <$> lookup el attrs
    where el = maybe (X.N name) (\n -> X.QN n name) ns

attr :: String -> X.Element i -> Maybe String
attr = attrFQ Nothing

