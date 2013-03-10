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

import Text.XML.Light
import qualified Network.DBus.Actions as DBus (unserializeSignature)
import Network.DBus.Model.Types

import qualified Data.ByteString.Char8 as BC
import Data.List (partition)

fromXML :: String -> Maybe Model
fromXML s = el >>= parseNode
    where el = parseXMLDoc s

tpNS = Just "tp"

parseNode :: Element -> Maybe Model
parseNode e = Model <$> mapM parseInterface (childElems "interface" e)
                    <*> attr "name" e
                    <*> pure (attrFQ (Just "xmlns") "tp" e)

parseInterface e =
    Interface <$> parseName e
              <*> mapM parseMethod (childElems "method" e)
              <*> mapM parseSignal (childElems "signal" e)
              <*> mapM parseProperty (childElems "property" e)
              <*> mapM parseEnumeration (childElemsFQ tpNS "enum" e)
              <*> mapM parseFlags (childElemsFQ tpNS "flags" e)
              <*> mapM parseStruct (childElemsFQ tpNS "struct" e)

parseStruct e =
    Struct <$> parseName e
           <*> mapM parseMember (childElemsFQ tpNS "member" e)

parseMember e =
    Member <$> parseName e
           <*> parseType e
           <*> pure (parseRawType e)
           <*> parseDoc e

parseEnumeration e =
    Enumeration <$> parseName e
                <*> parseType e
                <*> mapM parseEnumValue (childElemsFQ tpNS "enumvalue" e)

parseEnumValue e =
    EnumValue <$> attr "suffix" e
              <*> attr "value" e

parseFlags e =
    Flags <$> parseName e
          <*> attr "value-prefix" e
          <*> parseType e
          <*> mapM parseFlag (childElemsFQ tpNS "flag" e)
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

parseRawType e = attrFQ tpNS "type" e

parseName e = attr "name" e

parseDoc e = pure $ case childElemsFQ tpNS "docstring" e of
                         [el] -> case elContent el of
                                      [Text t] -> Just $ cdData t
                                      z        -> error (show z)
                         _    -> Nothing

------------------------------------------------------
-- XML helpers

childElemsFQ :: Maybe String -> String -> Element -> [Element]
childElemsFQ ns name el = filterChildren ((qnameEqNoUrl ns name) . elName) el

childElems name el = childElemsFQ Nothing name el

attrFQ :: Maybe String -> String -> Element -> Maybe String
attrFQ ns name el = lookupAttrBy (qnameEqNoUrl ns name) $ elAttribs el

attr = attrFQ Nothing

qnameEqNoUrl :: Maybe String -> String -> QName -> Bool
qnameEqNoUrl pre n qn = qName qn == n && qPrefix qn == pre
