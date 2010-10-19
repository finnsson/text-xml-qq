{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell -XQuasiQuotes -XUndecidableInstances #-}

module Text.XML.QQ where

import Text.XML.Light
import Text.XML.Light.Types
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.Data
import Data.Maybe

-- import Data.Ratio
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

import Language.Haskell.Meta.Parse
import Language.Haskell.TH.Lift

import Text.XML.LiftQQ

xmlQQ :: QuasiQuoter
xmlQQ = QuasiQuoter xmlExp xmlPat

xmlPat = undefined

xmlExp :: String -> ExpQ
xmlExp txt =
  case parsed' of 
    Left err -> error $ "Error in jsonExp: " ++ show err
    Right val -> xmlValue val
  where
    parsed' = parse xmlParser "txt" txt

-- Data types to Exp

xmlValue :: XmlValue -> ExpQ
xmlValue (XmlElementValue name ns attrs) =
  
  [| Element { elName = QName name Nothing ns, elAttribs = attrs', elContent = [], elLine = Nothing } |]
  where
    attrs' = map xmlAttr attrs
  -- RecConE (mkName "Text.XML.Light.Types.Element") list
  --  where
  --   list = 
  --    [(mkName "Text.XML.Light.Types.elName", AppE (AppE (AppE (ConE $ mkName "Text.XML.Light.Types.QName") (LitE (StringL name))) (ConE (mkName "Data.Maybe.Nothing"))) nsE)
  --    ,(mkName "Text.XML.Light.Types.elAttribs" ,ConE (mkName "[]"))
  --    ,(mkName "Text.XML.Light.Types.elContent" ,ConE (mkName "[]"))
  --    ,(mkName "Text.XML.Light.Types.elLine",ConE (mkName "Data.Maybe.Nothing")) ]
  --   nsE = maybe (ConE $ mkName "Data.Maybe.Nothing") (\n -> (AppE (ConE $ mkName "Data.Maybe.Just") (LitE (StringL n)))) ns
 --error "Not defined"

-- xmlAttr :: XmlAttr -> ExpQ
xmlAttr :: XmlAttr -> Attr
xmlAttr (XmlAttr name ns value) = Attr (QName name Nothing ns) value

-- Data types

data XmlValue =
  XmlElementValue {
    xmlElementValueName :: String,
    xmlElementValueNS :: Maybe String,
    xmlElementValueAttrs :: [XmlAttr]
  }

data XmlAttr =
  XmlAttr {
    xmlAttrName :: String,
    xmlAttrNS :: Maybe String,
    xmlAttrValue :: String
  }

-- Parser

type XmlParser = Parser XmlValue

type XmlAttrParser = Parser XmlAttr


xmlParser :: XmlParser
xmlParser = do
  spaces
  res <- xmlElementParser
  spaces
  return $ res

xmlElementParser :: XmlParser
xmlElementParser = do
  spaces
  char '<'
  (name,ns) <- nameParser
  spaces
  attrs <- many $ try xmlAttrParser
  spaces
  -- string "/>"
  closeTag <|> (openCloseTag name ns)
  spaces
  return $ XmlElementValue name ns attrs
  
closeTag :: Parser String
closeTag = do string "/>"

openCloseTag :: String -> Maybe String -> Parser String
openCloseTag name ns = do
  string $ "></" ++ (ns' ++ name') ++ ">"
  where
    name' = name
    ns' = maybe "" (\n -> n ++ ":") ns
 
xmlAttrParser :: XmlAttrParser 
xmlAttrParser = do
  spaces
  (name,ns) <- nameParser
  char '='
  value <- between (string "\"") (string "\"") (chars) -- alphaNum)
  return $ XmlAttr name ns value

nameParser :: Parser (String,Maybe String)
nameParser = do
  name1 <- symbol
  name2 <- optionMaybe (
    do
      char ':'
      symbol)
  let
    ns = maybe Nothing (\n -> Just name1) name2
    name = maybe name1 (\n -> n) name2
  return $ (name, ns)


-- helpers

symbol :: CharParser () String
symbol = many1 (noneOf "\\ \"/:;><$=")

chars :: CharParser () String
chars = many (noneOf "\"")
