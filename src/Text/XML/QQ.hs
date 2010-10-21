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
    parsed' = parse xmlElementParser "txt" txt

-- Data types to Exp

xmlValue :: Element -> ExpQ
xmlValue el = -- (XmlElementValue name ns attrs) =
  [| el |]


xmlElementParser :: Parser Element
xmlElementParser = do
  spaces
  char '<'
  (name,ns) <- nameParser
  spaces
  attrs <- many $ try attrParser
  spaces
  -- string "/>"
  contents <- closeTag <|> (openCloseTag name ns)
  spaces
  return $ blank_element { elName = (QName name Nothing ns), elAttribs = attrs, elContent = contents }
  
closeTag :: Parser [Content]
closeTag = do
  string "/>"
  return []

openCloseTag :: String -> Maybe String -> Parser [Content]
openCloseTag name ns = do
  -- string ">"
  contents <- between (string ">") (string "</") (many contentParser)
  string $ (ns' ++ name') ++ ">"
  return contents
  where
    name' = name
    ns' = maybe "" (\n -> n ++ ":") ns
 
attrParser :: Parser Attr 
attrParser = do
  spaces
  (name,ns) <- nameParser
  char '='
  value <- between (string "\"") (string "\"") (chars)
  return $ Attr (QName name Nothing ns) value

contentParser :: Parser Content
contentParser = do
  content <- (try xmlElementParser >>= return . Elem) <|> (crefParser >>= return . CRef)
  return content

crefParser :: Parser String
crefParser = many1 (noneOf "><")

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


