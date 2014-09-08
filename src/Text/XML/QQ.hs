{-# LANGUAGE TemplateHaskell, QuasiQuotes, UndecidableInstances #-}


-- | The XML quasiquoter.
--    
--    Given the variables
--    
--      >  url = "google.se"
--      >  elem = "gmail"
--      >  attrNs = "something"
--      >  attrName = "Pelle"
--      >  attrValue = "Arne"
--      >  elemCont = CRef "testing"
--      >  cont1 = Elem $ element { elName = qname "hej" }
--      >  cont2 = CRef "other test"
--    
--    the code
--    
--      >   [$xmlQQ|
--      >   <{url}:{elem} {attrNs}:{attrName}={attrValue} attr="cool">
--      >     <elem ns1:elem1="1" ns2:elem2="2"><<elemCont>></elem>
--      >     <elem />
--      >     <el />
--      >     <<cont1>>
--      >     <<cont2>>
--      >   </{url}:{elem}>
--      >   |]
--    
--    will generate the data structure
--    
--      >   element {
--      >     elName = QName elem Nothing (Just url),
--      >     elAttribs = [Attr (QName attrName Nothing (Just attrNs)) attrValue,
--      >                  Attr (qname "attr") "cool"],
--      >     elContent = [
--      >       (Elem $ element { elName = qname "elem",
--      >                         elAttribs = [Attr (QName "elem1" Nothing (Just "ns1")) "1",
--      >                                      Attr (QName "elem2" Nothing (Just "ns2")) "2"],
--      >                         elContent = [elemCont]
--      >                        }),
--      >        (Elem $ element { elName = qname "elem" }),
--      >        (Elem $ element { elName = qname "el" }),
--      >        cont1,
--      >        cont2]
--      >   }
module Text.XML.QQ (xmlQQ, xmlFileQQ) where

-- import Text.XML.Light
import qualified Text.XML.Light.Types as XT
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.Data
import Data.Maybe

-- import Data.Ratio
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error


xmlQQ :: QuasiQuoter
xmlQQ = QuasiQuoter xmlExp xmlPat xmlType xmlDec
  where
  xmlPat = undefined
  xmlType = undefined
  xmlDec = undefined

  xmlExp :: String -> ExpQ
  xmlExp txt =
    case parsed' of 
      Left err -> error $ "Error in jsonExp: " ++ show err
      Right val -> return $ elementToExp val
    where
      parsed' = parse xmlElementParser "txt" txt

xmlFileQQ :: QuasiQuoter
xmlFileQQ = quoteFile xmlQQ

-- Data types to Exp

elementToExp :: ElementMeta -> Exp
elementToExp (Element name attribs contents line) =
  AppE (AppE (AppE (AppE (ConE nElement) name') attr') contents') (ConE nNothing)
  where
    name' = qnameToExp name
    attr' = ListE $ map attrToExp attribs
    contents' = ListE $ map contentToExp contents

qnameToExp :: QNameMeta -> Exp
qnameToExp (QName name uri prefix) =
  AppE (AppE (AppE (ConE nQName) name') (ConE nNothing)) prefix'
  where
    prefix' = maybe (ConE nNothing) (\p -> (AppE (ConE nJust) (stringmetaToExp p))) prefix
    name' = stringmetaToExp name

stringmetaToExp :: StringMeta -> Exp
stringmetaToExp (StringMetaNormal s) = (LitE (StringL s))
stringmetaToExp (StringMetaVar s) = VarE $ mkName s

attrToExp :: AttrMeta -> Exp
attrToExp (Attr name val) =
  AppE (AppE (ConE nAttr) name') val' -- (LitE (StringL val))
  where
    name' = qnameToExp name
    val' = stringmetaToExp val

contentToExp :: ContentMeta -> Exp
contentToExp (Elem e) = AppE (ConE nElem) (elementToExp e)
contentToExp (CRef s) = AppE (ConE nCRef) (LitE (StringL s))
contentToExp (ContentVar v) = VarE $ mkName v
contentToExp _ = error "Case Text in contentToExp is not implemented yet."

nElem = mkName "Text.XML.Light.Types.Elem"
nText = mkName "Text.XML.Light.Types.Text"
nCRef = mkName "Text.XML.Light.Types.CRef"
nElement = mkName "Text.XML.Light.Types.Element"
nAttr = mkName "Text.XML.Light.Types.Attr"
nQName = mkName "Text.XML.Light.Types.QName"
nNothing = mkName "Data.Maybe.Nothing"
nJust = mkName "Data.Maybe.Just"
nList = mkName "[]"

blank_meta_element :: ElementMeta
blank_meta_element = Element (QName (StringMetaNormal "") Nothing Nothing) [] [] Nothing


-- Data types

data AttrMeta =
  Attr {
    attrKey :: QNameMeta,
    attrVal :: StringMeta
  }

data ElementMeta = 
  Element {
    elName :: QNameMeta,
    elAttribs :: [AttrMeta],
    elContent :: [ContentMeta],
    elLine :: Maybe Line
  }

data QNameMeta = 
  QName	{ 
    qName :: StringMeta,
    qURI :: Maybe String,
    qPrefix :: Maybe StringMeta
  }

data StringMeta =
  StringMetaNormal String
  | StringMetaVar String

getStringMeta :: StringMeta -> String
getStringMeta (StringMetaNormal n) = n
getStringMeta (StringMetaVar n) = "{" ++ n ++ "}"

data ContentMeta =
  Elem ElementMeta 
  | Text CDataMeta
  | CRef String
  | ContentVar String

data CDataMeta =
  CData	{
    cdVerbatim :: XT.CDataKind,
    cdData :: String,
    cdLine :: Maybe Line
  }
  

-- Parser

xmlElementParser :: Parser ElementMeta
xmlElementParser = do
  spaces
  char '<'
  name <- nameParser
  spaces
  attrs <- many $ try attrParser
  spaces
  -- string "/>"
  contents <- closeTag <|> (openCloseTag name)
  spaces
  return $ Element name attrs contents Nothing
  
closeTag :: Parser [ContentMeta]
closeTag = do
  string "/>"
  return []

openCloseTag :: QNameMeta -> Parser [ContentMeta]
openCloseTag (QName name Nothing ns) = do
  -- string ">"
  contents <- between (string ">") (string "</") (many contentParser)
  string $ (ns' ++ name') ++ ">"
  return contents
  where
    name' = getStringMeta name
    ns' = maybe "" (\n -> (getStringMeta n) ++ ":") ns
 
attrParser :: Parser AttrMeta
attrParser = do
  spaces
  name <- nameParser
  char '='
  value <- metaStringParser --between (string "\"") (string "\"") (chars)
  return $ Attr name value

contentParser :: Parser ContentMeta
contentParser = do
  spaces
  content <- (try contentVarParser) <|> (try xmlElementParser >>= return . Elem) <|> (crefParser >>= return . CRef)
  spaces
  return content

contentVarParser :: Parser ContentMeta
contentVarParser = do
  string "<<"
  s <- symbol
  string ">>"
  return $ ContentVar s

crefParser :: Parser String
crefParser = many1 (noneOf "><")

nameParser :: Parser QNameMeta -- (String,Maybe String)
nameParser = do
  name1 <- metaSymbolParser -- symbol
  name2 <- optionMaybe (
    do
      char ':'
      metaSymbolParser)
      -- symbol)
  let
    ns = maybe Nothing (\n -> Just ( name1)) name2
    name = maybe name1 (\n -> n) name2
  return $ QName ( name) Nothing ns -- (name, ns)


-- helpers

metaStringParser :: Parser StringMeta
metaStringParser = do
  metaNormalStringParser <|> metaVarSymbolParser

metaNormalStringParser :: Parser StringMeta
metaNormalStringParser = do
  char '"'
  s <- chars
  char '"'
  return $ StringMetaNormal s

metaSymbolParser :: Parser StringMeta
metaSymbolParser = do
  metaNormalSymbolParser <|> metaVarSymbolParser

metaNormalSymbolParser :: Parser StringMeta  
metaNormalSymbolParser = do
  s <- symbol
  return $ StringMetaNormal s

metaVarSymbolParser :: Parser StringMeta
metaVarSymbolParser = do
  char '{'
  s <- symbol
  char '}'
  return $ StringMetaVar s

symbol :: CharParser () String
symbol = many1 (noneOf "{}\\ \"/:;><$=")

chars :: CharParser () String
chars = many (noneOf "\"")


