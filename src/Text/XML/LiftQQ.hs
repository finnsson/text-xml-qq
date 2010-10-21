{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell -XQuasiQuotes -XUndecidableInstances #-}

module Text.XML.LiftQQ where

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

$(deriveLift (mkName "Text.XML.Light.Types.QName"))

$(deriveLift (mkName "Text.XML.Light.Types.Attr"))

$(deriveLift (mkName "Text.XML.Light.Types.Element"))

$(deriveLift (mkName "Text.XML.Light.Types.Content"))

$(deriveLift (mkName "Text.XML.Light.Types.CData"))

$(deriveLift (mkName "Text.XML.Light.Types.CDataKind"))
