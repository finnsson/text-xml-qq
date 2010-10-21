{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell -XQuasiQuotes #-}
module Text.XML.TestQQ where

import Text.XML.QQ

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework (defaultMain)

import Text.XML.Light
import Text.XML.Light.Types
import Data.Maybe
import Text.XML.Generic

import Language.Haskell.TH 

main = defaultMain [tests]

tests = $testGroupGenerator

case_simple_element_without_ns = expected @=? actual
  where
    actual = [$xmlQQ| <foo /> |]
    expected = element { elName = QName "foo" Nothing Nothing }

case_simple_element_without_ns_and_space = expected @=? actual
  where
    actual = [$xmlQQ| <foo/> |]
    expected = element { elName = QName "foo" Nothing Nothing }

case_simple_element = expected @=? actual
  where actual = [$xmlQQ| <ns:apa /> |]
        expected = element { elName = QName "apa" Nothing (Just"ns") }

case_simple_element_with_attribute_ns = expected @=? actual
  where actual = [$xmlQQ| <ns:apa what:is="this" /> |]
        expected = element { elName = QName "apa" Nothing (Just"ns"), elAttribs = [Attr (QName "is" Nothing (Just "what")) "this"] }
 
case_simple_element_with_close_tag = expected @=? actual
  where actual = [$xmlQQ| <ns:apa></ns:apa> |]
        expected = element { elName = QName "apa" Nothing (Just"ns") }  

case_element_with_attribute = expected @=? actual
  where
    actual = [$xmlQQ| <foo name="Pell e\\" /> |]
    expected = element { elName = QName "foo" Nothing Nothing , elAttribs = [Attr (QName "name" Nothing Nothing) "Pell e\\\\"] }

case_simple_element_with_close_tag_and_cref = expected @=? actual
  where actual = [$xmlQQ| <ns:apa>Banan</ns:apa> |]
        expected = element { elName = QName "apa" Nothing (Just"ns"), elContent = [CRef "Banan"] }  

case_apa_with_foo = expected @=? actual
  where actual = [$xmlQQ| <apa><foo></foo></apa> |]
        expected =
          element {
            elName = QName "apa" Nothing Nothing,
            elContent = [ Elem element { elName = QName "foo" Nothing Nothing } ]
          }

case_multiline_and_multielement = expected @=? actual
  where
    actual = [$xmlQQ|
<apa>
  <foo>Some text </foo>
</apa>
|]
    expected = element {
      elName = QName "apa" Nothing Nothing,
      elContent = [
        Elem element {
          elName = QName "foo" Nothing Nothing,
          elContent = [ CRef "Some text " ]
        }]
    }

case_var_as_name = expected @=? actual
  where
    actual = [$xmlQQ| <{foo} /> |]
    foo = "bar"
    expected = element { elName = QName "bar" Nothing Nothing }

-- helpers

element =
  Element {
    elName = (QName "" Nothing Nothing),
    elAttribs = [],
    elContent = [],
    elLine = Nothing
  }

deriving instance Eq Element

deriving instance Eq Content

deriving instance Eq CData 
