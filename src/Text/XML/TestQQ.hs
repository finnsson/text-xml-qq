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

case_var_as_ns = expected @=? actual
  where
    actual = [$xmlQQ| <{foo}:apa>hej</{foo}:apa> |]
    expected = element { elName = QName "apa" Nothing (Just "ape"), elContent = [CRef "hej"] }
    foo = "ape"

case_var_as_cref = expected @=? actual
  where
    actual = [$xmlQQ| <ape>  <<someVal>></ape> |]
    expected = element { elName = QName "ape" Nothing Nothing, elContent = [CRef "apa"] }
    someVal = CRef "apa"

case_var_as_attr_value = expected @=? actual
  where
    actual = [$xmlQQ| <monkey bar={foo} /> |]
    expected = element { elName = qname "monkey", elAttribs = [Attr (qname "bar") "cool"] }
    foo = "cool"


case_var_as_attr_name = expected @=? actual
  where
    actual = [$xmlQQ| <monkey {foo}="cool" /> |]
    expected = element { elName = qname "monkey", elAttribs = [Attr (qname "bar") "cool"] }
    foo = "bar"

case_mega_test = expected @=? actual
  where
    actual = [$xmlQQ|
<{url}:{elem} {attrNs}:{attrName}={attrValue} attr="cool">
  <elem ns1:elem1="1" ns2:elem2="2"><<elemCont>></elem>
  <elem />
  <el />
  <<cont1>>
<<cont2>>
</{url}:{elem}>
|]
    url = "google.se"
    elem = "gmail"
    attrNs = "something"
    attrName = "Pelle"
    attrValue = "Arne"
    elemCont = CRef "testing"
    cont1 = Elem $ element { elName = qname "hej" }
    cont2 = CRef "other test"
    expected =
      element {
        elName = QName elem Nothing (Just url),
        elAttribs = [Attr (QName attrName Nothing (Just attrNs)) attrValue,
                     Attr (qname "attr") "cool"],
        elContent = [
          (Elem $ element { elName = qname "elem",
                            elAttribs = [Attr (QName "elem1" Nothing (Just "ns1")) "1",
                                         Attr (QName "elem2" Nothing (Just "ns2")) "2"],
                            elContent = [elemCont]
                           }),
           (Elem $ element { elName = qname "elem" }),
           (Elem $ element { elName = qname "el" }),
           cont1,
           cont2]
      }


-- helpers

qname n = QName n Nothing Nothing

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
