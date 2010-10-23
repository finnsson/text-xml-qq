# README #

text-xml-qq is an Haskell quasiquoter that will convert XML into Haskell data structures compile-time
and thus catching syntax errors at compile time instead of runtime.

It also allows embedding of Haskell variables in the XML code.

## Example ##

Given the variables

    url = "google.se"
    elem = "gmail"
    attrNs = "something"
    attrName = "Pelle"
    attrValue = "Arne"
    elemCont = CRef "testing"
    cont1 = Elem $ element { elName = qname "hej" }
    cont2 = CRef "other test"

the code

    [$xmlQQ|
    <{url}:{elem} {attrNs}:{attrName}={attrValue} attr="cool">
      <elem ns1:elem1="1" ns2:elem2="2"><<elemCont>></elem>
      <elem />
      <el />
      <<cont1>>
      <<cont2>>
    </{url}:{elem}>
    |]

will generate the data structure

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

## API ##

    Text.XML.QQ.xmlQQ :: QuasiQuoter

Generates a Text.XML.Light.Types.Element given some xml-like code.

## Todo ##

* The XML parser is far from complete.

* The XML parser accepts incorrect XML.

* The XML parser rejects correct XML.

* Parser speed. The parser is naïve at the moment, filled with `try`S.

* Other XML backends.
