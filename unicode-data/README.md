Unicode data sources:
* http://www.unicode.org/Public/UCD/latest/
* http://www.unicode.org/Public/UCD/latest/ucd/
* http://www.unicode.org/Public/UCD/latest/ucdxml/

To generate the Haskell data structures from UCD:
ucd2haskell ucdxml/ucd.all.flat.xml ../Data/Unicode/Properties/
