## Unicode database update
Unicode data sources:
* http://www.unicode.org/Public/UCD/latest/
* http://www.unicode.org/Public/UCD/latest/ucd/
* http://www.unicode.org/Public/UCD/latest/ucdxml/

Download `ucd.all.flat.xml` from ucdxml source listed above and place it
in the `ucdxml` directory (or wherever you want).

## Generating Haskell files from Unicode database
To generate the Haskell data structures from UCD build the ucd2haskell
utility and run it like this:
```
ucd2haskell ucdxml/ucd.all.flat.xml ../Data/Unicode/Properties/
```
