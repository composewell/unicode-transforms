## Unicode database update
Unicode data sources:
* http://www.unicode.org/Public/UCD/latest/
* http://www.unicode.org/Public/UCD/latest/ucd/
* http://www.unicode.org/Public/UCD/latest/ucdxml/

Download and decompress the following files:
* `NormalizationTest.txt` from the `ucd` directory
* `ucd.all.flat.zip` from the `ucdxml` directory

```
wget -P ucd https://www.unicode.org/Public/UCD/latest/ucd/NormalizationTest.txt
wget -P ucdxml http://www.unicode.org/Public/UCD/latest/ucdxml/ucd.all.flat.zip
unzip -d ucdxml ucdxml/ucd.all.flat.zip
```

## Generating Haskell files from Unicode database
To generate the Haskell data structures from UCD build the ucd2haskell
utility and run it like this:
```
ucd2haskell ucdxml/ucd.all.flat.xml ../Data/Unicode/Properties/
```

Update the unicode version in the changelog below as well as in the top
level README and haddock docs.

## Changelog

### Unreleased

* Unicode 13.0.0: https://www.unicode.org/Public/13.0.0/

### 0.3.6

* Unicode 12.1.0: https://www.unicode.org/Public/12.1.0/ucdxml/

### 0.3.0

* Unicode 9.0.0: https://www.unicode.org/Public/9.0.0/

### 0.2.0

* Unicode 8.0.0: https://www.unicode.org/Public/8.0.0/
