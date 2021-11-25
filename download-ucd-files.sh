#!/bin/sh

# When reproducing the Haskell files we want to to be sure that the files that
# we used to generate them earlier are exactly the same as the ones we are
# downloading. To ensure that verfication of the checksum is necessary.

VERSION=14.0.0

SRC_FILE="https://www.unicode.org/Public/$VERSION/ucd/NormalizationTest.txt"
DST_FILE="test/data/NormalizationTest.txt"
SRC_FILE_CKSUM="7cb30cc2abe6c29c292b99095865d379ce1213045c78c4ff59c7e9391bbe2331"


# Download the file
wget -O "$DST_FILE" "$SRC_FILE"

# Verify checksum
DST_FILE_CKSUM=$(sha256sum $DST_FILE | cut -f1 -d' ')
if test "$DST_FILE_CKSUM" != "$SRC_FILE_CKSUM"
then
    echo "sha256sum of the downloaded file $DST_FILE "
    echo "   [$DST_FILE_CKSUM] does not match the expected checksum [$SRC_FILE_CKSUM]"
    exit 1
else
    echo "$DST_FILE checksum ok"
fi

# Export the version so it can be used by the executable
export UNICODE_VERSION="$VERSION"
