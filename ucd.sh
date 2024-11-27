#!/bin/sh

# When reproducing the Haskell files we want to to be sure that the files that
# we used to generate them earlier are exactly the same as the ones we are
# downloading. To ensure that verfication of the checksum is necessary.

VERSION=14.0.0

# When downloading fresh new version comment this out
VERIFY_CHECKSUM=y

# Filename:destination:checksum
FILES="\
    DerivedNormalizationProps.txt:ucd:b2c444c20730b097787fdf50bd7d6dd3fc5256ab8084f5b35b11c8776eca674c \
    NormalizationTest.txt:test/data:7cb30cc2abe6c29c292b99095865d379ce1213045c78c4ff59c7e9391bbe2331"

delete_file() {
    local pair=$1
    local file=$(echo $pair | cut -f1 -d':')
    local destination="$(echo $pair | cut -f2 -d':')/$file"
    rm "$destination"
}

# Remove previous downloaded files
delete_files() {
    for pair in $FILES
    do
        delete_file "$pair"
    done
}

# Download the files

# Download $file from https://www.unicode.org/Public/$VERSION/$file
# and verify the $checksum if $VERIFY_CHECKSUM is enabled
# $1 = file:checksum

download_file() {
    local pair=$1
    local file=$(echo $pair | cut -f1 -d':')
    local destination="$(echo $pair | cut -f2 -d':')/$file"
    local checksum=$(echo $pair | cut -f3 -d':')

    if test ! -e "$destination"
    then
        wget -P `dirname "$destination"` "https://www.unicode.org/Public/$VERSION/ucd/$file"
    fi
    if test -n "$VERIFY_CHECKSUM"
    then
        new_checksum=$(sha256sum "$destination" | cut -f1 -d' ')
        if test "$checksum" != "$new_checksum"
        then
            echo "sha256sum of the downloaded file $destination "
            echo "   [$new_checksum] does not match the expected checksum [$checksum]"
            exit 1
        else
            echo "$destination checksum ok"
        fi
    fi
}

# Extract $file from $FILES download it using download_file
download_files() {
    for pair in $FILES
    do
        download_file "$pair"
    done
}

# Generate the Haskell files.
run_generator() {
    # Compile and run ucd2haskell
    cabal run --flag ucd2haskell ucd2haskell -- \
          --input ./ucd/ \
          --output ./
}

# Print help text
print_help() {
    echo "Usage: ucd.sh <command>"
    echo
    echo "Available commands:"
    echo "  clean: delete downloaded files"
    echo "  download: downloads the text files required"
    echo "  generate: generate the haskell files from the downloaded text files"
    echo
    echo "Example:"
    echo "$ ./ucd.sh download && ./ucd.sh generate"
}

# Main program

# Export the version so it can be used by the executable
export UNICODE_VERSION="$VERSION"

# Parse command line
case $1 in
    -h|--help) print_help;;
    clean) delete_files;;
    download) download_files;;
    generate) run_generator;;
    *) echo "Unknown argument"; print_help;;
esac
