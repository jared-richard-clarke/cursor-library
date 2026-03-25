#!/usr/bin/env sh

set -e          # Exit if any command has a non-zero exit status.
set -u          # Exit on any reference to an undefined variable.
set -o pipefail # Prevent errors in a pipeline from being masked.

fetch() {
    RESOURCE="$1"
    FILE_NAME=$(basename "$1")

    curl --fail --silent --show-error --location "$RESOURCE" > "$FILE_NAME"

    if [ $? -ne 0 ]; then
        echo "Failed to fetch ${FILE_NAME}"
        exit 1
    fi
}

# Fetch most recent unicode license.

fetch "https://www.unicode.org/license.txt"

# Fetch grapheme-break and ancillary data.

cd "$(dirname "$0")/data"

fetch "https://www.unicode.org/Public/UCD/latest/ucd/DerivedCoreProperties.txt"
fetch "https://www.unicode.org/Public/UCD/latest/ucd/emoji/emoji-data.txt"
fetch "https://www.unicode.org/Public/UCD/latest/ucd/auxiliary/GraphemeBreakProperty.txt"

# Fetch grapheme-break test data.

fetch "https://www.unicode.org/Public/UCD/latest/ucd/auxiliary/GraphemeBreakTest.txt"
