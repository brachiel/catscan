#!/bin/bash

source ./config.sh

for var in DOCUMENT_DIR OCR_EXT OCR_DIR CAT_EXT CATEGORY_DIR SCAN_EXT
do
    if [ -z "${!var}" ]
    then
        echo "$var not configured."
        exit 1
    fi
done

# Move all documents and ocr results back to the ocr directory
mv $DOCUMENT_DIR/*$OCR_EXT $OCR_DIR
mv $DOCUMENT_DIR/*$SCAN_EXT $OCR_DIR

# Remove old categorisation files
rm -f ./$DOCUMENT_DIR/*$CAT_EXT

# Remove previous categorisation links
rm ./$CATEGORY_DIR/*/*$SCAN_EXT
rmdir ./$CATEGORY_DIR/*

# Restart categorisation and store
./lib/07-categorise.sh
./lib/09-store.sh

