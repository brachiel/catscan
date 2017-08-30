#!/bin/bash

source ./config.sh

# Link
for cat_file in $OCR_DIR/*$CAT_EXT; do
    document=`basename "$cat_file" $CAT_EXT`
    echo -n "Linking $document: "

    while read category; do
        echo -n "$category, "
        # we add the document to the keywords index
        echo $document >> "$CATEGORY_DIR/$category"
    done < $cat_file

    echo "done"
done

# store
mkdir -p $DOCUMENT_DIR

mv $OCR_DIR/*$SCAN_EXT $DOCUMENT_DIR
mv $OCR_DIR/*$OCR_EXT $DOCUMENT_DIR
mv $OCR_DIR/*$CAT_EXT $DOCUMENT_DIR

