#!/bin/bash

source ./config.sh

# Link
for cat_file in $OCR_DIR/*$CAT_EXT; do
    document=`basename "$cat_file" $CAT_EXT`
    echo -n "Linking $document: "

    while read category; do
        echo -n "$category, "
        mkdir -p "$CATEGORY_DIR/$category"
        # ../ because we are in a subdirectory of $CATEGORY_DIR
        ln -s "../$RELATIVE_DOC_PATH/$document" "$CATEGORY_DIR/$category/$document"
    done < $cat_file

    echo "done"
done

# store
mkdir -p $DOCUMENT_DIR

mv $OCR_DIR/*$SCAN_EXT $DOCUMENT_DIR
mv $OCR_DIR/*$OCR_EXT $DOCUMENT_DIR
mv $OCR_DIR/*$CAT_EXT $DOCUMENT_DIR

