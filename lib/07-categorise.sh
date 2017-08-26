#!/bin/bash

source ./config.sh

rm $OCR_DIR/*$CAT_EXT

for keyword in `cat $KEYWORD_FILE`
do
    echo -n "Categorising $keyword..."
    for matching_file in `agrep -liw $keyword $OCR_DIR/*$OCR_EXT`
    do
        echo "$keyword" >> "$OCR_DIR/`basename $matching_file $OCR_EXT`$CAT_EXT"
    done
    echo "done"
done

# find uncategorised files
echo -n "Find uncategorised files..."
for scan_file in $OCR_DIR/*$SCAN_EXT; do
    if [ ! -f "$scan_file$CAT_EXT" ]; then # no category set for this file
        echo "uncategorised" > "$scan_file$CAT_EXT"
    fi
done
echo "done"




