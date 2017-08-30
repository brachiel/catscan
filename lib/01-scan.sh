#!/bin/bash

source ./config.sh

mkdir -p $RAW_SCAN_DIR
cd $RAW_SCAN_DIR

scanimage --batch="`date +%Y%m%d_%H%M%S`_%d$RAW_SCAN_EXT"  --format=pnm --resolution $SCAN_RESOLUTION --mode Color --source ADF

# convert pnm -> png
mogrify -format $SCAN_EXT *$RAW_SCAN_EXT
rm *$RAW_SCAN_EXT

cd ..

