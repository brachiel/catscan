#!/bin/bash

source ./config.sh

mkdir -p $RAW_SCAN_DIR
cd $RAW_SCAN_DIR

scanimage --batch="`date +%Y%m%d_%H%M%S`_%d$SCAN_EXT"  --format=pnm --resolution 300 --mode Color --source ADF

cd ..

