#!/bin/bash
# Moves files from RAW_SCAN_DIR to OCR_DIR and performs OCR on all files in OCR_DIR
source ./config.sh

mkdir -p "$OCR_DIR"
rm -f "$OCR_DIR/*$OCR_EXT" # Remove all previous OCR files

mv "$RAW_SCAN_DIR/*$SCAN_EXT" "$OCR_DIR"

cd "$OCR_DIR"

for raw_scan in "./*$SCAN_EXT"
do
    echo tesseract "$raw_scan" "$raw_scan" -l deu
    tesseract "$raw_scan" "$raw_scan" -l deu
done

