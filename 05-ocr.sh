#!/bin/bash

source ./config.sh

mkdir -p "$OCR_DIR"

cd "$RAW_SCAN_DIR"

for raw_scan in ./*$SCAN_EXT
do
    echo tesseract "$raw_scan" "../$OCR_DIR/$raw_scan" -l deu
    tesseract "$raw_scan" "../$OCR_DIR/$raw_scan" -l deu

    echo mv "$raw_scan" "../$OCR_DIR"
    mv "$raw_scan" "../$OCR_DIR"
done

