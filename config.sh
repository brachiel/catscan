#!/bin/bash

# THIS IS NOT A PURE BASH FILE
# Use only a subset of bash compatible lines here
# Allowed are comments and settings of varaibles of the form
# <KEY>=<VALUE> # where VALUE is without space
# or
# <KEY>="<VALUE>" # where VALUE is without "

# Scan stage
# -> RAW_SCAN_DIR
SCAN_RESOLUTION=200
RAW_SCAN_DIR=raw_scans
RAW_SCAN_EXT=.pnm
SCAN_EXT=.png

# Ocr stage
# RAW_SCAN_DIR -> OCR_DIR
OCR_DIR=ocr_scans
OCR_EXT=.txt

# Categorisation stage
# Works in OCR_DIR
KEYWORD_FILE=keywords
CAT_EXT=.cat

# Store stage
# OCR_DIR -> DOCUMENTDIR
DOCUMENT_DIR=documents
CATEGORY_DIR=categories
RELATIVE_DOC_PATH="../$DOCUMENT_DIR" # Relative path from base categories dir to document

