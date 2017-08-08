#!/bin/bash

source ./config.sh

for keyword in `cat $KEYWORD_FILE`
do
    for match in `agrep -liw $keyword $OCR_DIR/*$OCR_EXT`
    do
        mkdir -p $CATEGORY_DIR/$keyword
        ln -s ../../$OCR_DIR/`basename -s $OCR_EXT $match` $CATEGORY_DIR/$keyword
    done
done

