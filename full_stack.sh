#!/bin/bash

for file in `ls -v 0*.sh`
do
    ./$file
done

