#!/bin/bash

for file in `ls -v lib/*.sh` # finds scripts sorted alphanumerically
do
    ./$file
done

