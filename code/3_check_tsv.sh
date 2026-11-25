#!/bin/bash

# Check all columns in the tsv file have the right number of columns (ie. no stray tabs in abstracts)

badlines=`awk -F'\t' '{print NF}' ../clean/extract.tsv | grep -c -v 4`

echo "Number of lines without 4 columns: $badlines"