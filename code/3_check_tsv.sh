#!/bin/bash

# Check all columns in the tsv file have the right number of columns (ie. no stray tabs in abstracts)

echo "Number of rows that do not have 4 columns:"
awk -F'\t' '{print NF}' clean/extract.tsv | grep -c -v 4