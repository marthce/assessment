#!/bin/bash

# Get files from their PMIDs - either from pmids_all or pmids_twenty depending on input from snakefile
INPUT_FILE="$1"
efetch -input $INPUT_FILE -db pubmed -format xml > ../raw/raw_data.xml
echo "Downloaded data successfully"

# Duplicate to clean directory
cp ../raw/raw_data.xml ../clean
echo "Copied data successfully"

# Remove all <PubmedData> sections to reduce bulk
sed -i '/<PubmedData>/,/<\/PubmedData>/d' ../clean/raw_data.xml
echo "Removed PubmedData sections successfully"

# Remove html entities
tidy -xml -indent -wrap 0 -utf8 --numeric-entities yes ../clean/raw_data.xml > ../clean/clean_data.xml
echo "Cleaned data successfully"