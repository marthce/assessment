#!/bin/bash

# Search for list of PMIDs and save these as a text file
esearch -db pubmed -query "antimicrobial[ti] AND resistance[ti] AND english[la]" | efetch -format uid > raw/list_of_pmids.txt
wc -l raw/list_of_pmids.txt

# Fetch all these articles and save as an xml
efetch -input "raw/list_of_pmids.txt" -db pubmed -format xml > raw/raw_data.xml
echo "Downloaded data successfully"

# Duplicate to clean directory
cp raw/raw_data.xml clean
echo "Copied data successfully"

# Remove all <PubmedData> sections to reduce bulk
sed -i '/<PubmedData>/,/<\/PubmedData>/d' clean/raw_data.xml
echo "Removed PubmedData sections successfully"

# Remove html entities
tidy -xml -indent -wrap 0 -utf8 --numeric-entities yes clean/raw_data.xml > clean/raw_data_clean.xml
echo "Cleaned data successfully"

# NB change raw_data_clean.xml to clean_data.xml