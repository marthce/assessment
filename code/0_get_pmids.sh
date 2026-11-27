#!/bin/bash

# Search for list of pmids and save as a text file

# all of them:
esearch -db pubmed -query "antimicrobial[ti] AND resistance[ti] AND english[la]" | efetch -format uid > ../raw/pmids_all.txt
wc -l ../raw/pmids_all.txt

# random selection of 20:

sort -R ../raw/pmids_all.txt | head -n20 | sort -g > ../raw/pmids_20.txt