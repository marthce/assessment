#!/bin/bash

#Download articles
#esearch -db pubmed -query "antimicrobial[ti] AND resistance[ti]" | efetch -format xml > ../raw/raw_data.xml

#practice file with one article:
#efetch -db pubmed -id 43018584 -format xml > raw/practice.xml
#xmllint --xpath 'string(/PubmedArticleSet/PubmedArticle/MedLineCitation/PMID)' raw/practice.xml

#practice file with two articles:
#efetch -db pubmed -id 43018584,43022061 -format xml > raw/practice2.xml

#practice file with 20 articles:
#head -n20 raw/list_of_pmids.txt > raw/20_pmids.txt
#efetch -input "raw/20_pmids.txt" -db pubmed -format xml > raw/practice20.xml

#extract title, pmid, abstract, and year
#xmlstarlet sel -t -v "//PMID" raw/practice2.xml
#xmlstarlet sel -t -v "//ArticleTitle" raw/practice2.xml > raw/list_of_titles.txt
#xmlstarlet sel -t -v "//AbstractText" raw/practice2.xml > raw/list_of_abstracts.txt
#xmlstarlet sel -t -v "//PubDate/Year" raw/practice2.xml > raw/list_of_years.txt


#Download articles: title contains 'antimicrobial' and 'resistance', not necessarily next to each other, and are in english
#try saving the list of IDs as a text file to pipe into efetch
esearch -db pubmed -query "antimicrobial[ti] AND resistance[ti] AND english[la]" | efetch -format uid > raw/list_of_pmids.txt
#^ this works!
#check length (it's the right length - 19159)
wc -l raw/list_of_pmids.txt
#download articles
efetch -input "raw/list_of_pmids.txt" -db pubmed -format xml > raw/raw_data.xml


# going to have to do something about entities


#cp raw/raw_data.xml clean
#sed -i '3484733d;3789002d;4044356d;4976080d;5883115d;5932937d;6301477d;6356227d;6429646d;6448043d;6448440d;6643357d;6746702d;6867241d;6867244d' clean/raw_data.xml
#xmlstarlet sel -t -v "//ArticleTitle" clean/raw_data.xml > raw/list_of_titles_check.txt

# try removing all <PubmedData> sections

sed -i '/<PubmedData>/,/<\/PubmedData>/d' clean/raw_data.xml

# try tidy package to parse html into clean xml

tidy -xml -utf8 --numeric-entities yes clean/raw_data.xml > clean/raw_data_clean.xml

# ^ there's no indent when doing this - not sure yet if this matters
# v add indent, and stop line wrapping
tidy -xml -indent -wrap 0 -utf8 --numeric-entities yes clean/raw_data.xml > clean/raw_data_clean.xml



# FINAL CODE: #################################################################################################################

# Search for list of PMIDs and save these as a text file
esearch -db pubmed -query "antimicrobial[ti] AND resistance[ti] AND english[la]" | efetch -format uid > raw/list_of_pmids.txt

# Fetch all these articles and save as an xml
efetch -input "raw/list_of_pmids.txt" -db pubmed -format xml > raw/raw_data.xml

# Remove all <PubmedData> sections to reduce bulk
sed -i '/<PubmedData>/,/<\/PubmedData>/d' clean/raw_data.xml

# Remove html entities
tidy -xml -indent -wrap 0 -utf8 --numeric-entities yes clean/raw_data.xml > clean/raw_data_clean.xml