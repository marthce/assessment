Exploring the patterns in geographical focus over time in research on antimicrobial resistance.

Summary of steps:
- Search for and download a set of research articles on antimicrobial resistance (in English) from PubMed.
- Clean (ie remove html entities from) the resulting .xml file in bash using the tidy package.
- Use tidyverse in R to parse the xml and extract PMIDs, years, titles and abstracts.
- Return to bash to check all is as it seems in the clean dataframe.
- Use tidytext in R to extract information from the dataframe about the countries and regions mentioned in the article abstracts.
- Display this information in a plot.