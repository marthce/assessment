Exploring research trends in antimicrobial resistance.

Summary of steps:
- Search for and download a set of research articles on antimicrobial resistance (in English) from PubMed.
- Clean (ie remove html entities from) the resulting .xml file in bash using the tidy package.
- Use tidyverse in R to parse the xml and extract PMIDs, years, titles and abstracts.
