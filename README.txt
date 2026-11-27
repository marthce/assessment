Exploring the patterns in geographical focus over time in research on antimicrobial resistance.

IMPORTANT:
- The snakefile is designed to be run from the TOP LEVEL of the directory.
- The dummy file begin_snakefile.txt begins the pipeline.
- The config file containing the test is called snakemake-config.yml. It will be set to TRUE by default,
and will run the pipeline using a random subset of 20 articles. Set to FALSE, it will run the pipeline
using the full list of PMIDs downloaded. Set to anything else, it will fail the pipeline.

SUMMARY OF METHOD:
- Search for and download a set of research articles on antimicrobial resistance (in English) from PubMed.
- Clean (ie remove html entities from) the resulting .xml file in bash using the tidy package.
- Use tidyverse in R to parse the xml and extract PMIDs, years, titles and abstracts.
- Return to bash to check all is as it seems in the resulting clean dataframe.
- Use tidytext in R to extract information from the dataframe about the countries and regions mentioned in the article abstracts.
- Display this information in a plot.