library(xml2)
library(tidyverse)

# Try on the whole thing.

#raw_text <- readLines("raw/raw_data.xml", warn = FALSE)
#clean_text <- gsub("&beta;", "&#946;", raw_text, fixed = TRUE)
#clean_text <- gsub("&rsquo;", "&#8217;", clean_text, fixed = TRUE)
#doc <- read_xml(paste(clean_text, collapse = "\n"))



#rd_list <- as_list(read_xml("raw/raw_data.xml"))
#rd_xml <- read_xml("raw/raw_data.xml")
# Issues with reading as an xml due to undefined entities (beta)
# Try reading as an html
#rd_html <- read_html("raw/raw_data.xml")
# Can't use as_list on a html - R crashes
#rd_html_list <- as_list(rd_html)
# try find_all

#pmids <- xml_find_all(rd_xml, "//PMID")

# METHOD 1 - HTML --------------------------------------------------------------

#rd_html <- read_html("raw/raw_data.xml")

#html_pubmedarticleset <- 

#html_pmids <- rd_html |> html_elements(".")


# METHOD 2 - -------------------------------------------------------------------
# Clean data first in bash

cleandata <- read_xml("clean/raw_data_clean.xml")
cleandatalist <- as_list(cleandata)

# Expand

clean_tibble <- as_tibble(cleandatalist) |> unnest_longer(PubmedArticleSet)

# Expand wider - by PubmedArticleSet, DateCompleted, Article, Abstract - to reveal wanted columns

clean_tibble_wider <- clean_tibble |>
  filter(PubmedArticleSet_id == "MedlineCitation") |>
  unnest_wider(PubmedArticleSet, names_repair = "unique") |>
  unnest_wider(DateCompleted) |>
  unnest_wider(Article) |>
  unnest_wider(Abstract)

# Select columns we want (PMID, ArticleTitle, AbstractText, Year)

clean_tibble_subset <- clean_tibble_wider |>
  dplyr::select(PMID, ArticleTitle, AbstractText, Year)

# Unnest twice - the first time still has the text hidden

clean_tibble_subset <- clean_tibble_subset |>
  unnest(cols = c(PMID, ArticleTitle, AbstractText, Year), keep_empty = TRUE) |>
  unnest(cols = c(PMID, ArticleTitle, AbstractText, Year), keep_empty = TRUE)

# Remove rows with missing values

subset_complete <- clean_tibble_subset |>
  drop_na()

# Save tibble to tsv file

write_tsv(subset_complete,
          file = paste0("./clean/extract.tsv"))
