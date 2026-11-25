library(xml2)
library(tidyverse)


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
