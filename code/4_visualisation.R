library(tidyverse)
library(tidytext)
library(ggplot2)

# Read in data -----------------------------------------------------------------


articles <- read_tsv("../clean/extract.tsv")

data("stop_words")


# Augment data set with decade, and number of articles per year and decade -----


extended <- articles |> mutate(Decade = floor(Year/10)*10) |>
  add_count(Year, name = "ArtPerYear") |>
  add_count(Decade, name = "ArtPerDecade")

exbyab <- extended |> unnest_tokens(word, AbstractText) |>
  anti_join(stop_words)


# We will need to search for pairs of words too --------------------------------

# unnest by pairs of words
extended_pairs <- extended |> unnest_tokens(bigram, AbstractText,
                                             token = "ngrams", n = 2)

# separate the pairs into two columns
pairs_separated <- extended_pairs |>
  separate(bigram, c("word1", "word2"), sep = " ")

# remove stop words
pairs_filtered <- pairs_separated |>
  filter(!word1 %in% stop_words$word) |>
  filter(!word2 %in% stop_words$word) |>
  relocate(word1, .after = ArticleTitle) |>
  relocate(word2, .after = word1)

# join them back up again
pairs_united <- pairs_filtered |> unite(bigram, word1, word2, sep = " ")


# Create lists of key words for each region ------------------------------------

europekey2 <- c("iceland", "ireland", "uk", "england", "scotland", "wales",
                "spain", "portugal", "france", "belgium", "netherlands",
                "luxembourg", "denmark", "germany", "switzerland",
                "liechtenstein", "italy", "malta", "czechia", "czech", "poland",
                "austria", "slovakia", "slovenia", "croatia", "bosnia", "serbia",
                "montenegro","macedonia", "kosovo", "albania", "greece",
                "romania", "bulgaria", "hungary", "ukraine", "belarus", "russia",
                "moldova","estonia", "latvia", "lithuania", "norway", "sweden",
                "finland", "turkiye", "georgia",
                "balkan", "balkans", "scandinavia", "scandinavian",
                "mediterranean", "europe", "european")

africakey2 <- c("algeria", "angola", "benin", "botswana", "burundi",
                "cameroon", "congo", "djibouti",
                "eritrea", "eswatini", "ethiopia", "gabon", "ghana", "gambia",
                "kenya", "liberia", "libya", "madagascar", "malawi", "mali",
                "mauritania", "mauritius", "morocco", "mozambique", "namibia",
                "niger", "nigeria", "rwanda", "senegal", "seychelles",
                "somalia", "sudan", "tanzania", "togo", "tunisia",
                "uganda", "zambia", "zimbabwe",
                "sahara", "saharan", "africa", "african")

oceaniakey2 <- c("australia", "zealand", "fiji", "kiribati",
                 "micronesia", "nauru", "palau", "samoa", "tonga",
                 "tuvalu", "vanuatu", "polynesia", "australasia",
                 "australasian", "oceania")

americaskey2 <- c("canada", "mexico", "antigua", "barbuda", "bahamas",
                  "barbados", "cuba", "dominica", "grenada",
                  "haiti", "jamaica", "vincent",
                  "grenadines", "trinidad", "tobago", "caribbean",
                  "guatemala", "belize", "honduras", "nicaragua",
                  "panama", "argentina", "bolivia", "brazil", "chile",
                  "colombia", "ecuador", "guyana", "paraguay", "peru",
                  "suriname", "uruguay", "venezuela", "america", "american")

asiakey2 <- c("asia", "asian", "mongolia", "china", "japan", "korea", "taiwan",
              "philippines", "papua", "indonesia", "malaysia", "brunei",
              "cambodia", "laos", "vietnam", "thailand", "myanmar", "burma",
              "bangladesh", "bhutan", "nepal", "india", "pakistan",
              "timor", "kazakhstan", "kyrgyzstan", "uzbekistan", "tajikistan")

middleeastkey2 <- c("turkmenistan", "afghanistan", "iran", "iraq", "azerbaijan",
                    "armenia", "syria", "israel", "lebanon", "jordan",
                    "palestine", "yemen", "oman", "kuwait", "bahrain", "qatar",
                    "egypt", "arab", "arabic")


# Find all unique articles that mention each region. ---------------------------

# ASIA

asiawords <- exbyab |>
  filter(word %in% asiakey2) |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

asiapairs <- pairs_united |>
  filter(bigram %in% c("timor leste", "sri lanka")) |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

asiacombined <- full_join(asiawords, asiapairs)

asiacombined <- asiacombined |> add_column(Region = "Asia")

# EUROPE

europewords <- exbyab |>
  filter(word %in% europekey2) |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

europepairs <- pairs_united |>
  filter(bigram == "united kingdom") |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

europecombined <- full_join(europewords, europepairs)

europecombined <- europecombined |> add_column(Region = "Europe")

# OCEANIA

oceaniawords <- exbyab |>
  filter(word %in% oceaniakey2) |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

oceaniapairs <- pairs_united |>
  filter(bigram %in% c("marshall islands", "solomon islands",
                       "pacific islands")) |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

oceaniacombined <- full_join(oceaniawords, oceaniapairs)

oceaniacombined <- oceaniacombined |> add_column(Region = "Oceania")

# AFRICA

africawords <- exbyab |>
  filter(word %in% africakey2) |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

africapairs <- pairs_united |>
  filter(bigram %in% c("burkina faso", "sierra leone",
                       "cabo verde", "cape verde", "ivory coast")) |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

africacombined <- full_join(africawords, africapairs)

africacombined <- africacombined |> add_column(Region = "Africa")

# AMERICAS

americaswords <- exbyab |>
  filter(word %in% americaskey2) |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

americaspairs <- pairs_united |>
  filter(bigram %in% c("united states", "puerto rico", "dominican republic",
                       "saint lucia", "st lucia", "saint kitts", "st kitts",
                       "el salvador", "costa rica")) |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

americascombined <- full_join(americaswords, americaspairs)

americascombined <- americascombined |> add_column(Region = "Americas")

# MIDDLE EAST

middleeastwords <- exbyab |>
  filter(word %in% middleeastkey2) |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

middleeastpairs <- pairs_united |>
  filter(bigram %in% c("middle east", "middle eastern", "saudi arabia",
                       "arab emirates")) |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

middleeastcombined <- full_join(middleeastwords, middleeastpairs)

middleeastcombined <- middleeastcombined |> add_column(Region = "Middle East")



# Make one big data set --------------------------------------------------------

combined <- asiacombined |>
  full_join(americascombined) |>
  full_join(middleeastcombined) |>
  full_join(europecombined) |>
  full_join(africacombined) |>
  full_join(oceaniacombined)


# Final visualisation yippee!!!!

combined |>
  group_by(Region) |> add_count(Decade, name = "RegionPerDecade") |>
  mutate(PropDecade = RegionPerDecade / ArtPerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade, colour = Region))+
  geom_point()+
  geom_line()+
  facet_wrap(~Region, ncol = 2)+
  labs(y = "Proportion", x = NULL,
       title = "Proportion of articles mentioning different world regions by decade")+
  theme(legend.position = "none") -> finalplot

ggsave(finalplot, filename = "finalplot.png", height = 5.81, width = 7.64)