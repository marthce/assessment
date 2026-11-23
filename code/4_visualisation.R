library(tidyverse)
library(tidytext)
library(ggplot2)
library(ggrepel)

articles <- read_tsv("../clean/extract.tsv")

practice <- c("All I do is sit and think about you,",
              "if I knew what you'd do,",
              "collapse my veins wearing beautiful shoes,",
              "it's not living if it's not with you")
practice
practice_tibble <- tibble(line = 1:4, text = practice)
practice_tibble
practice_tibble |> unnest_tokens(word, text)

# one word per row: title
articles |> unnest_tokens(word, ArticleTitle) -> bytitle_articles

# remove stop words
bytitle_articles |> anti_join(stop_words) -> rmsw_bytitle

data("stop_words")

# one word per row: abstract
articles |> unnest_tokens(word, AbstractText) -> byabstract_articles

# remove stop words
byabstract_articles |> anti_join(stop_words) -> rmsw_byabstract

# find most common words
rmsw_byabstract |> count(word, sort = TRUE)

# make plot
rmsw_byabstract |> count(word, sort = TRUE) |>
  filter(n > 5000) |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(x = n, y = word))+
  geom_col()+
  labs(y=NULL)


rmsw_byabstract |> count(word, sort = TRUE) |>
  filter(n > 5000) |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(x = n, y = word))+
  geom_col()


rmsw_bytitle |> count(word, sort = TRUE)

rmsw_bytitle |> count(word, sort = TRUE) |>
  filter(n > 500) |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(x = n, y = word))+
  geom_col()+
  labs(y=NULL)

rmsw_bytitle


# Remove stop words from both?

articles |> unnest_tokens(word, ArticleTitle) |>
  unnest_tokens(word, AbstractText) |>
  anti_join(stop_words) -> tidy_articles


# most common word by year

top_word <- c((tidy_articles |> group_by(Year) |> count(word, sort = TRUE) |>
                 slice_max(Year, n=1) |> slice_max(word, n=1) |>
                 mutate(top_word = word))$top_word)

tidy_articles |> group_by(Year) |> count(word, sort = TRUE) |>
  slice_max(Year, n=1) |> slice_max(word, n=1) |>
  ggplot(aes(x=Year, y=n))+
  geom_point()+
  geom_label_repel(aes(label = top_word),
                   box.padding = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50')

# ^ turns out this finds the last word in the alphabet

# most common word by year:

tidy_articles |> group_by(Year) |> count(word) |> slice_max(n, n=1) |>
  ggplot(mapping = aes(x = Year, y = n))+
  geom_point()+
  geom_label_repel(aes(label = word),
                   box.padding = 0.35,
                   point.padding = 0.5,
                   segment.color = "grey50")

# it's almost all 'resistance'


# try a histogram

book_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

tidy_articles |> group_by(Year) |> count(word) |> slice_max(n, n=1) |>
  ungroup() |>
  ggplot(aes(x = n, y = word))+
  geom_col()

# alternative way to get top word by year

articles |> unnest_tokens(word, AbstractText) |> anti_join(stop_words) |>
  count(Year, word, sort = TRUE) |> group_by(Year) |> slice_max(n, n=1) |>
  arrange(Year)

# try adding decades

rmsw_byabstract |> mutate(Decade = floor(Year/10)*10) |> relocate(Decade)

# now find most common 5 words by decade

rmsw_byabstract |> mutate(Decade = floor(Year/10)*10) |> relocate(Decade) |>
  count(Decade, word, sort = TRUE) |> group_by(Decade) |> slice_max(n, n=5) |>
  arrange(Decade)

# make a plot of this

rmsw_byabstract |> mutate(Decade = floor(Year/10)*10) |> relocate(Decade) |>
  count(Decade, word, sort = TRUE) |> group_by(Decade) |> slice_max(n, n=5) |>
  arrange(Decade) |> ungroup() |>
  ggplot(mapping = aes(x = n, y = fct_reorder(word, n), fill = Decade))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~Decade, ncol = 2, scales = "free")


# FREQUENCY OF 'SALMONELLA' OVER TIME

# as histogram
rmsw_byabstract |> mutate(Decade = floor(Year/10)*10) |> relocate(Decade) |>
  count(Decade, word, sort = TRUE) |> group_by(Decade) |>
  filter(word == "salmonella") |> arrange(Decade) |> ungroup() |>
  ggplot(mapping = aes(x = Decade, y = n))+
  geom_col()

# as (linked) scatterplot
rmsw_byabstract |> mutate(Decade = floor(Year/10)*10) |> relocate(Decade) |>
  count(Decade, word, sort = TRUE) |> group_by(Decade) |>
  filter(word == "salmonella") |> arrange(Decade) |> ungroup() |>
  ggplot(mapping = aes(x = Decade, y = n))+
  geom_point()+
  geom_line()

# ^ should work out how to do this weighted by total number of articles


# FREQUENCY OF 'MICE' OVER TIME:

# as linked scatterplot
rmsw_byabstract |> mutate(Decade = floor(Year/10)*10) |> relocate(Decade) |>
  count(Decade, word, sort = TRUE) |> group_by(Decade) |>
  filter(word == "mice") |> arrange(Decade) |> ungroup() |>
  ggplot(mapping = aes(x = Decade, y = n))+
  geom_point()+
  geom_line()+
  labs(x = "Decade", y = "Frequency",
       title = "Frequency of 'mice' over time")

# This doesn't mean very much
# Ideally, want this to show articles that mention mice as proportion of total
# articles for that decade.


rmsw_byabstract |> mutate(Decade = floor(Year/10)*10) |>
  count(PMID, word, sort = TRUE) |> arrange(PMID) |> filter(word)

rmsw_byabstract |> mutate(Decade = floor(Year/10)*10) |> group_by(PMID,word) |>
  add_tally(name = "freq") |> relocate(freq, .after = word)

#-------------------------------------------------------------------------------

# number of articles from each year and decade
articles |> count(Year)
articles |> mutate(Decade = floor(Year/10)*10) |> count(Decade)

# number of articles in each year and decade that mention mice

# approach 1
rmsw_byabstract |> group_by(PMID, word) |> summarise(n = n()) |>
  filter(word == "mice") -> pmidsofmice

# approach 2
rmsw_byabstract |> group_by(PMID) |> add_count(word) |>
  filter(word == "mice")

# approach 3
rmsw_byabstract |> filter(word %in% c("mice", "mouse")) |> group_by(PMID) |>
  add_count(word, name = "FreqInArticle") |> slice_max(FreqInArticle, n=1) |>
  slice_head() |> arrange(Year)

# NB can omit add_count() |> slice_max()

# approach 4
rmsw_byabstract |> filter(word %in% c("mice", "mouse")) |> group_by(PMID) |>
  slice_head() |> ungroup() |> arrange(Year) |>
  add_count(Year, name = "PerYear")

rmsw_byabstract |> filter(word %in% c("mice", "mouse")) |> group_by(PMID) |>
  slice_head() |> ungroup() |> arrange(Year) |>
  add_count(Year, name = "PerYear") |>
  ggplot(mapping = aes(x = Year, y = PerYear))+
  geom_point()+
  geom_line()+
  labs(title = "Articles mentioning mice per year")

# this works, but we need 0 values as well, and proportion

# augment data set with decade and number of articles per year

extended <- articles |> mutate(Decade = floor(Year/10)*10) |>
  add_count(Year, name = "ArtPerYear") |>
  add_count(Decade, name = "ArtPerDecade")

exbyab <- extended |> unnest_tokens(word, AbstractText) |>
  anti_join(stop_words)

# proportion of mice per year as plot:
exbyab |> filter(word %in% c("mice", "mouse")) |> group_by(PMID) |>
  slice_head() |> ungroup() |> arrange(Year) |>
  add_count(Year, name = "MicePerYear") |>
  mutate(PropYear = MicePerYear / PerYear) |>
  ggplot(mapping = aes(x = Year, y = PropYear))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "Proportion of articles mentioning mice by year")

# proportion of mice per decade as plot:
exbyab |> filter(word %in% c("mice", "mouse")) |> group_by(PMID) |>
  slice_head() |> ungroup() |> arrange(Year) |>
  add_count(Decade, name = "MicePerDecade") |>
  mutate(PropDecade = MicePerDecade / PerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "Proportion of articles mentioning mice by decade")


# Now we have the proportion, but we still need to add zeros.


# Try proportion of articles mentioning rats per decade:
exbyab |> filter(word %in% c("rats", "rat")) |> group_by(PMID) |>
  slice_head() |> ungroup() |> arrange(Year) |>
  add_count(Decade, name = "RatsPerDecade") |>
  mutate(PropDecade = RatsPerDecade / PerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "Proportion of articles mentioning rats by decade")


# Try proportion of articles mentioning Africa per decade:
exbyab |> filter(word == "africa") |> group_by(PMID) |>
  slice_head() |> ungroup() |> arrange(Year) |>
  add_count(Decade, name = "AfricaPerDecade") |>
  mutate(PropDecade = AfricaPerDecade / PerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "Proportion of articles mentioning Africa by decade")

# Try proportion of articles mentioning Europe per decade:
exbyab |> filter(word %in% c("europe", "england", "france")) |>
  group_by(PMID) |>
  slice_head() |> ungroup() |> arrange(Year) |>
  add_count(Decade, name = "EuropePerDecade") |>
  mutate(PropDecade = EuropePerDecade / PerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "Proportion of articles mentioning Europe by decade")

# Try proportion of articles mentioning Asia per decade:
exbyab |> filter(word %in% c("asia", "india", "china")) |>
  group_by(PMID) |>
  slice_head() |> ungroup() |> arrange(Year) |>
  add_count(Decade, name = "AsiaPerDecade") |>
  mutate(PropDecade = AsiaPerDecade / PerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "Proportion of articles mentioning Asia by decade")


# Try proportion of articles mentioning salmonella by decade:
exbyab |> filter(word == "salmonella") |>
  group_by(PMID) |>
  slice_head() |> ungroup() |> arrange(Year) |>
  add_count(Decade, name = "SalmonellaPerDecade") |>
  mutate(PropDecade = SalmonellaPerDecade / PerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "Proportion of articles mentioning salmonella by decade")

# Try proportion of articles mentioning e.coli by decade:
exbyab |> filter(word == "coli") |>
  group_by(PMID) |>
  slice_head() |> ungroup() |> arrange(Year) |>
  add_count(Decade, name = "EColiPerDecade") |>
  mutate(PropDecade = EColiPerDecade / PerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "Proportion of articles mentioning e. coli by decade")


# try to put them on the same plot.

Region <- c("asia", "europe", "africa", "america", "australia")

# approach 1


# on the same plot
exbyab |> filter(word %in% c("asia", "europe", "africa", "america", "australia")) |>
  group_by(PMID) |> arrange(Year) |> distinct() |> ungroup() |> group_by(word) |>
  add_count(Decade, name = "RegionPerDecade") |>
  mutate(PropDecade = RegionPerDecade / ArtPerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade, colour = word))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "Proportion of articles mentioning different world regions by decade")+
  scale_colour_discrete(labels = c("Africa", "America", "Asia", "Australia", "Europe"),
                        name = "Region")


# on different plots - this is better
exbyab |> filter(word %in% c("asia", "europe", "africa", "america", "australia")) |>
  group_by(PMID) |> arrange(Year) |> distinct() |> ungroup() |> group_by(word) |>
  add_count(Decade, name = "RegionPerDecade") |>
  mutate(PropDecade = RegionPerDecade / ArtPerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade, colour = word))+
  geom_point()+
  geom_line()+
  facet_wrap(~word, ncol = 2,
             labeller = as_labeller(c(africa = "Africa",
                                      america = "America",
                                      asia = "Asia",
                                      australia = "Australia",
                                      europe = "Europe")))+
  labs(y = "Proportion", x = NULL,
       title = "Proportion of articles mentioning different world regions by decade")+
  theme(legend.position="none")

# ^ Works!

# Need to add zeroes, and search for names of specific countries and cities too.


# Create data frames for key words for each region.

# How to decide regions?

# Compare Asia China, India, Japan and Korea
exbyab |> filter(word %in% c("asia", "china", "india", "japan", "korea")) |>
  group_by(PMID) |> arrange(Year) |> distinct() |> ungroup() |> group_by(word) |>
  add_count(Decade, name = "RegionPerDecade") |>
  mutate(PropDecade = RegionPerDecade / ArtPerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade, colour = word))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "Proportion of articles mentioning countries in Asia by decade")+
  scale_colour_discrete(labels = c("Asia", "China", "India", "Japan", "Korea"),
                        name = "Region")

# One for all asia:
exbyab |> filter(word %in% c("asia", "india", "china", "japan", "korea")) |>
  group_by(PMID) |>
  slice_head() |> ungroup() |> arrange(Year) |>
  add_count(Decade, name = "AsiaPerDecade") |>
  mutate(PropDecade = AsiaPerDecade / ArtPerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "Asia (India, China, Japan and Korea) by decade")


# Compare Australia and New Zealand
exbyab |> filter(word %in% c("australia", "zealand")) |>
  group_by(PMID) |> arrange(Year) |> distinct() |> ungroup() |> group_by(word) |>
  add_count(Decade, name = "RegionPerDecade") |>
  mutate(PropDecade = RegionPerDecade / ArtPerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade, colour = word))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "Proportion of articles mentioning Australia vs New Zealand")+
  scale_colour_discrete(labels = c("Asia", "New Zealand"),
                        name = "Region")

# Cumulative, Australia + New Zealand
exbyab |> filter(word %in% c("australia", "zealand")) |>
  group_by(PMID) |>
  slice_head() |> ungroup() |> arrange(Year) |>
  add_count(Decade, name = "AsiaPerDecade") |>
  mutate(PropDecade = AsiaPerDecade / ArtPerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "Oceania (Australia and New Zealand) by decade")




# Compare member states of UK

exbyab |> filter(word %in% c("england", "wales", "scotland")) |>
  group_by(PMID) |> arrange(Year) |> distinct() |> ungroup() |> group_by(word) |>
  add_count(Decade, name = "RegionPerDecade") |>
  mutate(PropDecade = RegionPerDecade / ArtPerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade, colour = word))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "Proportion of articles mentioning England, Scotland and Wales")+
  scale_colour_discrete(labels = c("England", "Wales", "Scotland"),
                        name = "Region")

# Together:
exbyab |> filter(word %in% c("england", "wales", "scotland")) |>
  group_by(PMID) |>
  slice_head() |> ungroup() |> arrange(Year) |>
  add_count(Decade, name = "UKPerDecade") |>
  mutate(PropDecade = UKPerDecade / ArtPerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "UK (England, Scotland and Wales) by decade")


# ATTEMPT 1 --------------------------------------------------------------------


europekey <- c("iceland", "icelandic", "ireland", "irish", "uk", "british",
               "england", "english", "scotland", "scottish", "wales", "welsh",
               "spain", "spanish", "portugal", "portuguese", "france", "french",
               "belgium", "belgian", "netherlands", "dutch", "luxembourg",
               "denmark", "danish", "germany", "german", "switzerland", "swiss",
               "liechtenstein", "italy", "italian", "malta", "maltese", "czechia",
               "czech", "poland", "polish", "austria", "austrian", "slovakia",
               "slovakian", "slovenia", "slovenian", "croatia", "croatian",
               "bosnia", "bosnian", "serbia", "serbian", "montenegro",
               "macedonia", "macedonian", "kosovo", "albania", "albanian",
               "greece", "greek", "romania", "romanian", "bulgaria", "bulgarian",
               "hungary", "hungarian", "ukraine", "ukrainian", "belarus",
               "belarussian", "russia", "russian", "moldova", "moldovan",
               "estonia", "estonian", "latvia", "latvian", "lithuania", "lithuanian",
               "norway", "norwegian", "sweden", "swedish", "finland", "finnish",
               "turkish", "turkiye", "georgia", "georgian", "balkan", "balkans",
               "scandinavia", "scandinavian", "mediterranean", "europe",
               "european")


exbyab |> filter(word %in% europekey) |>
  group_by(PMID) |>
  slice_head() |> ungroup() |> arrange(Year) |>
  add_count(Decade, name = "EuropePerDecade") |>
  mutate(PropDecade = EuropePerDecade / ArtPerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "All of Europe")


# just countries, not demonyms:
europekey2 <- c("iceland", "ireland", "uk", "england", "scotland", "wales",
               "spain", "portugal", "france", "belgium", "netherlands",
               "luxembourg", "denmark", "germany", "switzerland",
               "liechtenstein", "italy", "malta", "czechia", "czech", "poland",
               "austria", "slovakia", "slovenia", "croatia", "bosnia", "serbia",
               "montenegro","macedonia", "kosovo", "albania", "greece",
               "romania", "bulgaria", "hungary", "ukraine", "belarus", "russia",
               "moldova","estonia", "latvia", "lithuania","norway", "sweden",
               "finland", "turkiye", "georgia",
               "balkan", "balkans", "scandinavia", "scandinavian",
               "mediterranean", "europe", "european")

exbyab |> filter(word %in% europekey2) |>
  group_by(PMID) |>
  slice_head() |> ungroup() |> arrange(Year) |>
  add_count(Decade, name = "EuropePerDecade") |>
  mutate(PropDecade = EuropePerDecade / ArtPerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "All of Europe (Key 2)")


# Maybe add a column to data for region?


africakey2 <- c("algeria", "angola", "benin", "botswana", "burundi",
                "cameroon", "congo", "djibouti",
                "eritrea", "eswatini", "ethiopia", "gabon", "ghana", "gambia",
                "kenya", "liberia", "libya", "madagascar", "malawi", "mali",
                "mauritania", "mauritius", "morocco", "mozambique", "namibia",
                "niger", "nigeria", "rwanda", "senegal", "seychelles",
                "somalia", "sudan", "tanzania", "togo", "tunisia",
                "uganda", "zambia", "zimbabwe",
                "sahara", "saharan", "africa", "african")

exbyab |> filter(word %in% c("africa", "african")) |>
  group_by(PMID) |>
  slice_head() |> ungroup() |> arrange(Year) |>
  add_count(Decade, name = "AfricaPerDecade") |>
  mutate(PropDecade = AfricaPerDecade / ArtPerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "Just  'Africa'")

exbyab |> filter(word %in% africakey2) |>
  group_by(PMID) |>
  slice_head() |> ungroup() |> arrange(Year) |>
  add_count(Decade, name = "AfricaPerDecade") |>
  mutate(PropDecade = AfricaPerDecade / ArtPerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "All of Africa (Key 2)")


oceaniakey2 <- c("australia", "zealand", "fiji", "kiribati",
                 "micronesia", "nauru", "palau", "samoa", "tonga",
                 "tuvalu", "vanuatu", "polynesia", "australasia",
                 "australasian", "oceania")

exbyab |> filter(word %in% oceaniakey2) |>
  group_by(PMID) |>
  slice_head() |> ungroup() |> arrange(Year) |>
  add_count(Decade, name = "OceaniaPerDecade") |>
  mutate(PropDecade = OceaniaPerDecade / ArtPerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "All of Oceania (Key 2)")


americaskey2 <- c("canada", "mexico", "antigua", "barbuda", "bahamas",
                  "barbados", "cuba", "dominica", "grenada",
                  "haiti", "jamaica", "vincent",
                  "grenadines", "trinidad", "tobago", "caribbean",
                  "guatemala", "belize", "honduras", "nicaragua",
                  "panama", "argentina", "bolivia", "brazil", "chile",
                  "colombia", "ecuador", "guyana", "paraguay", "peru",
                  "suriname", "uruguay", "venezuela", "america", "american")

exbyab |> filter(word %in% americaskey2) |>
  group_by(PMID) |>
  slice_head() |> ungroup() |> arrange(Year) |>
  add_count(Decade, name = "AmericasPerDecade") |>
  mutate(PropDecade = AmericasPerDecade / ArtPerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "All of Americas (Key 2)")



asiakey2 <- c("asia", "asian", "mongolia", "china", "japan", "korea", "taiwan",
              "philippines", "papua", "indonesia", "malaysia", "brunei",
              "cambodia", "laos", "vietnam", "thailand", "myanmar", "burma",
              "bangladesh", "bhutan", "nepal", "india", "pakistan",
              "timor", "kazakhstan", "kyrgyzstan", "uzbekistan", "tajikistan")

exbyab |> filter(word %in% asiakey2) |>
  group_by(PMID) |>
  slice_head() |> ungroup() |> arrange(Year) |>
  add_count(Decade, name = "AsiaPerDecade") |>
  mutate(PropDecade = AsiaPerDecade / ArtPerDecade) |>
  ggplot(mapping = aes(x = Decade, y = PropDecade))+
  geom_point()+
  geom_line()+
  labs(y = "Proportion",
       title = "All of Asia (Key 2)")

# How to do pairs of words?

# n grams

extended_ngrams <- extended |> unnest_tokens(bigram, AbstractText,
                                             token = "ngrams", n = 2)

bigrams_separated <- extended_ngrams |> separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated |>
  filter(!word1 %in% stop_words$word) |>
  filter(!word2 %in% stop_words$word) |>
  relocate(word1, .after = ArticleTitle) |>
  relocate(word2, .after = word1)
bigrams_filtered |> count(word1, word2, sort = TRUE)
bigrams_unite <- bigrams_filtered |> unite(bigram, word1, word2, sep = " ")


# count for "middle east" and "middle eastern":

bigrams_filtered |> filter(word1 == "middle") |>
  filter(word2 %in% c("east", "eastern")) |> group_by(PMID) |> slice_head() |>
  ungroup() |> arrange(Year) |> add_count(Decade, name = "MEPerDecade")


bigrams_unite |> relocate(bigram, .after = ArticleTitle) |>
  filter(bigram %in% c("middle east", "middle eastern", "arab emirates")) |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year) |>
  add_count(Decade, name = "MEPerDecade")

middleeastkey2 <- c("turkmenistan", "afghanistan", "iran", "iraq", "azerbaijan",
                    "armenia", "syria", "israel", "lebanon", "jordan", "palestine",
                    "yemen", "oman", "kuwait", "bahrain", "qatar", "egypt",
                    "arabic")

#-------------------------------------------------------------------------------

# ASIA

asiawords <- exbyab |>
  filter(word %in% asiakey2) |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

asiapairs <- bigrams_unite |>
  filter(bigram %in% c("timor leste", "sri lanka")) |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

asiacombined <- full_join(asiawords, asiapairs)

asiacombined <- asiacombined |> add_column(Region = "Asia")

# EUROPE

europewords <- exbyab |>
  filter(word %in% europekey2) |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

europepairs <- bigrams_unite |>
  filter(bigram == "united kingdom") |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

europecombined <- full_join(europewords, europepairs)

europecombined <- europecombined |> add_column(Region = "Europe")

# OCEANIA

oceaniawords <- exbyab |>
  filter(word %in% oceaniakey2) |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

oceaniapairs <- bigrams_unite |>
  filter(bigram %in% c("marshall islands", "solomon islands", "pacific islands")) |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

oceaniacombined <- full_join(oceaniawords, oceaniapairs)

oceaniacombined <- oceaniacombined |> add_column(Region = "Oceania")

# AFRICA

africawords <- exbyab |>
  filter(word %in% africakey2) |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

africapairs <- bigrams_unite |>
  filter(bigram %in% c("burkina faso", "sierra leone",
                       "cabo verde", "cape verde", "ivory coast")) |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

africacombined <- full_join(africawords, africapairs)

africacombined <- africacombined |> add_column(Region = "Africa")

# AMERICAS

americaswords <- exbyab |>
  filter(word %in% americaskey2) |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

americaspairs <- bigrams_unite |>
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

middleeastpairs <- bigrams_unite |>
  filter(bigram %in% c("middle east", "middle eastern", "saudi arabia",
                       "arab emirates")) |>
  group_by(PMID) |> slice_head() |> ungroup() |> arrange(Year)

middleeastcombined <- full_join(middleeastwords, middleeastpairs)

middleeastcombined <- middleeastcombined |> add_column(Region = "Middle East")



# Make one big data set

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
  theme(legend.position = "none")



# Try by year just for fun:
combined |>
  group_by(Region) |> add_count(Year, name = "RegionPerYear") |>
  mutate(PropYear = RegionPerYear / ArtPerYear) |>
  ggplot(mapping = aes(x = Year, y = PropYear, colour = Region))+
  geom_point()+
  geom_line()+
  facet_wrap(~Region, ncol = 2)+
  labs(y = "Proportion", x = NULL,
       title = "Proportion of articles mentioning different world regions by year")+
  theme(legend.position = "none")

# Decade is nicer.