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
  add_count(Year, name = "PerYear") |> add_count(Decade, name = "PerDecade")

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
