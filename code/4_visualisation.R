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

