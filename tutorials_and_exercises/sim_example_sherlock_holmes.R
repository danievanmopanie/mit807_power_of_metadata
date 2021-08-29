#The game is afoot! Topic modeling of Sherlock Holmes stories
#https://juliasilge.com/blog/sherlock-holmes-stm/

#install.packages("tidyverse")
library(tidyverse)
#install.packages("gutenbergr")
library(gutenbergr)

sherlock_raw <- gutenberg_download(1661)
head(sherlock_raw)
sherlock_raw

sherlock <- sherlock_raw %>%
  mutate(story = ifelse(str_detect(text, "ADVENTURE"),
                        text,
                        NA)) %>%
  fill(story) %>%
  filter(story != "THE ADVENTURES OF SHERLOCK HOLMES") %>%
  mutate(story = factor(story, levels = unique(story)))

sherlock

#install.packages("tidytext")
library(tidytext)

tidy_sherlock <- sherlock %>%
  mutate(line = row_number()) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(word != "holmes")

tidy_sherlock %>%
  count(word, sort = TRUE)