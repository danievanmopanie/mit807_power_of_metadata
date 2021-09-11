#https://jayrobwilliams.com/files/html/teaching-materials/STM#
#Structural Topic Models
library(zip)

# download speeches and metadata
setwd("/Users/danieungerer/Documents/Meesters/Klasse/MIT807/mit807_power_of_metadata/tutorials_and_exercises/data")

list.files()
#unzip('europarl-data-speeches.zip')
#unzip('europarl-metadata.zip')

#Loading Text Data
#install.packages("readtext")
library(readtext) # easily read in text in directories

# recursively get filepaths for speeches from 09-12
list.files()
speeches_paths <- list.files(path = c('europarl-data-speeches/2009',
                                      'europarl-data-speeches/2010'),
                             recursive = T, full.names = T)

# read in speeches
speeches <- readtext(speeches_paths)
speeches

#Creating a Corpus
#install.packages("quanteda")
#install.packages('Rcpp')
library(quanteda)
library(Rcpp)
speeches <- corpus(speeches)
speeches

meta(speeches, field = 'type') <- 'European Parliament Speech'

library(lubridate) # year function

#read in speech docvars
speeches_dv <- read.delim('europarl-documents-metadata.tsv', sep = '\t')
head(speeches_dv)

# subset metadata to 2009-20012
speeches_dv <- speeches_dv[year(speeches_dv$date) >= 2009 &
                             year(speeches_dv$date) <= 2010, ]

# read in MEP docvars
MEP_dv <- read.delim('europarl-meps-metadata.tsv', sep = '\t')
head(MEP_dv)

# merge MEP docvars onto speech metadata
dv <- merge(speeches_dv, MEP_dv, 
            all.x = T,
            by.x = 'mep_ids', by.y = 'mep_id')

# merge docvars onto corpus
colnames(docvars(speeches))
docvars(speeches) <- dv
colnames(dv)

# inspect first entries
head(docvars(speeches))
colnames(docvars(speeches))

EPP_corp <- corpus_subset(speeches, group_shortname == 'EPP')
as.character(EPP_corp)[5]

#Data Exploration and Descriptive Statistics
kwic(speeches, 'hockey', window = 7)
?tokens
tokens(speeches, what = "word1")

# extract summary statistics
speeches_df <- summary(speeches, n = ndoc(speeches))
head(speeches_df)

library(ggplot2) # ggplots

# plot density of tokens by country
ggplot(data = speeches_df, aes(x = Tokens, fill = country)) +
  geom_density(alpha = .25, linetype = 0) +
  theme_bw() +
  coord_cartesian(xlim = c(0, 1500)) +
  theme(legend.position = 'right',
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank())

ggplot(data = speeches_df, aes(x = ymd(date), y = Tokens, color = group_shortname)) +
  geom_smooth(alpha = .6, linetype = 1, se = F, method = 'loess') +
  theme_bw() +
  theme(legend.position = 'right',
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank())

# count number of missing observations in each document variable
colnames(speeches_df)
colnames(speeches_dv)
colnames(speeches)

speeches <- docvars(speeches)
colSums(is.na(speeches))
apply(speeches, 2, function(x) sum(is.na(x)))

# drop any texts with missing document variables
speeches <- corpus_subset(speeches, !is.na(country_short))

# count number of missing observations in each document variable again
apply(speeches, 2, function(x) sum(is.na(x)))
colSums(is.na(speeches))

speeches_sub <- corpus_sample(speeches, size = floor(ndoc(speeches) / 10))
