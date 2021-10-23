#https://stackoverflow.com/questions/35691268/grouping-words-that-are-similar


#install.packages("stringdist") # install this package
library(stringdist) 
library(tidyverse)

data_path = "/Users/danieungerer/Documents/Meesters/Klasse/MIT807/mit807_power_of_metadata/data"
setwd(data_path)
list.files(data_path)

bibliometric_keywords <- read.csv("bibiliometric_keywords.csv")
colnames(bibliometric_keywords)

bibliometric_keywords <- bibliometric_keywords %>%
  group_by(keyword) %>%
  summarise(count = n())

arrange(bibliometric_keywords, desc(count))

bibliometric_keywords <- subset(bibliometric_keywords, bibliometric_keywords$count >= 100)

bibliometric_keywords <- bibliometric_keywords %>% distinct(keyword, .keep_all= TRUE) #remove all duplication and keep the first one
bibliometric_keywords <- bibliometric_keywords$keyword
bibliometric_keywords

bibliometric_keywords = tolower(bibliometric_keywords) # otherwise case matters too much
bibliometric_keywords

# Calculate a string distance matrix; LCS is just one option
?"stringdist-metrics" # see others
sdm = stringdistmatrix(bibliometric_keywords, 
                       bibliometric_keywords, 
                       useNames = TRUE, 
                       method = "lcs")

sdm[1:5,1:5]

# Hierarchical clustering
sdm_dist = as.dist(sdm) # convert to a dist object (you essentially already have distances calculated)
plot(hclust(sdm_dist))

library("cluster")
clusplot(pam(sdm_dist, 20), color=TRUE, shade=F, labels=2, lines=0)
