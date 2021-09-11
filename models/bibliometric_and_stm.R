#data prep

#install.packages("bibliometrix")
library(bibliometrix)
library(dplyr)
library(tidyr)

#set sorking directory
getwd()
path = "/Users/danieungerer/Documents/Meesters/Klasse/MIT807/mit807_power_of_metadata/data/scopus"
setwd(path)
getwd()

#read bibtex format
list.files()
path_to_bib_files <- c("./scopus_2016_to_2018.bib", "./scopus_2019_to_2021.bib") #this is for test and quicker turnaround time for analysis
#path_to_bib_files <- list.files(".", pattern="\\.bib$", full.names=TRUE) #switch this on for all files
path_to_bib_files

?convert2df
M <- convert2df(path_to_bib_files, dbsource = "scopus", format = "bibtex")

#inspect object
typeof(M)
colnames(M)
M$AB[1]
M$CR[550]

head(M$ID) #Keywords Plus®
head(M$DE) #Author Keywords

colSums(is.na(M))
dim(M)


#######################
#Bibliometric Analysis
######################
?biblioAnalysis
results <- biblioAnalysis(M, sep = ";")

results$CountryCollaboration
results$CO
length(results$CO)
sum(is.na(results$CO))

#classify countries in continents
#install.packages("countrycode")
library(countrycode)
df_countries = data.frame(results$CountryCollaboration)
head(df_countries)

df_countries$continent <- countrycode(sourcevar = df_countries[, "Country"],
                            origin = "country.name",
                            destination = "continent")
?subset
df_countries <- subset(df_countries, select = c("Country", "continent"))
df_countries$country <- df_countries$Country
df_countries$Country <- NULL
head(df_countries)
table(df_countries$continent)
table(df_countries$country)

length(results$Countries)
results$Countries[1]
typeof(results$Countries)
sum(results$Countries)

head(results$DE, 20)
length(results$DE)

options(width=100)
S <- summary(object = results, 
             k = 30, 
             pause = FALSE)

S$MostRelKeywords

plot(x = results, 
     k = 30, 
     pause = FALSE)

#the most frequent cited manuscripts
CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:20])

#the most frequent cited first authors
CR <- citations(M, field = "author", sep = ";")
cbind(CR$Cited[1:20])

#local citations measure how many times an author (or a document) 
#included in this collection have been cited by other authors also in the collection.
CR <- localCitations(M, sep = ";")
CR$Authors[1:20,]

CR$Papers[1:20,]

#Authors’ Dominance ranking
#calculates the authors’ dominance ranking as proposed by Kumar & Kumar, 2008
DF <- dominance(results, k = 10)
DF

#Authors’ h-index
indices <- Hindex(M, field = "author", elements="PHAAL R", sep = ";", years = 10)

# PHAAL's impact indices:
indices$H

# PHAAL's citations
indices$CitationList

#h-index of the first 10 most productive authors (in this collection)
authors = gsub(","," ",names(results$Authors)[1:10])
authors

indices <- Hindex(M, field = "author", elements=authors, sep = ";", years = 50)
indices$H

#Top-Authors’ Productivity over the Time
topAU <- authorProdOverTime(M, k = 10, graph = TRUE)

#Keyword co-occurrences
# Create keyword co-occurrences network
?biblioNetwork
NetMatrix <- biblioNetwork(M, 
                           analysis = "co-occurrences", 
                           network = "keywords",  #authors #references #sources #countries #keywords #author_keywords #titles #abstracts
                           sep = ";")


# Plot the network
net=networkPlot(NetMatrix, 
                normalize = "association", 
                weighted = T, 
                n = 30, 
                Title = "Keyword Co-occurrences", 
                type = "fruchterman", 
                size = T,
                edgesize = 5,
                labelsize = 0.7)

#Co-Word Analysis: The conceptual structure of a field
#Conceptual Structure using keywords (method="CA")
CS <- conceptualStructure(M,
                          field = "AB",  #"ID_TM" = Keywords Plus stemmed through the Porter's stemming algorithm, 
                                            #"DE_TM" = Author's Keywords stemmed through the Porter's stemming algorithm, 
                                            #"TI" = Terms extracted from titles
                                            #"AB" = Terms extracted from abstracts
                          method = "CA",    #"CA" for Correspondence Analysis
                                            #"MCA" for Multiple Correspondence Analysis
                                            #"MDS" for Metric Multidimensional Scaling
                          minDegree = 4,    #the minimum occurrences of terms to analyze and plot.
                          ngrams = 2,
                          clust = 6, 
                          stemming = FALSE, 
                          labelsize = 10, 
                          documents = 10)

?conceptualStructure
?termExtraction

##############

#STM Part

##############

#A. Ingest
#install.packages("stm")
library(stm)        # Package for structural topic modeling
#install.packages("igraph")
library(igraph)     # Package for network analysis and visualization
#install.packages("stmCorrViz")
library(stmCorrViz) # Package for hierarchical correlation view of STMs

?convert2df()
colnames(M)
head(M$AU) #Authors
head(M$DE) #Author_Keyword
head(M$ID) #Keywords_Plus
head(M$C1) #Author_Address
head(M$CR) #Cited_References
head(M$JI,10) #ISO_Source_Abbrev
head(M$AB) #Abstracts
head(M$AR,10) #Random nummers
head(M$chemicals_cas) 
head(M$coden) 
head(M$RP) 
table(M$DT) 
head(M$DI) 
head(M$BE)
head(M$FU)
head(M$BN)
head(M$SN)
head(M$SO,20)
head(M$LA)
head(M$TC)
head(M$PN)
head(M$PU)
head(M$PM)
head(M$DB)
head(M$sponsors)
head(M$TI)
head(M$VL)
head(M$PY)
head(M$FX)
head(M$AU_UN)
head(M$AU1_UN)
table(M$AU_UN_NR)
head(M$AU_UN_NR,20)
head(M$SR_FULL)
head(M$SR)

colnames(M)

df_checkpoint <- M

df_pre_stm <- df_checkpoint %>%
  rename(
    Authors = AU,
    Author_Keywords = DE,
    Keyword_Plus = ID,
    Author_Address = C1,
    Cited_References = CR,
    ISO_Source_Abbrev = JI,
    Abstract = AB, #Include in STM
    Reprint_Address = RP,
    Document_Type = DT,
    DOI = DI,
    Editors = BE,
    Funding_Agent = FU,
    ISBN = BN,
    ISSN = SN,
    Publication_Name = SO, #Include in STM
    Language = LA,
    Times_Cited = TC, #Include in STM
    Part_Number = PN,
    Publisher = PU, #Include in STM
    PubMed_ID = PM,
    Database = DB,
    Document_Title = TI, #Include in STM
    Volume = VL,
    Year_Published = PY, #Include in STM
    Funding_Text = FX 
  )

library(stringi)
colSums(is.na(df_pre_stm))
head(df_pre_stm)
country_vec <- df_countries$country
#country_vec <- stri_trans_general(countrycode::codelist$country.name.en, id = "Latin-ASCII")
df_pre_stm$country = stri_extract_first(df_pre_stm$Author_Address, regex = sprintf(r="(\b(%s)\b)", stri_c(country_vec,collapse = "|")))
#df_pre_stm$country[is.na(df_pre_stm$country)] <- "Other"
df_pre_stm$continent <- countrycode(sourcevar = df_pre_stm[, "country"],
                                      origin = "country.name",
                                      destination = "continent")
#df_pre_stm$continent[is.na(df_pre_stm$continent)] <- "Other"
df_pre_stm$continent

head(df_pre_stm)
colSums(is.na(df_pre_stm))
table(df_pre_stm$country)
table(df_pre_stm$continent)

df_stm <- df_pre_stm
head(df_stm)

colSums(is.na(df_stm))
dim(df_stm)

df_stm <- subset(df_stm, Times_Cited > 0, select = c("Abstract", "Year_Published", "Publication_Name", "Times_Cited", "country", "continent"))
df_stm$Publication_Name <- as.factor(df_stm$Publication_Name)
df_stm$Publication_Name
colSums(is.na(df_stm))
str(df_stm)
dim(df_stm)

df_stm <- df_stm %>% drop_na
colSums(is.na(df_stm))
df_stm$country <- as.factor(df_stm$country)
df_stm$continent <- as.factor(df_stm$continent)
dim(df_stm)
head(df_stm)
df_stm$Times_Cited <- as.integer(df_stm$Times_Cited)
table(df_stm$Times_Cited)
table(df_stm$Year_Published)
str(df_stm)

#B. Prepare
#stemming and stopword removal
?textProcessor()
processed <- textProcessor(df_stm$Abstract, 
                           metadata = df_stm,
                           removestopwords = TRUE,
                           removepunctuation = FALSE,
                           removenumbers = FALSE,
                           stem = FALSE,
                           wordLengths = c(4, Inf),
                           custompunctuation = c(".", "/", "!", ",", "?", "(", ")", "[", "]", ":", ";"))

#structure and index the data for usage in the structural topic model
?prepDocuments
head(processed$documents)
length(processed$documents)
head(processed$vocab)
head(processed$meta)

out <- prepDocuments(processed$documents, 
                     processed$vocab, 
                     processed$meta,
                     lower.thresh = 3, #low frequency words removed
                     verbose = TRUE)

out$docs.removed

docs <- out$documents
head(docs)
docs[4][1]
vocab <- out$vocab
head(vocab, 10)
vocab[312]
vocab[332]
vocab[789]
meta <- out$meta
head(meta, 10)

plotRemoved(processed$documents, 
            lower.thresh = seq(1,25, by=1))

#C. Estimate
?stm
title_PrevalenceFit <- stm(documents = out$documents, 
                           vocab = out$vocab, 
                           K = 15, 
                           prevalence =~ continent + s(Year_Published) + s(Times_Cited), 
                           max.em.its = 75, 
                           data = out$meta, 
                           init.type = "Spectral", 
                           seed = 8458159, 
                           sigma.prior = 0.1,
                           verbose = TRUE)

title_PrevalenceFit$mu
title_PrevalenceFit$sigma
title_PrevalenceFit$beta
title_PrevalenceFit$settings
title_PrevalenceFit$convergence
title_PrevalenceFit$theta[1]
title_PrevalenceFit$eta
title_PrevalenceFit$invsigma
title_PrevalenceFit$time
title_PrevalenceFit$version

plot(title_PrevalenceFit, 
     type = "summary", 
     xlim = c(0,.4))

par(mfrow = c(1, 2))
plot(title_PrevalenceFit, 
     type = "labels", 
     topics = c(1, 2, 3, 4, 5))
plot(title_PrevalenceFit, 
     type = "labels",
     topics = c(6, 7, 8, 9, 10))
dev.off()

plot(title_PrevalenceFit, 
     type="hist")

plot(title_PrevalenceFit, 
     type="perspectives", 
     topics=c(6, 10))

plot(title_PrevalenceFit, 
     type="perspectives", 
     topics=c(3, 4))

plot(title_PrevalenceFit, 
     type="perspectives", 
     topics=c(7, 10))

plot(title_PrevalenceFit, 
     type="perspectives", 
     topics=c(5, 6))

#D. Evaluate
?selectModel
title_PrevalenceSelect <- selectModel(out$documents, 
                                      out$vocab, 
                                      K = 15, 
                                      prevalence =~ continent + s(Year_Published) + s(Times_Cited),
                                      max.em.its = 200, 
                                      data = meta, 
                                      runs  = 10, 
                                      seed = 8458159)

title_PrevalenceSelect$runout[[2]]
title_PrevalenceSelect$semcoh
title_PrevalenceSelect$exclusivity
title_PrevalenceSelect$sparsity

plotModels(title_PrevalenceSelect,
           pch = c(1, 2, 3, 4),
           legend.position = "bottomright")

topicQuality(model = title_PrevalenceFit, 
             documents = docs)

selectedModel2 <- title_PrevalenceSelect$runout[[2]] # Choose model #3
selectedModel2

#model search across number of topics
?manyTopics()
storage <- manyTopics(out$documents, 
                      out$vocab, 
                      K = c(8:12), 
                      prevalence =~ s(Year_Published) + s(Times_Cited),
                      data = meta, 
                      runs = 10)

storageOutput1 <- storage$out[[1]] # For example, choosing the model with 7 topics
plot(storageOutput1)

storageOutput5 <- storage$out[[5]] # For example, choosing the model with 10 topics
plot(storageOutput5)

plot(storageOutput5, 
     type = "summary", 
     xlim = c(0,.4))

plot(storageOutput5, 
     type="perspectives", 
     topics=c(2, 9))

#figure out the best model automatically defined by exclusivity and semantic coherence for each K
?searchK
kResult <- searchK(out$documents, 
                   out$vocab, 
                   K = c(7,15), 
                   prevalence =~ s(Year_Published) + s(Times_Cited),
                   data = meta)

plot(kResult)


#E. Understand
?labelTopics
labelTopicsSel <- labelTopics(title_PrevalenceFit)

labelTopicsSel
labelTopicsSel$score
labelTopicsSel$topicnums

?sageLabels
sageLabels(title_PrevalenceFit)

str(df_stm$Document_Title)

z = df_stm$Document_Title[-out$docs.removed]
length(z)

?findThoughts
thoughts3 <- findThoughts(title_PrevalenceFit, 
                          texts = z, 
                          n = 5, 
                          topics = 3)$docs[[1]]

thoughts9 <- findThoughts(title_PrevalenceFit, 
                          texts = z, 
                          n = 5, 
                          topics = 9)$docs[[1]]

par(mfrow = c(1,2))
plotQuote(thoughts3, width=30, main="Topic 3")
plotQuote(thoughts9, width=30, main="Topic 9")
dev.off()

mod.out.corr <- topicCorr(title_PrevalenceFit)
plot(mod.out.corr)


#Visualize
install.packages("wordcloud")
library(wordcloud)
?cloud
cloud(title_PrevalenceFit, 
      topic=7)

stmCorrViz(title_PrevalenceFit, 
           "stm-interactive-correlation.html", 
           documents_raw = data$documents, 
           documents_matrix=out$documents)

