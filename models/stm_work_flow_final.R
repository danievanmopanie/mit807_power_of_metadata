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
#path_to_bib_files <- c("./scopus_1980_to_2002.bib", "./scopus_2003_to_2008.bib", "./scopus_2009_to_2012.bib") #this is for test and quicker turnaround time for analysis
path_to_bib_files <- list.files(".", pattern="\\.bib$", full.names=TRUE) #switch this on for all files
path_to_bib_files

#converting scopus collection into a bibliographic dataframe
M <- convert2df(path_to_bib_files, dbsource = "scopus", format = "bibtex")

#inspect bibtex object
typeof(M)
colnames(M)
colSums(is.na(M))
M$AB[550]
M$CR[550]
M$ID[550]
M$DE[550]

head(M$ID) #Keywords Plus®
head(M$DE) #Author Keywords
dim(M)

###########################
#  Bibliometric Analysis  #
###########################
#performs a bibliometric analysis of a dataset imported from SCOPUS 
?biblioAnalysis
results <- biblioAnalysis(M, sep = ";")

options(width=100)
S <- summary(object = results, 
             k = 30, 
             pause = FALSE)

plot(x = results, 
     k = 20, 
     pause = FALSE)

head(S$MostRelSources, 50)
str(S$MostRelSources)

#classify countries in continents
#install.packages("countrycode")
results$CountryCollaboration
library(countrycode)
df_countries = data.frame(results$CountryCollaboration)
head(df_countries)
dim(df_countries)

df_countries$continent <- countrycode(sourcevar = df_countries[, "Country"],
                                      origin = "country.name",
                                      destination = "continent")
head(df_countries)
table(df_countries$Country)
table(df_countries$continent)

df_countries <- subset(df_countries, select = c("Country", "continent"))
df_countries$Continent <- df_countries$continent
df_countries$continent <- NULL
head(df_countries)

##############
#  STM Part  #
##############

#A. Ingest
#install.packages("stm")
library(stm)        # Package for structural topic modeling
#install.packages("igraph")
library(igraph)     # Package for network analysis and visualization
#install.packages("stmCorrViz")
library(stmCorrViz) # Package for hierarchical correlation view of STM

df_checkpoint <- M
head(df_checkpoint)

df_pre_stm <- df_checkpoint %>%
  rename(
    Authors = AU,
    Author_Keywords = DE, #Include in STM
    Keyword_Plus = ID, #Include in STM
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

#create variable for country and continent
library(stringi)
head(df_pre_stm$Author_Keywords)
df_pre_stm$Author_Keywords <- ifelse(is.na(df_pre_stm$Author_Keywords) == TRUE, "", df_pre_stm$Author_Keywords)
df_pre_stm$Keyword_Plus <- ifelse(is.na(df_pre_stm$Keyword_Plus) == TRUE, "", df_pre_stm$Keyword_Plus)
colSums(is.na(df_pre_stm))
head(df_pre_stm)
country_vec <- df_countries$Country
country_vec
#now find the country in the Author_Address string
df_pre_stm$Country = stri_extract_first(df_pre_stm$Author_Address, 
                                        regex = sprintf(r="(\b(%s)\b)", 
                                                        stri_c(country_vec,collapse = "|")))
df_pre_stm$Country

#now find the Country in the Author_Address string
df_pre_stm$Continent <- countrycode(sourcevar = df_pre_stm[, "Country"],
                                    origin = "country.name",
                                    destination = "continent")
df_pre_stm$Continent

#make all the null countries and continents "Other"
df_pre_stm$Country <- ifelse(is.na(df_pre_stm$Country), "OTHER", df_pre_stm$Country)
head(df_pre_stm$Country)

df_pre_stm$Continent <- ifelse(is.na(df_pre_stm$Continent), "OTHER", df_pre_stm$Continent)
head(df_pre_stm$Continent)
table(df_pre_stm$Continent)

#now proceed with a subset ready for STM
dim(df_pre_stm)
df_stm <- df_pre_stm
df_stm <- subset(df_stm, select = c("Document_Title", "Abstract", "Year_Published", "Publication_Name", "Times_Cited", "Country", "Continent", "Author_Keywords", "Keyword_Plus"))
colSums(is.na(df_stm))
df_stm <- df_stm %>% drop_na
head(df_stm)
colSums(is.na(df_stm))
df_stm$Decade <- df_stm$Year_Published
str_sub(df_stm$Decade, 4, 4) <- "0"; df_stm$Decade 
table(df_stm$Decade)
df_stm$Publication_Name <- as.factor(df_stm$Publication_Name)
str(df_stm)
dim(df_stm)

suppressWarnings(library(stm))
suppressWarnings(library(stringr))

#stemming and stopword removal
?textProcessor()
colnames(df_stm)
processed <- textProcessor(df_stm$Abstract, 
                           metadata = df_stm,
                           removestopwords = TRUE,
                           removepunctuation = TRUE,
                           removenumbers = FALSE,
                           stem = FALSE,
                           wordLengths = c(3, Inf),
                           #custompunctuation = c(".", "/", "!", ",", "?", "(", ")", "[", "]", ":", ";", "\\", "{", "}"),
                           verbose = TRUE)

out <- prepDocuments(processed$documents, 
                     processed$vocab, 
                     processed$meta,
                     lower.thresh = 5, #low frequency words removed
                     verbose = TRUE)

out$docs.removed #check to see if prepDocuments has removed any documents from the corpus

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

#identify documents where the word "system" has been used
wntword <- which(processed$vocab == "system")

filterdocs <- lapply(processed$documents, 
                     function(ch) grep(wntword, ch))

indexList <- filterdocs[sapply(filterdocs, function(x) length(x) > 0)]

head(df_stm[as.numeric(names(indexList)),1:3])

#run a test on different size (K) topic models to see what is the optimal amount of topics
str(df_stm)
storage1<-searchK(docs, 
                  vocab, 
                  K = c(5, 10, 15, 20, 25, 30, 35, 40, 50, 60, 70), 
                  prevalence=~ Country + s(Year_Published) + s(Times_Cited), 
                  data = meta,
                  set.seed(9999), 
                  verbose=TRUE)

print(storage1$results)
options(repr.plot.width=6, repr.plot.height=6)
plot(storage1)

?unnest
unnest_list <- unnest(storage1$result, c(K, exclus, semcoh, heldout, residual, bound, lbound, em.its))
df_results <- data.frame(unnest_list)
df_results

require(gridExtra)
plot1 <- ggplot(data = df_results, 
       aes(x = K, y = exclus, group = 1)) +
  geom_line(linetype = "dashed")+
  geom_point() + 
  labs(x = "Number of Topics",
       y = "Exclusivity",
       title = "Exclusivity")

plot1

plot2 <- ggplot(data = df_results, 
                aes(x = K, y = semcoh, group = 1)) +
  geom_line(linetype = "dashed")+
  geom_point() + 
  labs(x = "Number of Topics",
       y = "Semantic Coherance",
       title = "Semantic Coherance")

plot3 <- ggplot(data = df_results, 
                aes(x = K, y = heldout, group = 1)) +
  geom_line(linetype = "dashed")+
  geom_point() + 
  labs(x = "Number of Topics",
       y = "Held-Out Likelihood",
       title = "Held-Out Likelihood")

plot4 <- ggplot(data = df_results, 
                aes(x = K, y = residual, group = 1)) +
  geom_line(linetype = "dashed")+
  geom_point() + 
  labs(x = "Number of Topics",
       y = "Residuals",
       title = "Residuals")

plot5 <- ggplot(data = df_results, 
                aes(x = K, y = lbound, group = 1)) +
  geom_line(linetype = "dashed")+
  geom_point() + 
  labs(x = "Number of Topics",
       y = "Lower Bound",
       title = "Lower Bound")
  
grid.arrange(plot3, plot4, plot2, plot1, ncol=2)

#select 3 models to test
model_22 <- stm(documents=out$documents, 
                     vocab=out$vocab, 
                     prevalence =~ Decade + Continent + s(Times_Cited), 
                     K=22, data=out$meta, init.type = "Spectral", verbose = TRUE)

model_24 <- stm(documents=out$documents, 
                     vocab=out$vocab, 
                     prevalence =~ Year_Published + Country + s(Times_Cited),
                     K=24, data=out$meta, init.type = "Spectral", verbose = TRUE)

model_26 <- stm(documents=out$documents, 
                     vocab=out$vocab, 
                     prevalence =~ Decade + Continent + s(Times_Cited), 
                     K=26, data=out$meta, init.type = "Spectral", verbose = TRUE)

model_28 <- stm(documents=out$documents, 
                    vocab=out$vocab, 
                    prevalence =~ Decade + Continent + s(Times_Cited), 
                    K=28, data=out$meta, init.type = "Spectral", verbose = TRUE)


#exclusivity against semantic coherence per topic per model
suppressWarnings(library(ggplot2))
suppressWarnings(library(htmlwidgets))

M22_ExSem<-as.data.frame(cbind(c(1:22),exclusivity(model_22), semanticCoherence(model=model_22, docs), "Mod22"))
M24_ExSem<-as.data.frame(cbind(c(1:24),exclusivity(model_24), semanticCoherence(model=model_24, docs), "Mod24"))
M26_ExSem<-as.data.frame(cbind(c(1:26),exclusivity(model_26), semanticCoherence(model=model_26, docs), "Mod26"))
M28_ExSem<-as.data.frame(cbind(c(1:28),exclusivity(model_28), semanticCoherence(model=model_28, docs), "Mod28"))

Models_ExSem<-rbind(M22_ExSem, M24_ExSem, M26_ExSem, M28_ExSem)
colnames(Models_ExSem)<-c("K","Exclusivity", "SemanticCoherence", "Model")

Models_ExSem$Exclusivity<-as.numeric(as.character(Models_ExSem$Exclusivity))
Models_ExSem$SemanticCoherence<-as.numeric(as.character(Models_ExSem$SemanticCoherence))

#plot chosen models
options(repr.plot.width = 8, repr.plot.height = 8, repr.plot.res = 100)
plotexcoer <- ggplot(Models_ExSem, aes(SemanticCoherence, Exclusivity, color = Model)) + 
  geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label = K), nudge_y = .04) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")
plotexcoer

#model analysis
topics_Mod24 <- make.dt(model_24, meta)
head(topics_Mod24,2)

tableau_extract <- topics_Mod24[,c(1:26, 28:33)]#visualize theta proportions, and other metadata
head(tableau_extract)
tableau_extract$topics <- colnames(tableau_extract[,c(2:25)])[apply(tableau_extract[,c(2:25)],1,which.max)]
table(tableau_extract$topics)
getwd()
write.csv(tableau_extract,"topics_listed.csv", row.names = TRUE)

topics_Mod24[1:5,c(1:26, 28:33)]#visualize proportions, and other metadata

#visualize the estimates of document-topic proportions 
options(repr.plot.width=10, repr.plot.height=15)
plot.STM(model_24, "hist")

#plot of which topics are coming from which documents
suppressWarnings(library(tidytext))# the package sometimes is not loaded correctly. If this happens, you might have to re-start the kernel 
td_theta <- tidytext::tidy(model_24, matrix = "theta")
td_theta

selectiontdthteta<-td_theta[td_theta$document%in%c(1:30),]#select the first 30 documents. be careful to select a sensible interval, as attempting to load a very huge corpus might crash the kernel

thetaplot1<-ggplot(selectiontdthteta, aes(y=gamma, x=as.factor(topic), fill = as.factor(topic))) +
  geom_bar(stat="identity",alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ document, ncol = 5) +
  labs(title = "Theta values per document",
       y = expression(theta), x = "Topic")

options(repr.plot.width=10, repr.plot.height=7, repr.plot.res=100)
thetaplot1

#look at the word frequencies per topic
suppressWarnings(library(dplyr))
#install.packages("drlib")
suppressWarnings(library(drlib))#drlib is available on github and needs devtools to be installed

td_beta <- tidytext::tidy(model_24)

options(repr.plot.width=10, repr.plot.height=8, repr.plot.res=100)
td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")

#more detailed look at the word distribution within each topic
betaT1_6 <- td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>% filter(topic == "Topic 1" | topic == "Topic 2" |
                                                             topic == "Topic 3" | topic == "Topic 4" |
                                                             topic == "Topic 5" | topic == "Topic 6")

betaT7_12 <- td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>% filter(topic == "Topic 7" | topic == "Topic 8" |
                                                                topic == "Topic 9" | topic == "Topic 10" |
                                                                topic == "Topic 11" | topic == "Topic 12")

betaT13_18 <- td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>% filter(topic == "Topic 13" | topic == "Topic 14" |
                                                                topic == "Topic 15" | topic == "Topic 16" |
                                                                topic == "Topic 17" | topic == "Topic 18")

betaT19_24 <- td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>% filter(topic == "Topic 19" | topic == "Topic 20" |
                                                                topic == "Topic 21" | topic == "Topic 22" |
                                                                topic == "Topic 23" | topic == "Topic 24")

betaT15_and_21 <- td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>% filter(topic == "Topic 15" | topic == "Topic 21")

betaplotT1_6<-ggplot(betaT1_6[betaT1_6$beta > 0.004,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity") + 
  coord_flip() + 
  facet_wrap(~ topic, scales = "free_y") +
  labs(x ="Terms", y = expression(beta),title = "Word probabilities for Topics 1-6")

betaplotT7_12<-ggplot(betaT7_12[betaT7_12$beta > 0.004,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity") + 
  coord_flip() + 
  facet_wrap(~ topic, scales = "free_y") +
  labs(x ="Terms", y = expression(beta),title = "Word probabilities for Topics 7-12")

betaplotT13_18<-ggplot(betaT13_18[betaT13_18$beta > 0.004,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity") + 
  coord_flip() + 
  facet_wrap(~ topic, scales = "free_y") +
  labs(x ="Terms", y = expression(beta),title = "Word probabilities for Topics 13-18")

betaplotT19_24<-ggplot(betaT19_24[betaT19_24$beta > 0.004,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity") + 
  coord_flip() + 
  facet_wrap(~ topic, scales = "free_y") +
  labs(x ="Terms", y = expression(beta),title = "Word probabilities for Topics 19-24")

betaplotT15_and_21<-ggplot(betaT15_and_21[betaT15_and_21$beta > 0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity") + 
  coord_flip() + 
  facet_wrap(~ topic, scales = "free_y") +
  labs(x ="Terms", y = expression(beta),title = "Word probabilities for Topics 15 and 21")

options(repr.plot.width=9, repr.plot.height=10, repr.plot.res=100)
betaplotT1_6
betaplotT7_12
betaplotT13_18
betaplotT19_24
betaplotT15_and_21

#visualize the topic distribution (which topics are overall more common)
options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)
par(mfrow=c(1,1))
plot.STM(model_24, "summary", n=7)# distribution and top 5 words per topic

#make comparisons between topics and understand more which differences there are between them
labelTopics(model_24, topics=c(5,6,9), n=10)# complete list of top 10 words per topics 5-6-9
labelTopics(model_24, n=10)

par(mfrow=c(1,1), mar=c(0,2,0,0))
options(repr.plot.width=5, repr.plot.height=3, repr.plot.res=100)
plot.STM(model_24, "labels", topics=c(5,6,9), label="frex", n=10, width=55)#top 10 FREX words per topics 5-6-9

#glimpse at highly representative documents per each topic with findThoughts
colnames(df_stm)
thoughts1 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=1, n=4)$docs[[1]]
thoughts1_1 <- findThoughts(model_24,texts=df_stm$Abstract, topics=1, n=2)$docs[[1]]
thoughts1_2 <- findThoughts(model_24,texts=df_stm$Author_Keywords, topics=1, n=4)$docs[[1]]
thoughts1_3 <- findThoughts(model_24,texts=df_stm$Keyword_Plus, topics=1, n=4)$docs[[1]]

thoughts2 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=2, n=4)$docs[[1]]
thoughts3 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=3, n=4)$docs[[1]]
thoughts4 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=4, n=4)$docs[[1]]

thoughts5 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=5, n=4)$docs[[1]]
thoughts6 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=6, n=4)$docs[[1]]
thoughts7 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=7, n=4)$docs[[1]]
thoughts8 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=8, n=4)$docs[[1]]

thoughts9 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=9, n=4)$docs[[1]]
thoughts10 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=10, n=4)$docs[[1]]
thoughts11 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=11, n=4)$docs[[1]]
thoughts12 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=12, n=4)$docs[[1]]

thoughts13 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=13, n=4)$docs[[1]]
thoughts14 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=14, n=4)$docs[[1]]
thoughts15 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=15, n=4)$docs[[1]]
thoughts16 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=16, n=4)$docs[[1]]

thoughts17 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=17, n=4)$docs[[1]]
thoughts18 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=18, n=4)$docs[[1]]
thoughts19 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=19, n=4)$docs[[1]]
thoughts20 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=20, n=4)$docs[[1]]

thoughts21 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=21, n=4)$docs[[1]]
thoughts22 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=22, n=4)$docs[[1]]
thoughts23 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=23, n=4)$docs[[1]]
thoughts24 <- findThoughts(model_24,texts=df_stm$Document_Title, topics=24, n=4)$docs[[1]]

options(repr.plot.width=10, repr.plot.height=12, repr.plot.res=100)
par(mfrow=c(1,4), mar=c(0,0,2,2))
plotQuote(thoughts1, width=15, maxwidth=200, text.cex=0.7, main="Topic 1 Title")
plotQuote(thoughts1_1, width=15, maxwidth=200, text.cex=0.8, main="Topic 1 Abstract")
plotQuote(thoughts1_2, width=20, maxwidth=200, text.cex=0.7, main="Topic 1 Author Keywords")
plotQuote(thoughts1_3, width=30, maxwidth=200, text.cex=0.55, main="Topic 1 Keywords Plus")


plotQuote(thoughts2, width=15, maxwidth=200, text.cex=0.8, main="Topic 2 Title")
plotQuote(thoughts3, width=15, maxwidth=200, text.cex=0.8, main="Topic 3")
plotQuote(thoughts4, width=15, maxwidth=200, text.cex=0.8, main="Topic 4")

par(mfrow=c(1,4), mar=c(0,0,2,2))
plotQuote(thoughts5, width=15, maxwidth=200, text.cex=0.8, main="Topic 5")
plotQuote(thoughts6, width=15, maxwidth=200, text.cex=0.8, main="Topic 6")
plotQuote(thoughts7, width=15, maxwidth=200, text.cex=0.8, main="Topic 7")
plotQuote(thoughts8, width=15, maxwidth=200, text.cex=0.8, main="Topic 8")

par(mfrow=c(1,4), mar=c(0,0,2,2))
plotQuote(thoughts9, width=15, maxwidth=200, text.cex=0.8, main="Topic 9")
plotQuote(thoughts10, width=15, maxwidth=200, text.cex=0.8, main="Topic 10")
plotQuote(thoughts11, width=15, maxwidth=200, text.cex=0.8, main="Topic 11")
plotQuote(thoughts12, width=15, maxwidth=200, text.cex=0.8, main="Topic 12")

par(mfrow=c(1,4), mar=c(0,0,2,2))
plotQuote(thoughts13, width=15, maxwidth=200, text.cex=0.8, main="Topic 13")
plotQuote(thoughts14, width=15, maxwidth=200, text.cex=0.8, main="Topic 14")
plotQuote(thoughts15, width=15, maxwidth=200, text.cex=0.8, main="Topic 15")
plotQuote(thoughts16, width=15, maxwidth=200, text.cex=0.8, main="Topic 16")

par(mfrow=c(1,4), mar=c(0,0,2,2))
plotQuote(thoughts17, width=15, maxwidth=200, text.cex=0.8, main="Topic 17")
plotQuote(thoughts18, width=15, maxwidth=200, text.cex=0.8, main="Topic 18")
plotQuote(thoughts19, width=15, maxwidth=200, text.cex=0.8, main="Topic 19")
plotQuote(thoughts20, width=15, maxwidth=200, text.cex=0.8, main="Topic 20")

par(mfrow=c(1,4), mar=c(0,0,2,2))
plotQuote(thoughts21, width=15, maxwidth=200, text.cex=0.8, main="Topic 21")
plotQuote(thoughts22, width=15, maxwidth=200, text.cex=0.8, main="Topic 22")
plotQuote(thoughts23, width=15, maxwidth=200, text.cex=0.8, main="Topic 23")
plotQuote(thoughts24, width=15, maxwidth=200, text.cex=0.8, main="Topic 24")

#original exclusivity-semantic coherence plot
options(repr.plot.width=7, repr.plot.height=6, repr.plot.res=100)
ggplot(Models_ExSem[Models_ExSem$Model=="Mod24",], aes(SemanticCoherence, Exclusivity, color = Model)) + 
  geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_x = 1, nudge_y = .017) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence for model with 24 topics")

options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)
par(mfrow=c(2,2), mar=c(0,0,2,2))
plot.STM(model_24, "perspectives", topics=c(9,13))
plot.STM(model_24, "perspectives", topics=c(3,11))
plot.STM(model_24, "perspectives", topics=c(3,22))
plot.STM(model_24, "perspectives", topics=c(11,22))
dev.off()

#word clouds
par(mfrow=c(2,2), mar=c(0,0,2,2))
cloud(model_24, topic = 3, max.words = 200)
cloud(model_24, topic = 9, max.words = 200)
cloud(model_24, topic = 22, max.words = 200)
cloud(model_24, topic = 23, max.words = 200)
dev.off()

#topic correlaiton
mod.out.corr <- topicCorr(model_24)
par(mfrow=c(1,1))
plot(mod.out.corr)

#Correlation Plots
#install.packages("corrplot")
library(corrplot)
?corrplot

m <- mod.out.corr$cor
colnames(m) <- paste("Topic", 1:24, sep = " ")
rownames(m) <- paste("Topic", 1:24, sep = " ")
m

corrplot(m, method="color", tl.cex = 0.65)

corrplot(m, method="pie")


# Using different color spectrum
col <- colorRampPalette(c("red", "white", "blue"))(20)
corrplot(m, type="upper", order="hclust", col=col, tl.cex = 0.65)

corrplot(m, method="number", tl.cex = 0.65, number.cex = 0.5)


