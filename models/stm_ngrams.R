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

head(df_stm)
colnames(df_stm)

model <- head(df_stm, 2000)
model <- df_stm
model$Original_Text <- model$Abstract
colnames(model)
colSums(is.na(model))
dim(model)

?corpus
my_corpus <- corpus(model, text_field = 'Abstract')

?texts
tokens_clean <- tokens(my_corpus, 
                       what = "word",
                       remove_numbers = TRUE, 
                       remove_punct = TRUE,
                       remove_symbols = TRUE,
                       split_hyphens = FALSE,
                       include_docvars = TRUE, 
                       verbose = TRUE)

# remove stopwords and convert to lower-case
?stopwords
tokens_clean <- tokens_clean %>%
  tokens_remove(stopwords(language = 'en', source = "smart")) %>%
  #tokens_wordstem() %>%
  tokens_tolower()

tokens_ngrams <- tokens_ngrams(tokens_clean, n = 1:3)

print(tokens_ngrams[5])
tokens_ngrams$Publication_Name[1]
tokens_ngrams$Document_Title[1]
tokens_ngrams$Original_Text[1]

?dfm
dfm_corpus <- dfm(tokens_ngrams,
                  verbose = TRUE)
dim(dfm_corpus)

?dfm_trim
dfm_corpus_trim <- dfm_trim(dfm_corpus,
                            min_termfreq = 5,
                            #max_docfreq = 0.90,
                            termfreq_type = "count",
                            verbose = TRUE)
dim(dfm_corpus_trim)

#do a little wordcloud tester
textplot_wordcloud(
  dfm_corpus_trim,
  min_size = 0.5, max_size = 4,
  min_count = 3, max_words = 500,
  color = "darkgreen",
  font = NULL,
  adjust = 0, rotation = 0.1,
  random_order = FALSE, random_color = FALSE, ordered_color = FALSE,
  labelcolor = "gray20",
  labelsize = 1.5, labeloffset = 0,
  fixed_aspect = TRUE, comparison = FALSE)

dfm_to_stm <- dfm_corpus_trim  #dfm_corpus_trim or dfm_corpus

out <- convert(dfm_corpus_trim, to = "stm")  # convert to stm format

docs <- out$documents
docs
vocab <- out$vocab
vocab
meta <- out$meta

colnames(meta)

storage1<-searchK(docs, 
                  vocab, 
                  K = c(10, 15, 20, 25, 30), 
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
model_13 <- stm(documents = out$documents, 
                vocab = out$vocab, 
                prevalence =~ Country + s(Year_Published) + s(Times_Cited), 
                K = 13, 
                data = out$meta, 
                init.type = "Spectral", 
                verbose = TRUE)

model_15 <- stm(documents = out$documents, 
                vocab = out$vocab, 
                prevalence =~ Country + s(Year_Published) + s(Times_Cited), 
                K = 15, 
                data = out$meta, 
                init.type = "Spectral", 
                verbose = TRUE)

model_17 <- stm(documents = out$documents, 
                vocab = out$vocab, 
                prevalence =~ Country + s(Year_Published) + s(Times_Cited), 
                K = 17, 
                data = out$meta, 
                init.type = "Spectral", 
                verbose = TRUE)

#exclusivity against semantic coherence per topic per model
suppressWarnings(library(ggplot2))
suppressWarnings(library(htmlwidgets))

M13_Excl_Sem <- as.data.frame(cbind(c(1:13), exclusivity(model_13), semanticCoherence(model = model_13, docs), "Model K = 13"))
M15_Excl_Sem <- as.data.frame(cbind(c(1:15),exclusivity(model_15), semanticCoherence(model = model_15, docs), "Model K = 15"))
M17_Excl_Sem <- as.data.frame(cbind(c(1:17),exclusivity(model_17), semanticCoherence(model = model_17, docs), "Model K = 17"))


Models_Excl_Sem <- rbind(M13_Excl_Sem, M15_Excl_Sem, M17_Excl_Sem)
colnames(Models_Excl_Sem)<-c("K","Exclusivity", "SemanticCoherence", "Model")

Models_Excl_Sem$Exclusivity <- as.numeric(as.character(Models_Excl_Sem$Exclusivity))
Models_Excl_Sem$SemanticCoherence <- as.numeric(as.character(Models_Excl_Sem$SemanticCoherence))

#plot chosen models
options(repr.plot.width = 8, repr.plot.height = 8, repr.plot.res = 100)
plotexcoer <- ggplot(Models_Excl_Sem, aes(SemanticCoherence, Exclusivity, color = Model)) + 
  geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label = K), nudge_y = .04) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")
plotexcoer

#model analysis of chosen model
K_topics_chosen <- make.dt(model_24, meta)
head(K_topics_chosen,2)

tableau_extract <- K_topics_chosen[,c(1:26, 28:33)]#visualize theta proportions, and other metadata
head(tableau_extract)
tableau_extract$topics <- colnames(tableau_extract[,c(2:25)])[apply(tableau_extract[,c(2:25)],1,which.max)]
table(tableau_extract$topics)
getwd()
write.csv(tableau_extract,"topics_listed.csv", row.names = TRUE)

K_topics_chosen[1:5,c(1:26, 28:33)]#visualize proportions, and other metadata

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

