#install.packages("bibliometrix")
library(bibliometrix)
library(dplyr)
library(tidyr)
library(stringr)

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
df_stm <- subset(df_stm, select = c("Document_Title", 
                                    "Abstract", 
                                    "Year_Published", 
                                    "Publication_Name", 
                                    "Times_Cited", 
                                    "Country", 
                                    "Continent", 
                                    "Author_Keywords", 
                                    "Keyword_Plus",
                                    "Language",
                                    "Document_Type"))

#remove nulls
colSums(is.na(df_stm))
df_stm <- df_stm %>% drop_na
head(df_stm)
colSums(is.na(df_stm))

#create a decade field
df_stm$Decade <- df_stm$Year_Published
str_sub(df_stm$Decade, 4, 4) <- "0"; df_stm$Decade 
table(df_stm$Decade)
df_stm$Publication_Name <- as.factor(df_stm$Publication_Name)
str(df_stm)
dim(df_stm)

#Only english inputs
table(df_stm$Language)
df_stm <- subset(df_stm, df_stm$Language == "ENGLISH")

#Only academic publications
table(df_stm$Document_Type)
df_stm <- subset(df_stm, df_stm$Document_Type == "ARTICLE" | df_stm$Document_Type == "CONFERENCE PAPER")
table(df_stm$Document_Type)

#Remove duplication
df_stm <- df_stm %>%
  distinct(Document_Title, Abstract, .keep_all = TRUE)

head(df_stm)
colnames(df_stm)

#start by preparing the data with the quanteda package (allows ngrams!)
model <- df_stm
model$Original_Text <- model$Abstract #this allows you to see the original text when you do exploration
colnames(model)
colSums(is.na(model))
dim(model)

#?corpus
library(quanteda)
library(quanteda.textplots)
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
#?stopwords
tokens_clean <- tokens_clean %>%
  tokens_remove(stopwords(language = 'en', source = "smart")) %>%
  #tokens_wordstem() %>%  #i do not want to stem at this point in time
  tokens_tolower()

tokens_ngrams <- tokens_ngrams(tokens_clean, n = 1:2)

print(tokens_ngrams[2])
tokens_ngrams$Publication_Name[1]
tokens_ngrams$Document_Title[1]
tokens_ngrams$Original_Text[1]

?dfm
dfm_corpus <- dfm(tokens_ngrams,
                  verbose = TRUE)
dim(dfm_corpus)

?dfm_trim
dfm_corpus_trim <- dfm_trim(dfm_corpus,
                            min_termfreq = 7,
                            #max_docfreq = 0.90,
                            termfreq_type = "count",
                            verbose = TRUE)
dim(dfm_corpus_trim)

#do a little wordcloud tester
par(mfrow=c(1,1))
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
vocab <- out$vocab
vocab
meta <- out$meta
colnames(meta)

# Start the clock
timer_start <- proc.time()
storage1<-searchK(docs, 
                  vocab, 
                  K = c(5, 10, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 25, 30, 40, 60, 80, 100), 
                  prevalence=~ Country + s(Year_Published) + s(Times_Cited), 
                  data = meta,
                  set.seed(9999), 
                  verbose = TRUE)
# Stop the clock
timer_end <- proc.time() - ptm
timer_end

print(storage1$results)
options(repr.plot.width=6, repr.plot.height=6)
plot(storage1)

?unnest
unnest_list <- unnest(storage1$result, c(K, exclus, semcoh, heldout, residual, bound, lbound, em.its))
df_results <- data.frame(unnest_list)
df_results

csv_path = "/Users/danieungerer/Documents/Meesters/Klasse/MIT807/mit807_power_of_metadata/data"
setwd(csv_path)
getwd()
write.csv(df_results,"model_performance.csv", row.names = TRUE)

require(gridExtra)
library(ggplot2)
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

# select 4 models to test (K = c(15, 17, 19, 20))
# start the clock!
timer_start <- proc.time()

model_16 <- stm(documents = out$documents, 
                vocab = out$vocab, 
                prevalence =~ Country + s(Year_Published) + s(Times_Cited), 
                K = 16, 
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

model_18 <- stm(documents = out$documents, 
                vocab = out$vocab, 
                prevalence =~ Country + s(Year_Published) + s(Times_Cited), 
                K = 18, 
                data = out$meta, 
                init.type = "Spectral", 
                verbose = TRUE)

model_19 <- stm(documents = out$documents, 
                vocab = out$vocab, 
                prevalence =~ Country + s(Year_Published) + s(Times_Cited), 
                K = 19, 
                data = out$meta, 
                init.type = "Spectral", 
                verbose = TRUE)

model_20 <- stm(documents = out$documents, 
                vocab = out$vocab, 
                prevalence =~ Country + s(Year_Published) + s(Times_Cited), 
                K = 20, 
                data = out$meta, 
                init.type = "Spectral", 
                verbose = TRUE)

model_21 <- stm(documents = out$documents, 
                vocab = out$vocab, 
                prevalence =~ Country + s(Year_Published) + s(Times_Cited), 
                K = 21, 
                data = out$meta, 
                init.type = "Spectral", 
                verbose = TRUE)

# Stop the clock
timer_end <- proc.time() - timer_start
timer_end

#exclusivity against semantic coherence per topic per model
suppressWarnings(library(ggplot2))
suppressWarnings(library(htmlwidgets))

M16_Excl_Sem <- as.data.frame(cbind(c(1:16), exclusivity(model_16), semanticCoherence(model = model_16, docs), "Model K = 16"))
M17_Excl_Sem <- as.data.frame(cbind(c(1:17), exclusivity(model_17), semanticCoherence(model = model_17, docs), "Model K = 17"))
M18_Excl_Sem <- as.data.frame(cbind(c(1:18), exclusivity(model_18), semanticCoherence(model = model_18, docs), "Model K = 18"))
M19_Excl_Sem <- as.data.frame(cbind(c(1:19), exclusivity(model_19), semanticCoherence(model = model_19, docs), "Model K = 19"))
M20_Excl_Sem <- as.data.frame(cbind(c(1:20), exclusivity(model_20), semanticCoherence(model = model_20, docs), "Model K = 20"))
M21_Excl_Sem <- as.data.frame(cbind(c(1:21), exclusivity(model_21), semanticCoherence(model = model_21, docs), "Model K = 21"))

Models_Excl_Sem <- rbind(M16_Excl_Sem, M17_Excl_Sem, M18_Excl_Sem, M19_Excl_Sem, M20_Excl_Sem, M21_Excl_Sem)
colnames(Models_Excl_Sem)<-c("K","Exclusivity", "SemanticCoherence", "Model")
head(Models_Excl_Sem)

Models_Excl_Sem$Exclusivity <- as.numeric(as.character(Models_Excl_Sem$Exclusivity))
Models_Excl_Sem$SemanticCoherence <- as.numeric(as.character(Models_Excl_Sem$SemanticCoherence))
head(Models_Excl_Sem)

csv_path = "/Users/danieungerer/Documents/Meesters/Klasse/MIT807/mit807_power_of_metadata/data"
setwd(csv_path)
getwd()
write.csv(Models_Excl_Sem,"detail_model_comaprison.csv", row.names = TRUE)

#plot chosen models
options(repr.plot.width = 8, repr.plot.height = 8, repr.plot.res = 100)
plotexcoer <- ggplot(Models_Excl_Sem, aes(SemanticCoherence, Exclusivity, color = Model)) + 
  geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label = K), nudge_y = .04) +
  labs(x = "Semantic Coherence",
       y = "Exclusivity",
       title = "Comparing Exclusivity and Semantic Coherence") #+
  #facet_grid(rows = vars(Row), cols = vars(Col)) +
  #theme(strip.text.x = element_blank(), strip.text.y = element_blank())
plotexcoer

#model analysis of chosen model
chosen_model <- model_17
K_topics_chosen <- make.dt(chosen_model, meta)
colnames(K_topics_chosen)
dim(K_topics_chosen)

tableau_extract <- K_topics_chosen[ , c(2:24, 29)]#visualize theta proportions, and other metadata
head(tableau_extract)
tableau_extract$Topics <- colnames(tableau_extract[ , c(1:17)])[apply(tableau_extract[ , c(1:17)], 1 , which.max)]
table(tableau_extract$Topics)
colnames(tableau_extract)

csv_path = "/Users/danieungerer/Documents/Meesters/Klasse/MIT807/mit807_power_of_metadata/data"
setwd(csv_path)
getwd()
write.csv(tableau_extract,"topics_listed.csv", row.names = TRUE)

#visualize the estimates of document-topic proportions 
options(repr.plot.width=10, repr.plot.height=15)
plot.STM(chosen_model, "hist")

#plot of which topics are coming from which documents
suppressWarnings(library(tidytext))# the package sometimes is not loaded correctly. If this happens, you might have to re-start the kernel 
td_theta <- tidytext::tidy(chosen_model, matrix = "theta")
head(td_theta,4)

csv_path = "/Users/danieungerer/Documents/Meesters/Klasse/MIT807/mit807_power_of_metadata/data"
setwd(csv_path)
getwd()
write.csv(td_theta,"theta_values_per_document.csv", row.names = TRUE)

selectiontdthteta<-td_theta[td_theta$document%in%c(1:20),]#select the first 30 documents. be careful to select a sensible interval, as attempting to load a very huge corpus might crash the kernel

thetaplot1<-ggplot(selectiontdthteta, aes(y=gamma, x=as.factor(topic), fill = as.factor(topic))) +
  geom_bar(stat="identity",alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ document, ncol = 4) +
  labs(title = "Theta values per document",
       y = expression(theta), x = "Topic")

options(repr.plot.width=10, repr.plot.height=7, repr.plot.res=100)
thetaplot1

#compare well clear theta topics and not so clear
model$Document_Title[4]
model$Abstract[4]
tableau_extract[4]

model$Document_Title[15]
model$Abstract[15]
tableau_extract[15]

#look at the word frequencies per topic
suppressWarnings(library(dplyr))

td_beta <- tidytext::tidy(chosen_model)
head(td_beta)

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
beta_group_1 <- td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>% filter(topic == "Topic 1" | topic == "Topic 2" | topic == "Topic 3" | 
                                                                topic == "Topic 4" | topic == "Topic 5")
beta_group_1


beta_group_2 <- td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>% filter(topic == "Topic 6" | topic == "Topic 7" | topic == "Topic 8" | 
                                                                topic == "Topic 9" | topic == "Topic 10")

beta_group_3 <- td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>% filter(topic == "Topic 11" | topic == "Topic 12" | topic == "Topic 13" | 
                                                                topic == "Topic 14" | topic == "Topic 16")

beta_group_4 <- td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>% filter(topic == "Topic 16" | topic == "Topic 17" | topic == "Topic 18" | 
                                                                topic == "Topic 19" | topic == "Topic 20")

library(ggplot2)

betaplot_group_1 <- ggplot(beta_group_1[beta_group_1$beta > 0.005,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity") + 
  coord_flip() + 
  facet_wrap(~ topic, scales = "free_y") +
  labs(x ="Terms", y = expression(beta),title = "Word probabilities for Topics 1-5")

betaplot_group_2 <- ggplot(beta_group_2[beta_group_2$beta > 0.005,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity") + 
  coord_flip() + 
  facet_wrap(~ topic, scales = "free_y") +
  labs(x ="Terms", y = expression(beta),title = "Word probabilities for Topics 6-10")

betaplot_group_3 <- ggplot(beta_group_3[beta_group_3$beta > 0.005,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity") + 
  coord_flip() + 
  facet_wrap(~ topic, scales = "free_y") +
  labs(x ="Terms", y = expression(beta),title = "Word probabilities for Topics 11-15")

betaplot_group_4 <- ggplot(beta_group_4[beta_group_4$beta > 0.005,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity") + 
  coord_flip() + 
  facet_wrap(~ topic, scales = "free_y") +
  labs(x ="Terms", y = expression(beta),title = "Word probabilities for Topics 16-20")

options(repr.plot.width=9, repr.plot.height=10, repr.plot.res=100)
betaplot_group_1
betaplot_group_2
betaplot_group_3
betaplot_group_4

#visualize the topic distribution (which topics are overall more common)
options(repr.plot.width = 7, repr.plot.height = 7, repr.plot.res = 100)
par(mfrow=c(1,1))
plot.STM(chosen_model, "summary", n = 7)# distribution and top 5 words per topic

#make comparisons between topics and understand more which differences there are between them
labelTopics(chosen_model, topics=c(5,6,9), n=10)# complete list of top 10 words per topics 5-6-9
?labelTopics
labelTopics(chosen_model, n = 10)

par(mfrow=c(1,1), mar=c(0,2,0,0))
options(repr.plot.width=5, repr.plot.height=3, repr.plot.res=100)
plot.STM(chosen_model, "labels", topics=c(5,6,9), label="frex", n=10, width=55)#top 10 FREX words per topics 5-6-9

#glimpse at highly representative documents per each topic with findThoughts
colnames(model)
thoughts1_0 <- findThoughts(chosen_model, texts = model$Document_Title, topics = 1, n = 4)$docs[[1]]
thoughts1_1 <- findThoughts(chosen_model, texts= model$Abstract, topics = 1, n = 2)$docs[[1]]
thoughts1_2 <- findThoughts(chosen_model, texts= model$Author_Keywords, topics = 1, n = 4)$docs[[1]]
thoughts1_3 <- findThoughts(chosen_model, texts= model$Keyword_Plus, topics = 1, n = 4)$docs[[1]]

?par
par(mfrow = c(1, 4), mai = c(1, 0.1, 0.1, 0.1), mar = c(0, 0, 2, 2))
plotQuote(thoughts1_0, width=15, maxwidth=200, text.cex=1, main="Topic 1 Title")
plotQuote(thoughts1_1, width=20, maxwidth=200, text.cex=0.9, main="Topic 1 Abstract")
plotQuote(thoughts1_2, width=25, maxwidth=200, text.cex=1, main="Topic 1 Author Keywords")
plotQuote(thoughts1_3, width=25, maxwidth=200, text.cex=1, main="Topic 1 Keywords Plus")

thoughts2_0 <- findThoughts(chosen_model, texts = model$Document_Title, topics = 2, n = 4)$docs[[1]]
thoughts2_1 <- findThoughts(chosen_model, texts= model$Abstract, topics = 2, n = 3)$docs[[1]]
thoughts2_2 <- findThoughts(chosen_model, texts= model$Author_Keywords, topics = 2, n = 4)$docs[[1]]
thoughts2_3 <- findThoughts(chosen_model, texts= model$Keyword_Plus, topics = 2, n = 4)$docs[[1]]

par(mfrow=c(1,4), mar=c(0.1,0,2,2))
plotQuote(thoughts2_0, width=15, maxwidth=200, text.cex=1, main="Topic 2 Title")
plotQuote(thoughts2_1, width=20, maxwidth=200, text.cex=0.9, main="Topic 2 Abstract")
plotQuote(thoughts2_2, width=25, maxwidth=200, text.cex=1, main="Topic 2 Author Keywords")
plotQuote(thoughts2_3, width=25, maxwidth=200, text.cex=1, main="Topic 2 Keywords Plus")

thoughts3_0 <- findThoughts(chosen_model, texts = model$Document_Title, topics = 3, n = 4)$docs[[1]]
thoughts3_1 <- findThoughts(chosen_model, texts= model$Abstract, topics = 3, n = 3)$docs[[1]]
thoughts3_2 <- findThoughts(chosen_model, texts= model$Author_Keywords, topics = 3, n = 4)$docs[[1]]
thoughts3_3 <- findThoughts(chosen_model, texts= model$Keyword_Plus, topics = 3, n = 4)$docs[[1]]

par(mfrow=c(1,4), mar = c(0, 0, 2, 2))
plotQuote(thoughts3_0, width=15, maxwidth=200, text.cex=1, main="Topic 3 Title")
plotQuote(thoughts3_1, width=20, maxwidth=200, text.cex=0.9, main="Topic 3 Abstract")
plotQuote(thoughts3_2, width=25, maxwidth=200, text.cex=1, main="Topic 3 Author Keywords")
plotQuote(thoughts3_3, width=25, maxwidth=200, text.cex=1, main="Topic 3 Keywords Plus")

thoughts4_0 <- findThoughts(chosen_model, texts = model$Document_Title, topics = 4, n = 4)$docs[[1]]
thoughts4_1 <- findThoughts(chosen_model, texts= model$Abstract, topics = 4, n = 3)$docs[[1]]
thoughts4_2 <- findThoughts(chosen_model, texts= model$Author_Keywords, topics = 4, n = 4)$docs[[1]]
thoughts4_3 <- findThoughts(chosen_model, texts= model$Keyword_Plus, topics = 4, n = 4)$docs[[1]]

par(mfrow=c(1,4), mar=c(0.1,0,2,2))
plotQuote(thoughts4_0, width=15, maxwidth=200, text.cex=1, main="Topic 4 Title")
plotQuote(thoughts4_1, width=20, maxwidth=200, text.cex=0.9, main="Topic 4 Abstract")
plotQuote(thoughts4_2, width=25, maxwidth=200, text.cex=1, main="Topic 4 Author Keywords")
plotQuote(thoughts4_3, width=25, maxwidth=200, text.cex=1, main="Topic 4 Keywords Plus")

thoughts5_0 <- findThoughts(chosen_model, texts = model$Document_Title, topics = 5, n = 4)$docs[[1]]
thoughts5_1 <- findThoughts(chosen_model, texts= model$Abstract, topics = 5, n = 3)$docs[[1]]
thoughts5_2 <- findThoughts(chosen_model, texts= model$Author_Keywords, topics = 5, n = 4)$docs[[1]]
thoughts5_3 <- findThoughts(chosen_model, texts= model$Keyword_Plus, topics = 5, n = 4)$docs[[1]]

par(mfrow=c(1,4), mar=c(0.1,0,2,2))
plotQuote(thoughts5_0, width=15, maxwidth=200, text.cex=1, main="Topic 5 Title")
plotQuote(thoughts5_1, width=20, maxwidth=200, text.cex=0.9, main="Topic 5 Abstract")
plotQuote(thoughts5_2, width=25, maxwidth=200, text.cex=1, main="Topic 5 Author Keywords")
plotQuote(thoughts5_3, width=25, maxwidth=200, text.cex=1, main="Topic 5 Keywords Plus")

thoughts6_0 <- findThoughts(chosen_model, texts = model$Document_Title, topics = 6, n = 4)$docs[[1]]
thoughts6_1 <- findThoughts(chosen_model, texts= model$Abstract, topics = 6, n = 3)$docs[[1]]
thoughts6_2 <- findThoughts(chosen_model, texts= model$Author_Keywords, topics = 6, n = 4)$docs[[1]]
thoughts6_3 <- findThoughts(chosen_model, texts= model$Keyword_Plus, topics = 6, n = 4)$docs[[1]]

par(mfrow=c(1,4), mar=c(0.1,0,2,2))
plotQuote(thoughts6_0, width=15, maxwidth=200, text.cex=1, main="Topic 6 Title")
plotQuote(thoughts6_1, width=20, maxwidth=200, text.cex=0.9, main="Topic 6 Abstract")
plotQuote(thoughts6_2, width=25, maxwidth=200, text.cex=1, main="Topic 6 Author Keywords")
plotQuote(thoughts6_3, width=25, maxwidth=200, text.cex=1, main="Topic 6 Keywords Plus")

thoughts7_0 <- findThoughts(chosen_model, texts = model$Document_Title, topics = 7, n = 4)$docs[[1]]
thoughts7_1 <- findThoughts(chosen_model, texts= model$Abstract, topics = 7, n = 3)$docs[[1]]
thoughts7_2 <- findThoughts(chosen_model, texts= model$Author_Keywords, topics = 7, n = 4)$docs[[1]]
thoughts7_3 <- findThoughts(chosen_model, texts= model$Keyword_Plus, topics = 7, n = 4)$docs[[1]]

par(mfrow=c(1,4), mar=c(0.1,0,2,2))
plotQuote(thoughts7_0, width=15, maxwidth=200, text.cex=1, main="Topic 7 Title")
plotQuote(thoughts7_1, width=20, maxwidth=200, text.cex=0.9, main="Topic 7 Abstract")
plotQuote(thoughts7_2, width=25, maxwidth=200, text.cex=1, main="Topic 7 Author Keywords")
plotQuote(thoughts7_3, width=25, maxwidth=200, text.cex=1, main="Topic 7 Keywords Plus")

thoughts8_0 <- findThoughts(chosen_model, texts = model$Document_Title, topics = 8, n = 4)$docs[[1]]
thoughts8_1 <- findThoughts(chosen_model, texts= model$Abstract, topics = 8, n = 3)$docs[[1]]
thoughts8_2 <- findThoughts(chosen_model, texts= model$Author_Keywords, topics = 8, n = 4)$docs[[1]]
thoughts8_3 <- findThoughts(chosen_model, texts= model$Keyword_Plus, topics = 8, n = 4)$docs[[1]]

par(mfrow=c(1,4), mar=c(0.1,0,2,2))
plotQuote(thoughts8_0, width=15, maxwidth=200, text.cex=1, main="Topic 8 Title")
plotQuote(thoughts8_1, width=20, maxwidth=200, text.cex=0.9, main="Topic 8 Abstract")
plotQuote(thoughts8_2, width=25, maxwidth=200, text.cex=1, main="Topic 8 Author Keywords")
plotQuote(thoughts8_3, width=25, maxwidth=200, text.cex=1, main="Topic 8 Keywords Plus")

thoughts9_0 <- findThoughts(chosen_model, texts = model$Document_Title, topics = 9, n = 4)$docs[[1]]
thoughts9_1 <- findThoughts(chosen_model, texts= model$Abstract, topics = 9, n = 3)$docs[[1]]
thoughts9_2 <- findThoughts(chosen_model, texts= model$Author_Keywords, topics = 9, n = 4)$docs[[1]]
thoughts9_3 <- findThoughts(chosen_model, texts= model$Keyword_Plus, topics = 9, n = 4)$docs[[1]]

par(mfrow=c(1,4), mar=c(0.1,0,2,2))
plotQuote(thoughts9_0, width=15, maxwidth=200, text.cex=1, main="Topic 9 Title")
plotQuote(thoughts9_1, width=20, maxwidth=200, text.cex=0.9, main="Topic 9 Abstract")
plotQuote(thoughts9_2, width=25, maxwidth=200, text.cex=1, main="Topic 9 Author Keywords")
plotQuote(thoughts9_3, width=25, maxwidth=200, text.cex=1, main="Topic 9 Keywords Plus")

thoughts10_0 <- findThoughts(chosen_model, texts = model$Document_Title, topics = 10, n = 4)$docs[[1]]
thoughts10_1 <- findThoughts(chosen_model, texts= model$Abstract, topics = 10, n = 3)$docs[[1]]
thoughts10_2 <- findThoughts(chosen_model, texts= model$Author_Keywords, topics = 10, n = 4)$docs[[1]]
thoughts10_3 <- findThoughts(chosen_model, texts= model$Keyword_Plus, topics = 10, n = 4)$docs[[1]]

par(mfrow=c(1,4), mar=c(0.1,0,2,2))
plotQuote(thoughts10_0, width=15, maxwidth=200, text.cex=1, main="Topic 10 Title")
plotQuote(thoughts10_1, width=20, maxwidth=200, text.cex=0.9, main="Topic 10 Abstract")
plotQuote(thoughts10_2, width=25, maxwidth=200, text.cex=1, main="Topic 10 Author Keywords")
plotQuote(thoughts10_3, width=25, maxwidth=200, text.cex=1, main="Topic 10 Keywords Plus")

thoughts11_0 <- findThoughts(chosen_model, texts = model$Document_Title, topics = 11, n = 4)$docs[[1]]
thoughts11_1 <- findThoughts(chosen_model, texts= model$Abstract, topics = 11, n = 3)$docs[[1]]
thoughts11_2 <- findThoughts(chosen_model, texts= model$Author_Keywords, topics = 11, n = 4)$docs[[1]]
thoughts11_3 <- findThoughts(chosen_model, texts= model$Keyword_Plus, topics = 11, n = 4)$docs[[1]]

par(mfrow=c(1,4), mar=c(0.1,0,2,2))
plotQuote(thoughts11_0, width=15, maxwidth=200, text.cex=1, main="Topic 11 Title")
plotQuote(thoughts11_1, width=20, maxwidth=200, text.cex=0.9, main="Topic 11 Abstract")
plotQuote(thoughts11_2, width=25, maxwidth=200, text.cex=1, main="Topic 11 Author Keywords")
plotQuote(thoughts11_3, width=25, maxwidth=200, text.cex=1, main="Topic 11 Keywords Plus")

thoughts12_0 <- findThoughts(chosen_model, texts = model$Document_Title, topics = 12, n = 4)$docs[[1]]
thoughts12_1 <- findThoughts(chosen_model, texts= model$Abstract, topics = 12, n = 3)$docs[[1]]
thoughts12_2 <- findThoughts(chosen_model, texts= model$Author_Keywords, topics = 12, n = 4)$docs[[1]]
thoughts12_3 <- findThoughts(chosen_model, texts= model$Keyword_Plus, topics = 12, n = 4)$docs[[1]]

par(mfrow=c(1,4), mar=c(0.1,0,2,2))
plotQuote(thoughts12_0, width=15, maxwidth=200, text.cex=1, main="Topic 12 Title")
plotQuote(thoughts12_1, width=20, maxwidth=200, text.cex=0.9, main="Topic 12 Abstract")
plotQuote(thoughts12_2, width=25, maxwidth=200, text.cex=1, main="Topic 12 Author Keywords")
plotQuote(thoughts12_3, width=25, maxwidth=200, text.cex=1, main="Topic 12 Keywords Plus")

thoughts13_0 <- findThoughts(chosen_model, texts = model$Document_Title, topics = 13, n = 4)$docs[[1]]
thoughts13_1 <- findThoughts(chosen_model, texts= model$Abstract, topics = 13, n = 3)$docs[[1]]
thoughts13_2 <- findThoughts(chosen_model, texts= model$Author_Keywords, topics = 13, n = 4)$docs[[1]]
thoughts13_3 <- findThoughts(chosen_model, texts= model$Keyword_Plus, topics = 13, n = 4)$docs[[1]]

par(mfrow=c(1,4), mar=c(0.1,0,2,2))
plotQuote(thoughts13_0, width=15, maxwidth=200, text.cex=1, main="Topic 13 Title")
plotQuote(thoughts13_1, width=20, maxwidth=200, text.cex=0.9, main="Topic 13 Abstract")
plotQuote(thoughts13_2, width=25, maxwidth=200, text.cex=1, main="Topic 13 Author Keywords")
plotQuote(thoughts13_3, width=25, maxwidth=200, text.cex=1, main="Topic 13 Keywords Plus")

thoughts14_0 <- findThoughts(chosen_model, texts = model$Document_Title, topics = 14, n = 4)$docs[[1]]
thoughts14_1 <- findThoughts(chosen_model, texts= model$Abstract, topics = 14, n = 3)$docs[[1]]
thoughts14_2 <- findThoughts(chosen_model, texts= model$Author_Keywords, topics = 14, n = 4)$docs[[1]]
thoughts14_3 <- findThoughts(chosen_model, texts= model$Keyword_Plus, topics = 14, n = 4)$docs[[1]]

par(mfrow=c(1,4), mar=c(0.1,0,2,2))
plotQuote(thoughts14_0, width=15, maxwidth=200, text.cex=1, main="Topic 14 Title")
plotQuote(thoughts14_1, width=20, maxwidth=200, text.cex=0.9, main="Topic 14 Abstract")
plotQuote(thoughts14_2, width=25, maxwidth=200, text.cex=1, main="Topic 14 Author Keywords")
plotQuote(thoughts14_3, width=25, maxwidth=200, text.cex=1, main="Topic 14 Keywords Plus")

thoughts15_0 <- findThoughts(chosen_model, texts = model$Document_Title, topics = 15, n = 4)$docs[[1]]
thoughts15_1 <- findThoughts(chosen_model, texts= model$Abstract, topics = 15, n = 3)$docs[[1]]
thoughts15_2 <- findThoughts(chosen_model, texts= model$Author_Keywords, topics = 15, n = 4)$docs[[1]]
thoughts15_3 <- findThoughts(chosen_model, texts= model$Keyword_Plus, topics = 15, n = 4)$docs[[1]]

par(mfrow=c(1,4), mar=c(0.1,0,2,2))
plotQuote(thoughts15_0, width=15, maxwidth=200, text.cex=1, main="Topic 15 Title")
plotQuote(thoughts15_1, width=20, maxwidth=200, text.cex=0.9, main="Topic 15 Abstract")
plotQuote(thoughts15_2, width=25, maxwidth=200, text.cex=1, main="Topic 15 Author Keywords")
plotQuote(thoughts15_3, width=25, maxwidth=200, text.cex=1, main="Topic 15 Keywords Plus")

thoughts16_0 <- findThoughts(chosen_model, texts = model$Document_Title, topics = 16, n = 4)$docs[[1]]
thoughts16_1 <- findThoughts(chosen_model, texts= model$Abstract, topics = 16, n = 3)$docs[[1]]
thoughts16_2 <- findThoughts(chosen_model, texts= model$Author_Keywords, topics = 16, n = 4)$docs[[1]]
thoughts16_3 <- findThoughts(chosen_model, texts= model$Keyword_Plus, topics = 16, n = 4)$docs[[1]]

par(mfrow=c(1,4), mar=c(0.1,0,2,2))
plotQuote(thoughts16_0, width=15, maxwidth=200, text.cex=1, main="Topic 16 Title")
plotQuote(thoughts16_1, width=20, maxwidth=200, text.cex=0.9, main="Topic 16 Abstract")
plotQuote(thoughts16_2, width=25, maxwidth=200, text.cex=1, main="Topic 16 Author Keywords")
plotQuote(thoughts16_3, width=25, maxwidth=200, text.cex=1, main="Topic 16 Keywords Plus")

thoughts17_0 <- findThoughts(chosen_model, texts = model$Document_Title, topics = 17, n = 4)$docs[[1]]
thoughts17_1 <- findThoughts(chosen_model, texts= model$Abstract, topics = 17, n = 3)$docs[[1]]
thoughts17_2 <- findThoughts(chosen_model, texts= model$Author_Keywords, topics = 17, n = 4)$docs[[1]]
thoughts17_3 <- findThoughts(chosen_model, texts= model$Keyword_Plus, topics = 17, n = 4)$docs[[1]]

par(mfrow=c(1,4), mar=c(0.1,0,2,2))
plotQuote(thoughts17_0, width=15, maxwidth=200, text.cex=1, main="Topic 17 Title")
plotQuote(thoughts17_1, width=20, maxwidth=200, text.cex=0.9, main="Topic 17 Abstract")
plotQuote(thoughts17_2, width=25, maxwidth=200, text.cex=1, main="Topic 17 Author Keywords")
plotQuote(thoughts17_3, width=25, maxwidth=200, text.cex=1, main="Topic 17 Keywords Plus")

#compare those whom might seem strange
par(mfrow=c(2,2), mar=c(0,0,2,2))
plot.STM(chosen_model, "perspectives", topics=c(5,9))
plot.STM(chosen_model, "perspectives", topics=c(3,11))
plot.STM(chosen_model, "perspectives", topics=c(5,8))
plot.STM(chosen_model, "perspectives", topics=c(1,10))

#word clouds
par(mfrow=c(1,1))
cloud(chosen_model, topic = 1, max.words = 100)
cloud(chosen_model, topic = 2, max.words = 100)
cloud(chosen_model, topic = 3, max.words = 100)
cloud(chosen_model, topic = 4, max.words = 100, scale = c(3.5,0.07))
cloud(chosen_model, topic = 5, max.words = 100, scale = c(3,0.05))

cloud(chosen_model, topic = 6, max.words = 100)
cloud(chosen_model, topic = 7, max.words = 100)
cloud(chosen_model, topic = 8, max.words = 100)
cloud(chosen_model, topic = 9, max.words = 100)
cloud(chosen_model, topic = 10, max.words = 100)

cloud(chosen_model, topic = 11, max.words = 100)
cloud(chosen_model, topic = 12, max.words = 100, scale = c(3.5,0.07))
cloud(chosen_model, topic = 13, max.words = 100)
cloud(chosen_model, topic = 14, max.words = 100, scale = c(4,0.1))
cloud(chosen_model, topic = 15, max.words = 100, scale = c(4,0.1))

cloud(chosen_model, topic = 16, max.words = 100, scale = c(4,0.1))
cloud(chosen_model, topic = 17, max.words = 100)

#topic correlation
mod.out.corr <- topicCorr(chosen_model)
mod.out.corr
par(mfrow=c(1,1))
plot(mod.out.corr$posadj)

library(igraph)
incidence_mat <- mod.out.corr$posadj
colnames(incidence_mat) <- c("Technology Research Management", "Technology Road Mapping and Selection", "Technology Commercialization",
                             "Organisational Culture and Innovation", "Technology Evaluation", "Information Technology",
                             "Innovation in SMEs", "Internationalisation of Innovation", "New Product Development", 
                             "Technology Education", "Industrial Technology Management", "Energy and Sustainability",
                             "Technology Innovation Management", "Technology Entrepreneurship", "Biomedical Technology",
                             "Technology Adoption", "Knowledge Management")

rownames(incidence_mat) <- c("Technology Research Management", "Technology Road Mapping and Selection", "Technology Commercialization",
                             "Organisational Culture and Innovation", "Technology Evaluation", "Information Technology",
                             "Innovation in SMEs", "Internationalisation of Innovation", "New Product Development", 
                             "Technology Education", "Industrial Technology Management", "Energy and Sustainability",
                             "Technology Innovation Management", "Technology Entrepreneurship", "Biomedical Technology",
                             "Technology Adoption", "Knowledge Management")

incidence_mat
diag(incidence_mat) <- 0

?graph_from_adjacency_matrix
network <- graph_from_adjacency_matrix(incidence_mat, mode = "undirected")

?plot
plot(network, 
     edge.arrow.size=.5,
     vertex.size = 10,
     vertex.label.cex = 0.8,
     vertex.label.color="black", 
     vertex.label.dist=1.5,
     edge.curved=.1)

#Correlation Plots
#install.packages("corrplot")
library(corrplot)
?corrplot

m <- mod.out.corr$cor
colnames(m) <- c("Technology Research Management", "Technology Road Mapping and Selection", "Technology Commercialization",
                             "Organisational Culture and Innovation", "Technology Evaluation", "Information Technology",
                             "Innovation in SMEs", "Internationalisation of Innovation", "New Product Development", 
                             "Technology Education", "Industrial Technology Management", "Energy and Sustainability",
                             "Technology Innovation Management", "Technology Entrepreneurship", "Biomedical Technology",
                             "Technology Adoption", "Knowledge Management")

rownames(m) <- c("Technology Research Management", "Technology Road Mapping and Selection", "Technology Commercialization",
                             "Organisational Culture and Innovation", "Technology Evaluation", "Information Technology",
                             "Innovation in SMEs", "Internationalisation of Innovation", "New Product Development", 
                             "Technology Education", "Industrial Technology Management", "Energy and Sustainability",
                             "Technology Innovation Management", "Technology Entrepreneurship", "Biomedical Technology",
                             "Technology Adoption", "Knowledge Management")
m

corrplot(m, method="color", tl.cex = 0.65)

# Using different color spectrum
col <- colorRampPalette(c("red", "white", "blue"))(20)
corrplot(m, type="upper", order="hclust", col=col, tl.cex = 0.65)

