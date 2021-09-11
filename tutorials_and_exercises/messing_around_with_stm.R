#Messing around with STM, part IIIa: model selection
#https://francescocaberlin.blog/2019/06/26/messing-around-with-stm-part-iiia-model-selection/
#https://github.com/fracab/STMIndeed/blob/master/Indeed%20RJupyterNB2b.ipynb

# download speeches and metadata
setwd("/Users/danieungerer/Documents/Meesters/Klasse/MIT807/mit807_power_of_metadata/tutorials_and_exercises/data")
list.files()

suppressWarnings(library(stm))
suppressWarnings(library(stringr))

DF <- read.table("totaljobsCoordRates.txt", fileEncoding="latin1")
head(DF)

DF<-DF[!is.na(DF$Description),]# stm doesn't work with missing data
DF<-DF[!is.na(DF$rateby),]

DF$Description = str_replace_all(DF$Description, "/"," ")# substitute "/" with a space  
DF$Description = str_replace_all(DF$Description, "&#x27;|&quot;|&#x2F;", "'") ## links and other eventual html encoding (adapted from https://juliasilge.com/blog/evaluating-stm/)
DF$Description = str_replace_all(DF$Description, "<a(.*?)>", " ")             ## 
DF$Description = str_replace_all(DF$Description, "&gt;|&lt;|&amp;", " ")      ##
DF$Description = str_replace_all(DF$Description, "&#[:digit:]+;", " ")        ##
DF$Description = str_remove_all(DF$Description, "<[^>]*>")
head(DF)
table(DF$rateby)
str(DF)
## textProcessor prepares the corpus, representing the documents as lists containting 
#word indices and associated word counts, the vocab character vector associated with 
#the word indices and a metadata matrix containing the covariates. As we can remove 
#some words, here we opt to remove "work" and "will" from the corpus 
processed <- textProcessor(DF$Description, 
                           metadata = DF, 
                           customstopwords=c("work", "will"), 
                           verbose=TRUE)

##PrepDocuments is a helpful function to perform some manipulations like removing words 
#based on thresholds  without compromising the indexes 
out<-prepDocuments(processed$documents, 
                   processed$vocab, 
                   processed$meta, 
                   verbose=TRUE)

docs<-out$documents
vocab<-out$vocab
meta<-out$meta

#identify documents where the word "heartland" has been used
wntword<-which(processed$vocab=="heartland")
filterdocs<-lapply(processed$documents, function(ch) grep(wntword, ch))
indexList<-filterdocs[sapply(filterdocs, function(x) length(x) > 0)]
DF[as.numeric(names(indexList)),2:3]

storage1<-searchK(docs, 
                  vocab, 
                  K = c(5,10,15,20, 50), 
                  prevalence=~ rateby, 
                  data=meta,
                  set.seed(9999), 
                  verbose=TRUE)

print(storage1$results)
options(repr.plot.width=6, repr.plot.height=6)
plot(storage1)


model10Prrateby<-stm(documents=out$documents, 
                     vocab=out$vocab, prevalence =~ rateby, K=10, data=out$meta, init.type = "Spectral", verbose=FALSE)

model15Prrateby<-stm(documents=out$documents, 
                     vocab=out$vocab, prevalence =~ rateby, K=15, data=out$meta, init.type = "Spectral", verbose=FALSE)

model20Prrateby<-stm(documents=out$documents, 
                     vocab=out$vocab, prevalence =~ rateby, K=20, data=out$meta, init.type = "Spectral", verbose=FALSE)


suppressWarnings(library(ggplot2))
suppressWarnings(library(htmlwidgets))

M10ExSem<-as.data.frame(cbind(c(1:10),exclusivity(model10Prrateby), semanticCoherence(model=model10Prrateby, docs), "Mod10"))
M15ExSem<-as.data.frame(cbind(c(1:15),exclusivity(model15Prrateby), semanticCoherence(model=model15Prrateby, docs), "Mod15"))
M20ExSem<-as.data.frame(cbind(c(1:20),exclusivity(model20Prrateby), semanticCoherence(model=model20Prrateby, docs), "Mod20"))

ModsExSem<-rbind(M10ExSem, M15ExSem, M20ExSem)
colnames(ModsExSem)<-c("K","Exclusivity", "SemanticCoherence", "Model")

ModsExSem$Exclusivity<-as.numeric(as.character(ModsExSem$Exclusivity))
ModsExSem$SemanticCoherence<-as.numeric(as.character(ModsExSem$SemanticCoherence))

options(repr.plot.width=7, repr.plot.height=6, repr.plot.res=100)

plotexcoer<-ggplot(ModsExSem, aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_y=.04)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")
plotexcoer

#Model analysis
topicprop15<-make.dt(model15Prrateby, meta)
topicprop15
topicprop15[1:5,c(1:18, 27)]#visualize proportions, job title and rateby

options(repr.plot.width=7, repr.plot.height=7)
plot.STM(model15Prrateby, "hist")

#plot the topic distribution per document, using tidytext and ggplot
suppressWarnings(library(tidytext))# the package sometimes is not loaded correctly. If this happens, you might have to re-start the kernel 
td_theta <- tidytext::tidy(model15Prrateby, matrix = "theta")

selectiontdthteta<-td_theta[td_theta$document%in%c(1:15),]#select the first 30 documents. be careful to select a sensible interval, as attempting to load a very huge corpus might crash the kernel
head(selectiontdthteta, 20)

thetaplot1<-ggplot(selectiontdthteta, aes(y=gamma, x=as.factor(topic), fill = as.factor(topic))) +
  geom_bar(stat="identity",alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ document, ncol = 3) +
  labs(title = "Theta values per document",
       y = expression(theta), x = "Topic")

options(repr.plot.width=8, repr.plot.height=7, repr.plot.res=100)
thetaplot1

# understand more about each topic
suppressWarnings(library(dplyr))
suppressWarnings(library(drlib))#drlib is available on github and needs devtools to be installed

td_beta <- tidytext::tidy(model15Prrateby)

options(repr.plot.width=7, repr.plot.height=8, repr.plot.res=100)

td_beta %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
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
betaT1<-td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%filter(topic=="Topic 1")#beta values for topic 1

betaplotT1<-ggplot(betaT1[betaT1$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 1")#plot word probabilities higher than 0.003 for topic 1

options(repr.plot.width=9, repr.plot.height=10, repr.plot.res=100)
betaplotT1

options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)
plot.STM(model15Prrateby, "summary", n=5)# distribution and top 5 words per topic

#topics 5,6,9 seem to have a degree of overlap
labelTopics(model15Prrateby, topics=c(5,6,9), n=10)# complete list of top 10 words per topics 5-6-9

par(mfrow=c(1,1), mar=c(0,2,0,0))
options(repr.plot.width=5, repr.plot.height=3, repr.plot.res=100)
plot.STM(model15Prrateby, "labels", topics=c(5,6,9), label="frex", n=10, width=55)#top 10 FREX words per topics 5-6-9

#glimpse at highly representative documents per each topic with findThoughts, and plot them with plotQuote
thoughts5 <- findThoughts(model15Prrateby,texts=DF$Description, topics=5, n=3)$docs[[1]]# take  3 representative documents per topic 5
thoughts6 <- findThoughts(model15Prrateby,texts=DF$Description, topics=6, n=3)$docs[[1]]# take  3 representative documents per topic 6
thoughts9 <- findThoughts(model15Prrateby,texts=DF$Description, topics=9, n=3)$docs[[1]]# take  3 representative documents per topic 9

options(repr.plot.width=10, repr.plot.height=12, repr.plot.res=100)
par(mfrow=c(1,4), mar=c(0,0,2,2))

plotQuote(thoughts5, width=30, maxwidth=500, text.cex=1.25, main="Topic 5")
plotQuote(thoughts6, width=30, maxwidth=500, text.cex=1.25, main="Topic 6")
plotQuote(thoughts9, width=30, maxwidth=500, text.cex=1.25, main="Topic 9")

options(repr.plot.width=7, repr.plot.height=6, repr.plot.res=100)
ggplot(ModsExSem[ModsExSem$Model=="Mod15",], aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence for model with 15 topics")

#plot.STM with "perspectives" as argument, wich allows us to have a graphical display of topical contrasts
options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)
par(mfrow=c(2,2), mar=c(0,0,2,2))

plot.STM(model15Prrateby, "perspectives", topics=c(5,9))
plot.STM(model15Prrateby, "perspectives", topics=c(5,6))
plot.STM(model15Prrateby, "perspectives", topics=c(6,9))
dev.off()

out$meta$rateby<-as.factor(out$meta$rateby)
prep<-estimateEffect(1:15~ rateby, model15Prrateby, metadata=out$meta, uncertainty="Global")#nsim is defaulted to 25, but on a small model a higher number lead to more consistent results
summary(prep, topics=c(1:3), nsim=1000)# summary of regression on topic 1-3

options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)
plot.estimateEffect(prep, model=model15Prrateby, covariate="rateby", topics=3, 
                    method="pointestimate", 
                    xlim=c(-.5,1))
