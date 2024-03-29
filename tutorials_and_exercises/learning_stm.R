#Website:   https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf
#Github:    https://github.com/dondealban/learning-stm/blob/master/stm-test-workflow.R

#STM Simple Example

# This script implements a test workflow of a structural topic model using the stm R
# package (Roberts et al. 2016). The script incorporates some minor modifications in 
# the original script by Nicholas B. Adams. The link to the source script is found at:  
# https://github.com/nickbadams/D-Lab_TextAnalysisWorkingGroup/tree/master/STM_prep.
# 
# Script modified by: Jose Don T. De Alban
# Date modified:      09 Sept 2017


# ----------------------------------------
# LOAD LIBRARIES
# ----------------------------------------

#install.packages("stm")
library(stm)        # Package for sturctural topic modeling
#install.packages("igraph")
library(igraph)     # Package for network analysis and visualisation
#install.packages("stmCorrViz")
library(stmCorrViz) # Package for hierarchical correlation view of STMs

# ----------------------------------------
# LOAD DATA
# ----------------------------------------

getwd()
#setwd("/cloud/project/tutorials_and_exercises/data") #for cloud IDE
setwd("/Users/danieungerer/Documents/Meesters/Klasse/MIT807/mit807_power_of_metadata/tutorials_and_exercises") #for local IDE

list.files()

setwd("/Users/danieungerer/Documents/Meesters/Klasse/MIT807/mit807_power_of_metadata/tutorials_and_exercises/data")
list.files()
data <- read.csv("poliblogs2008.csv") 
load("VignetteObjects.RData") 

head(data,1)
colnames(data)
dim(data)

# ----------------------------------------
# PREPARE AND PRE-PROCESS DATA
# ----------------------------------------

# Stemming, stopword removal, etc. using textProcessor() function
?textProcessor
processed <- textProcessor(data$documents, 
                           metadata=data)
head(processed$meta)
typeof(processed)

# Structure and index for usage in the STM model. Ensure that object has no missing
# values. Remove low frequency words using 'lower.thresh' option. See ?prepDocuments 
# for more information.
?prepDocuments
out <- prepDocuments(processed$documents, 
                     processed$vocab, 
                     processed$meta)

# The output will have object meta, documents, and vocab 
docs <- out$documents
head(docs)
vocab <- out$vocab
head(vocab,30)
tail(vocab,30)
meta <-out$meta
head(meta)

# Take a look at how many words and documents would be removed using different 
# lower.thresholds. Save plot as pdf.
#pdf("stm-plot-removed.pdf", width=10, height=8.5)
plotRemoved(processed$documents, lower.thresh=seq(1,200, by=100))
#dev.off()

# ----------------------------------------
# ESTIMATE THE STRUCTURAL TOPIC MODEL
# ----------------------------------------

# ESTIMATION WITH THE TOPIC PREVALENCE PARAMETER.
# Run an STM model using the 'out' data with 20 topics. Ask how prevalence of topics 
# varies across documents' meta data, including 'rating' and 'day'. The option 's(day)' 
# applies a spline normalization to 'day' variable. The authors specified the maximum
# number of expectation-maximization iterations = 75, and the seed they are using for 
# reproducibility.

poliblogPrevFit <- stm(out$documents, 
                       out$vocab, 
                       K = 20, 
                       prevalence =~rating+s(day), 
                       max.em.its=150, 
                       data=out$meta, 
                       init.type="Spectral", 
                       seed=8458159)

poliblogPrevFit$time

# Plot the STM using different types. See the proportion of each topic in the entire
# corpus. Save as pdf files.

par("mar")
par(mar = c(1,1,1,1))
#pdf("stm-plot-prevfit.pdf", width=10, height=8.5)
plot(poliblogPrevFit)
#dev.off()
#pdf("stm-plot-prevfit-summary.pdf", width=10, height=8.5)
plot(poliblogPrevFit, type="summary", xlim=c(0,.4))
#dev.off()
#pdf("stm-plot-prevfit-labels.pdf", width=10, height=8.5)
plot(poliblogPrevFit, type="labels", topics=c(3,7,20))
#dev.off()
#pdf("stm-plot-prevfit-histogram.pdf", width=14, height=12.5)
plot(poliblogPrevFit, type="hist")
#dev.off()
#pdf("stm-plot-prevfit-perspectives-two-topic.pdf", width=14, height=12.5)
plot(poliblogPrevFit, type="perspectives", topics=c(7,10))
#dev.off()

plot(poliblogPrevFit, type="labels", topics=c(3,7,20))
# ----------------------------------------
# EVALUATE MODELS
# ----------------------------------------

# SEARCH AND SELECT MODEL FOR A FIXED NUMBER OF TOPICS.
# The function selectModel() assists the user in finding and selecting a model with
# desirable properties in both semantic coherence and exclusivity dimensions (e.g.,
# models with average scores towards the upper right side of the plot). STM will
# compare a number of models side by side and will keep the models that do not 
# converge quickly. 
poliblogSelect <- selectModel(out$documents, 
                              out$vocab, 
                              K=20, 
                              prevalence=~rating+s(day),
                              max.em.its=150, 
                              data=meta, 
                              runs=20, 
                              seed=8458159)

# Plot the different models that make the cut along exclusivity and semantic coherence
# of their topics. Save plot as pdf file.
#pdf("stm-plot-selected.pdf", width=10, height=8.5)
par(mar = c(2,2,1,1))
plotModels(poliblogSelect)
#dev.off()

# Each STM has semantic coherence and exclusivity values associated with each topic. 
# The topicQuality() function plots these values and labels each with its topic number.
# Save plot as pdf file.
#pdf("stm-plot-topic-quality.pdf", width=10, height=8.5)
topicQuality(model=poliblogPrevFit, documents=docs)
dev.off()

# Select one of the models to work with based on the best semantic coherence and 
# exclusivity values (upper right corner of plot).
selectedModel3 <- poliblogSelect$runout[[3]] # Choose model #3

# Another option is the manyTopics() function that performs model selection across
# separate STMs that each assume different number of topics. It works the same as 
# selectModel(), except user specifies a range of numbers of topics that they want 
# the model fitted for. For example, models with 5, 10, and 15 topics. Then, for 
# each number of topics, selectModel() is run multiple times. The output is then 
# processed through a function that takes a pareto dominant run of the model in 
# terms of exclusivity and semantic coherence. If multiple runs are candidates 
# (i.e., none weakly dominates the others), a single model run is randomly chosen 
# from the set of undominated runs. Save plots as pdf files.
storage <- manyTopics(out$documents, 
                      out$vocab, 
                      K=c(7:10), 
                      prevalence=~rating+s(day),
                      data=meta, 
                      runs=10)

storageOutput1 <- storage$out[[1]] # 7 topics
pdf("stm-plot-storage-output1.pdf", width=10, height=8.5)
plot(storageOutput1)
dev.off()
storageOutput2 <- storage$out[[2]] # 8 topics
pdf("stm-plot-storage-output2.pdf", width=10, height=8.5)
plot(storageOutput2)
dev.off()
storageOutput3 <- storage$out[[3]] # 9 topics
pdf("stm-plot-storage-output3.pdf", width=10, height=8.5)
plot(storageOutput3)
dev.off()
storageOutput4 <- storage$out[[4]] # 10 topics
pdf("stm-plot-storage-output4.pdf", width=10, height=8.5)
plot(storageOutput4)
dev.off()

# ALTERNATIVE: MODEL SEARCH ACROSS A NUMBER OF TOPICS.
# Let R figure out the best model for you defined by exclusivity and semantic 
# coherence for each K (i.e. # of topics). The searchK() uses a data-driven
# approach to selecting the number of topics. Save plot as pdf file.
kResult <- searchK(out$documents, out$vocab, K=c(7,10), prevalence=~rating+s(day),
                   data=meta)
pdf("stm-plot-searchk.pdf", width=10, height=8.5)
plot(kResult)
dev.off()

# ----------------------------------------
# INTERPRET STMs BY PLOTTING RESULTS
# ----------------------------------------

# According to the package vignette, there are a number of ways to interpret the model
# results. These include:
# 1. Displaying words associated with topics: labelTopics(), sageLabels()
# 2. Displaying documents highly associated with particular topics: findThoughts()
# 3. Estimating relationships between metadata and topics: estimateEffect()
# 4. Estimating topic correlations: topicCorr()

# labelTopics().
# Label topics by listing top words for selected topics 3, 7, 20. Save as txt file.
labelTopicsSel <- labelTopics(poliblogPrevFit, c(3,7,20))
sink("stm-list-label-topics-selected.txt", append=FALSE, split=TRUE)
print(labelTopicsSel)
sink()
# Label topics by listing top words for all topics. Save as txt file.
labelTopicsAll <- labelTopics(poliblogPrevFit, c(1:20))
sink("stm-list-label-topics-all.txt", append=FALSE, split=TRUE)
print(labelTopicsAll)
sink()

# sageLabels().
# This can be used as a more detailed alternative to labelTopics(). The function displays
# verbose labels that describe topics and topic-covariate groups in depth.
sink("stm-list-sagelabel.txt", append=FALSE, split=TRUE)
print(sageLabels(poliblogPrevFit))
sink()

# findThoughts().
# Read documents that are highly correlated with the user-specified topics using the 
# findThoughts() function. Object 'thoughts1' contains 3 documents about topic 1 and
# 'texts=shortdoc' gives just the first 250 words. Additional examples are done for
# topics 3,7,10, and 20.
thoughts1 <- findThoughts(poliblogPrevFit, texts=shortdoc, n=3, topics=1)$docs[[1]]
pdf("stm-plot-find-thoughts1.pdf", width=10, height=8.5)
plotQuote(thoughts1, width=40, main="Topic 1")
dev.off()
thoughts3 <- findThoughts(poliblogPrevFit, texts=shortdoc, n=3, topics=3)$docs[[1]]
pdf("stm-plot-find-thoughts3.pdf", width=10, height=8.5)
plotQuote(thoughts3, width=40, main="Topic 3")
dev.off()
thoughts7 <- findThoughts(poliblogPrevFit, texts=shortdoc, n=3, topics=7)$docs[[1]]
pdf("stm-plot-find-thoughts7.pdf", width=10, height=8.5)
plotQuote(thoughts7, width=40, main="Topic 7")
dev.off()
thoughts10 <- findThoughts(poliblogPrevFit, texts=shortdoc, n=3, topics=10)$docs[[1]]
pdf("stm-plot-find-thoughts10.pdf", width=10, height=8.5)
plotQuote(thoughts10, width=40, main="Topic 10")
dev.off()
thoughts20 <- findThoughts(poliblogPrevFit, texts=shortdoc, n=3, topics=20)$docs[[1]]
pdf("stm-plot-find-thoughts20.pdf", width=10, height=8.5)
plotQuote(thoughts20, width=40, main="Topic 20")
dev.off()

# estimateEffect().
# Explore how prevalence of topics varies across documents according to document
# covariates (metadata). First, users must specify the variable that they wish to use 
# for calculating an effect. If there are multiple variables specified in 
# estimateEffect(), then all other variables are held at their sample median. These 
# parameters include the expected proportion of a document that belongs to a topic as
# a function of a covariate, or a first difference type estimate, where topic prevalence
# for a particular topic is contrasted for two groups (e.g., liberal versus conservative).
out$meta$rating <- as.factor(out$meta$rating)
prep <- estimateEffect(1:20 ~ rating+s(day), poliblogPrevFit, meta=out$meta, 
                       uncertainty="Global")

# See how prevalence of topics differs across values of a categorical covariate
pdf("stm-plot-estimate-effect-categorical.pdf", width=10, height=8.5)
plot(prep, covariate="rating", topics=c(3, 7, 20), model=poliblogPrevFit, 
     method="difference", cov.value1="Liberal", cov.value2="Conservative",
     xlab="More Conservative ... More Liberal", main="Effect of Liberal vs. Conservative",
     xlim=c(-.15,.15), labeltype ="custom", custom.labels=c('Obama', 'Sarah Palin', 
                                                            'Bush Presidency'))
dev.off()

# See how prevalence of topics differs across values of a continuous covariate
pdf("stm-plot-estimate-effect-continuous.pdf", width=10, height=8.5)
plot(prep, "day", method="continuous", topics=20, model=z, printlegend=FALSE, xaxt="n", 
     xlab="Time (2008)")
monthseq <- seq(from=as.Date("2008-01-01"), to=as.Date("2008-12-01"), by="month")
monthnames <- months(monthseq)
axis(1, at=as.numeric(monthseq)-min(as.numeric(monthseq)), labels=monthnames)
dev.off()

# topicCorr().
# STM permits correlations between topics. Positive correlations between topics indicate
# that both topics are likely to be discussed within a document. A graphical network
# display shows how closely related topics are to one another (i.e., how likely they are
# to appear in the same document). This function requires 'igraph' package.
mod.out.corr <- topicCorr(poliblogPrevFit)
pdf("stm-plot-topic-correlations.pdf", width=10, height=8.5)
plot(mod.out.corr)
dev.off()

# ----------------------------------------
# VISUALISE &  PRESENT STMs RESULTS
# ----------------------------------------

# TOPICAL CONTENT.
# STM can plot the influence of covariates included in as a topical content covariate.
# A topical content variable allows for the vocabulary used to talk about a particular 
# topic to vary. First, the STM must be fit with a variable specified in the content 
# option.
poliblogContent <- stm(out$documents, out$vocab, K=20, prevalence=~rating+s(day), 
                       content=~rating, max.em.its=75, data=out$meta, 
                       init.type="Spectral", seed=8458159)
pdf("stm-plot-content-perspectives.pdf", width=10, height=8.5)
plot(poliblogContent, type="perspectives", topics=7)
dev.off()

# WORD CLOUD.
pdf("stm-plot-prevfit-wordcloud.pdf", width=10, height=8.5)
cloud(poliblogPrevFit, topic=7)
dev.off()
pdf("stm-plot-content-wordcloud.pdf", width=10, height=8.5)
cloud(poliblogContent, topic=7)
dev.off()

# ----------------------------------------
# PLOT COVARIATE INTERACTIONS
# ----------------------------------------

# Interactions between covariates can be examined such that one variable may “moderate”
# the effect of another variable.
poliblogInteraction <- stm(out$documents, out$vocab, K=20, prevalence=~rating*day, 
                           max.em.its=75, data=out$meta, seed=8458159)

# Prep covariates using the estimateEffect() function, only this time, we include the 
# interaction variable. Plot the variables and save as pdf files.
prep2 <- estimateEffect(c(20) ~ rating*day, poliblogInteraction, metadata=out$meta, 
                        uncertainty="None")
pdf("stm-plot-interact-estimate-effect.pdf", width=10, height=8.5)
plot(prep2, covariate="day", model=poliblogInteraction, method="continuous", xlab="Days",
     moderator="rating", moderator.value="Liberal", linecol="blue", ylim=c(0,0.12), 
     printlegend=F)
plot(prep2, covariate="day", model=poliblogInteraction, method="continuous", xlab="Days",
     moderator="rating", moderator.value="Conservative", linecol="red", add=T,
     printlegend=F)
legend(0,0.12, c("Liberal", "Conservative"), lwd=2, col=c("blue", "red"))
dev.off()

# ----------------------------------------
# PLOT CONVERGENCE
# ----------------------------------------

pdf("stm-plot-prevfit-convergence.pdf", width=10, height=8.5)
plot(poliblogPrevFit$convergence$bound, type="l", ylab="Approximate Objective", 
     main="Convergence")
dev.off()

# ----------------------------------------
# INTERACTIVE VISUALISATION OF STM
# ----------------------------------------

# The stmCorrViz() function generates an interactive visualisation of topic hierarchy/
# correlations in a structural topicl model. The package performs a hierarchical
# clustering of topics that are then exported to a JSON object and visualised using D3.

stmCorrViz(poliblogPrevFit, "stm-interactive-correlation.html", 
           documents_raw=data$documents, documents_matrix=out$documents)
