#set sorking directory
getwd()
path = "/Users/danieungerer/Documents/Meesters/Klasse/MIT807/mit807_power_of_metadata/data/scopus"
setwd(path)
getwd()

#read bibtex format
list.files()
file_name <- "scopus_1980_to_2002.bib" #"scopus_2013_to_2015.bib" #"scopus_2019_to_2021.bib" #"scopus_2016_to_2018.bib"
file_name

M <- convert2df(file_name, dbsource = "scopus", format = "bibtex")


#inspect object
head(M)
typeof(M)
colnames(M)
M$AB[1]
head(M$CR)
