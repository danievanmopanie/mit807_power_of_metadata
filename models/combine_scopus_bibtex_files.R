#install.packages("bibliometrix")
library(bibliometrix)

#set sorking directory
getwd()
path = "/Users/danieungerer/Documents/Meesters/Klasse/MIT807/mit807_power_of_metadata/data/scopus"
setwd(path)
getwd()

#read bibtex format
list.files()
path_to_bib_files <- list.files(".", pattern="\\.bib$", full.names=TRUE)
path_to_bib_files

combined_bib <- ""
for (path_to_bib_file in path_to_bib_files) {
  
  fileCon <- file(path_to_bib_file)
  content <- readLines(fileCon)
  close(fileCon)
  
  combined_bib <- paste0(combined_bib, "\n", "\n", trimws(paste0(content, collapse="\n")))
  
} 

list.files()

cat(combined_bib, file="combined_references.bib", "\n")