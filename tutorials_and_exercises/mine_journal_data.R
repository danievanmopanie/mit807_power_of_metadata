#install.packages("bibliometrix")
library(bibliometrix)

library(rscopus)
help(scopus_search)

if (have_api_key()) {
  res = scopus_search(query = "all(gene)", max_count = 20,
                      count = 10)
  df = gen_entries_to_df(res$entries)
  head(df$df)
  sci_res = sciencedirect_search(query = "heart+attack AND text(liver)",
                                 max_count = 30, count = 25)
  sci_df = gen_entries_to_df(sci_res$entries)
  Sys.sleep(2)
  nt = sciencedirect_search(query = "title(neurotoxin)", max_count = 20,
                            count = 10, wait_time = 1)
  nt_df = gen_entries_to_df(nt$entries)
  nt_df = nt_df$df
}

scopus_search(query = "Technology", 
              api_key = "6f27b8d725d77ea49234fc7a7803be74", 
              count = 200,
              view = c("STANDARD", "COMPLETE"), 
              start = 0, 
              verbose = TRUE,
              max_count = 20000,
              http = "http://api.elsevier.com/content/search/scopus?query=heart&apiKey=[api_key]",
              headers = NULL, 
              wait_time = 0)
