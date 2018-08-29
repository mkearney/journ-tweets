## load pkgs
library(rtweet)
library(mwk)

## read in list of screen names
sns <- readxl::read_xlsx("data/twitter_handles.xlsx",
  col_names = "screen_name", col_types = "text") %>%
  mutate(screen_name = gsub("\\s|@", "", screen_name)) %>%
  unique()

## read in most recent date
since_date <- gsub(".*-|\\.rds", "", list.files("data", pattern = "\\.rds$"))
since_date <- since_date[order(as.integer(since_date), decreasing = TRUE)[1]]
since_date <- as.Date(since_date, format = "%Y%m%d") - 1

## format for 'from' search
fromsns <- paste0("from:", sns$screen_name)

## slice screen names into groups of nine
pos <- seq_along(sns$screen_name) %>%
  map(rep, 9) %>%
  unlist() %>%
  .[seq_len(nrow(sns))]

## paste screen name groups into strings (using boolean OR)
g <- map_chr(seq_len(ceiling(nrow(sns) / 9)), ~ paste(fromsns[pos == .x], collapse = " OR "))
g <- paste0(g, " since:", since_date)

## search safely
safely_search <- safely(search_tweets)

## search for up to 5000 tweets form each
s <- map(g, safely_search, n = 3000, token = bearer_token())
s <- map(s, ~ .x[[1]]) %>% bind_rows()

## save
save_as <- sprintf("data/from-journos-%s.rds", gsub("-", "", Sys.Date()))
saveRDS(s, save_as)
