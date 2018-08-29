

## Data collection script
library(rtweet)
library(mwk)

## useful rate limit funs
source("R/rate-limits.R")

## read in list of screen names
sns <- readxl::read_xlsx("data/twitter_handles.xlsx",
  col_names = "screen_name", col_types = "text") %>%
  mutate(screen_name = gsub("\\s|@", "", screen_name)) %>%
  unique()

## safe function for getting timelines
safe_timeline <- safely(get_timeline)

## map in chunks
tml1 <- map(sns$screen_name[1:50], safe_timeline, n = 3200)
tml2 <- map(sns$screen_name[51:124], safe_timeline, n = 3200, token = bearer_token())

## merge into single data frame
tml1 <- map(tml1, ~ .x[[1]]) %>% bind_rows()
tml2 <- map(tml2, ~ .x[[1]]) %>% bind_rows()
tml <- bind_rows(tml1, tml2) %>% funique::funique()

## get any tweets they replied to
replies <- tml %>%
  pull(reply_to_status_id) %>%
  na_omit() %>%
  unique() %>%
  lookup_tweets()

## format for 'reply' search
tosns <- paste0("to:", sns$screen_name)

## slice screen names into groups of nine
pos <- c(1:14) %>%
  map(rep, 9) %>%
  unlist() %>%
  .[seq_len(nrow(sns))]

## paste screen name groups into strings (using boolean OR)
g <- map_chr(1:14, ~ paste(tosns[pos == .x], collapse = " OR "))

## search safely
safely_search <- safely(search_tweets)

## search for 3,000 most recent tweets mentioning the journalist handles
tojournos <- map(g, safely_search, include_rts = FALSE, token = bearer_token(), n = 3000)

## merge into single data frame
tojournos <- map(tojournos, ~ .x[[1]]) %>% bind_rows()

## cleanup
rm(tml1, tml2, g, pos, tosns, rl)

## join for complete data set
d <- bind_rows(tml, replies, tojournos)

## save data
save_as <- sprintf("data/journos-%s.rds", gsub("-", "", Sys.Date()))
saveRDS(d, save_as)


d <- readRDS(save_as)

## filter omitted screen names
om <- sns$screen_name[!tolower(sns$screen_name) %in% tolower(d$screen_name)]

nt <- d %>%
  group_by(screen_name) %>%
  summarise(n = n()) %>%
  filter(n < 100 & tolower(screen_name) %in% tolower(sns$screen_name)) %>%
  pull(screen_name) %>%
  tolower()

## get their timelines
otml <- map(om, safe_timeline, n = 3200)
otml <- map(otml, ~ .x[[1]]) %>% bind_rows()

## get any tweets they replied to
oreplies <- otml %>%
  pull(reply_to_status_id) %>%
  na_omit() %>%
  unique() %>%
  lookup_tweets()

## update full data set
d <- bind_rows(d, otml, oreplies)

## only unique rows
d <- funique::funique(d)

## resave
saveRDS(d, save_as)
