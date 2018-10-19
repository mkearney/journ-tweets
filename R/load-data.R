## load pkgs
library(mwk)

## read data fles
d <- map(list.files("data", pattern = "\\.rds$", full.names = TRUE), readRDS)

## merge into single data frame
d <- bind_rows(d)

## remove duplcates
d <- d[!duplicated(d$status_id), ]

## count by screen name
d %>%
  group_by(screen_name) %>%
  summarise(n = n(), journo_replies = sum(!is.na(reply_to_status_id))) %>%
  arrange(desc(n)) %>%
  print(n = 200)

## read in list of screen names
journ_ids <- readxl::read_xlsx("data/twitter_handles.xlsx",
  col_names = "screen_name", col_types = "text") %>%
  mutate(screen_name = gsub("\\s|@", "", screen_name)) %>%
  unique()

## match to data and join to get user_id
journ_ids$user_id <- d$user_id[
  match(tolower(journ_ids$screen_name), tolower(d$screen_name))]
## override screen name to get correct/updated capitalization
journ_ids$screen_name <- d$screen_name[
  match(tolower(journ_ids$screen_name), tolower(d$screen_name))]

is_non_to_journalist <- function(user_id, reply_to_status_id, reply_to_user_id) {
  ## is a reply
  !is.na(reply_to_status_id) &
    ## is a non-journalist
    !user_id %in% journ_ids$user_id &
      ## replies to a journalist
      reply_to_user_id %in% journ_ids$user_id
}
is_journalist_to_non <- function(user_id, reply_to_status_id, reply_to_user_id) {
  ## is a reply
  !is.na(reply_to_status_id) &
    ## is a journalist
    user_id %in% journ_ids$user_id &
    ## replies to a non
    !reply_to_user_id %in% journ_ids$user_id
}

## number of each kind
d %>%
  mutate(is_journo = tolower(screen_name) %in% tolower(journ_ids$screen_name),
    is_reply = !is.na(reply_to_status_id),
    non_to_journalist = is_non_to_journalist(user_id, reply_to_status_id, reply_to_user_id),
    journalist_to_non = is_journalist_to_non(user_id, reply_to_status_id, reply_to_user_id)) %>%
  select(non_to_journalist:journalist_to_non) %>%
  table()

## randomly sample original posts
random_sample <- function(x, n) {
  x <- filter(x, created_at > "2018-07-31")
  ## filter statuses that have replies but that aren't replies
  s <- x$status_id[x$status_id %in% na_omit(x$reply_to_status_id) & !is.na(x$reply_to_status_id)]
  ## randomly sample those original posts
  s <- sample(s, n)

  ## filter replies to sampled statuses
  r <- x$reply_to_status_id[x$status_id %in% s & !is.na(x$reply_to_status_id)]

  ## filter replies to replies of sampled statuses
  r <- c(r, x$reply_to_status_id[x$reply_to_status_id %in% r & !is.na(x$reply_to_status_id)])

  ## filter original data with originals, replies, and replies to replies
  x <- filter(x, (status_id %in% s | (!is.na(reply_to_status_id) & reply_to_status_id %in% s)) |
    (status_id %in% r | (!is.na(reply_to_status_id) & reply_to_status_id %in% r)))

  ## output
  x %>% mutate(
    og = status_id %in% s,
    reply = (!is.na(reply_to_status_id) & reply_to_status_id %in% s),
    reference_tweet = ifelse(og, status_id, reply_to_status_id)
  ) %>%
    select(-og, reply) %>%
    arrange(reference_tweet, status_id, reply_to_status_id)
}

## sample size
n <- 200
d_ <- d
d <- filter(d, created_at > "2018-07-31 24:00:00")


#na_omit(d$reply_to_status_id)[na_omit(d$reply_to_status_id) %in% d$status_id]

og <- sample(d$status_id[tolower(d$screen_name) %in% tolower(journ_ids$screen_name) &
    is.na(d$reply_to_status_id) & d$status_id %in% na_omit(d$reply_to_status_id)], n)
(r1 <- d$status_id[d$reply_to_status_id %in% og])
(r2 <- d$status_id[d$reply_to_status_id %in% r1])
(r3 <- d$status_id[d$reply_to_status_id %in% r2])


rs <- filter(d, status_id %in% c(og, r1, r2, r3)) %>%
  mutate(order = match(status_id, c(og, r1, r2, r3))) %>%
  arrange(order) %>%
  select(status_id, reply_to_status_id, order) %>%
  mutate(tweet = case_when(
    status_id %in% og ~ "original",
    status_id %in% r1 ~ "to_original",
    status_id %in% r2 ~ "to_reply",
    status_id %in% r3 ~ "to_replyreply"
  )) %>%
  select(-order) %>%
  tidyr::spread(tweet, reply_to_status_id) %>%
  select(-original) %>%
  mutate(original = ifelse(is.na(to_original) & is.na(to_reply) &
      is.na(to_replyreply), status_id, NA_character_)) %>%
  select(original, to_original, to_reply, to_replyreply, status_id) %>%
  mutate(original = ifelse(is.na(original), "", "➡"),
    to_original = ifelse(is.na(to_original), "", " ↩"),
    to_reply = ifelse(is.na(to_reply), "", "  ↩↩"),
    to_replyreply = ifelse(is.na(to_replyreply), "", "   ↩↩↩"),
    reply_tree = case_when(
      nchar(original) == 1 ~ original,
      nchar(to_original) == 2 ~ to_original,
      nchar(to_reply) == 4 ~ to_reply,
      nchar(to_replyreply) == 6 ~ to_replyreply,
      TRUE ~ ""
  ),
    row = seq_along(reply_tree)) %>%
  select(row, reply_tree, status_id) %>%
  left_join(d, by = "status_id") %>%
  mutate(id_tree = case_when(
    nchar(reply_tree) == 1 ~ status_id,
    nchar(reply_tree) == 2 ~ paste0(reply_to_status_id, "-", status_id),
    nchar(reply_tree) == 4 ~ paste0(c("",
      reply_to_status_id[-length(status_id)]), "-",
      reply_to_status_id, "-", status_id),
    nchar(reply_tree) == 6 ~ paste0(c("", "",
      reply_to_status_id[-c((length(status_id)-1),
        length(status_id))]), "-", c("",
          reply_to_status_id[-length(status_id)]), "-",
      reply_to_status_id, "-", status_id)
  ),
    id_tree = gsub("\\b(?=\\d)", "x", id_tree, perl = TRUE)) %>%
  select(row, reply_tree, id_tree, status_id) %>%
  left_join(d)

st <- min(d$created_at)
en <- max(d$created_at)
st <- strftime(st, "%b %d")
en <- strftime(en, "%b %d")
n <- prettyNum(nrow(d), big.mark = ",")

rtweet::ts_plot(d, "days", color = 'transparent') +
  theme_mwk() +
  geom_point() +
  geom_smooth(span = .5) +
  labs(title = glue::glue("Frequency of observed (population) tweets from {st} to {en}"),
    subtitle = glue::glue("Daily counts of observed twitter statuses (n = {n}) in journalist-interactions data set"),
    x = NULL, y = NULL) +
  ylim(0, NA) +
  ggsave("~/Desktop/pop-ts.png")




st <- min(d_$created_at)
en <- max(d_$created_at)
st <- strftime(st, "%b %d")
en <- strftime(en, "%b %d")
n <- prettyNum(nrow(d_), big.mark = ",")

rtweet::ts_plot(d_, "months", color = 'transparent') +
  theme_mwk() +
  geom_point() +
  geom_smooth(span = .5) +
  labs(title = glue::glue("Frequency of observed (population) tweets from {st} to {en}"),
    subtitle = glue::glue("Monthly counts of observed twitter statuses (n = {n}) in journalist-interactions data set"),
    x = NULL, y = NULL) +
  ggsave("~/Desktop/full-ts.png")



st <- min(rs$created_at)
en <- max(rs$created_at)
st <- strftime(st, "%b %d")
en <- strftime(en, "%b %d")
n <- prettyNum(nrow(rs), big.mark = ",")


rtweet::ts_plot(rs, "days", color = 'transparent') +
  theme_mwk(base_size = 18) +
  geom_point() +
  geom_smooth(span = .80) +
  labs(title = glue::glue("Frequency of observed (sample) tweets from {st} to {en}"),
    subtitle = glue::glue("Daily counts of twitter statuses in random sample (n = {n}) of journalist-interactions data set"),
    x = NULL, y = NULL) +
  ggsave("~/Desktop/sample-ts.png")

rtweet::save_as_csv(rs, "~/Dropbox/ryan-thomas-data2.csv")


s <- random_sample(d, 1)
mutate(s, id = status_id,
  status = ifelse(is.na(reply_to_status_id), status_id, ""),
  reply = ifelse(is.na(reply_to_status_id), "", reply_to_status_id),
  reply2 = ifelse(reply != "" & reply %in% status[1], status_id, "")) %>%
  select(status, reply, reply2)


random_sample(d, 1) %>%
  mutate(original_status = ifelse(status_id == reference_tweet, status_id, NA_character_),
    reply_to_status_id = ifelse((reply_to_status_id %in% c(na_omit(status_id))) |
        (reply_to_status_id != reply_to_status_id &
            reply_to_status_id %in% reply_to_status_id), reply_to_status_id, NA_character_),
    status_id = ifelse(is.na(original_status) &
        is.na(reply_to_status_id), status_id, NA_character_)) %>%
  mutate(original_status = ifelse(
    is.na(original_status), "", paste0("x", original_status)),
    reply_to_status_id = ifelse(
      is.na(reply_to_status_id), "<-", paste0("x", reply_to_status_id))) %>%
  select(original_status, reply_to_status_id, status_id) %>%
  mutate(to_reply = original_status != "" & nchar(reply_to_status_id) > 5,
    to_reply = ifelse(to_reply, original_status, ""),
    original_status = ifelse(to_reply != "", "", original_status)) %>%
  #arrange(is.na(status_id)) %>%
  print(n = 100)

rtweet:::save_as_csv(d[1:50, ], "~/Desktop/ryan-thomas-data.csv")

rtweet::ts_plot(d, "weeks")


filter(d, is.na(reply_to_status_id)) %>%
  pull(created_at) %>%
  range()


filter(d_, !screen_name %in% journ_ids$screen_name, !is.na(reply_to_status_id), reply_to_screen_name %in% journ_ids$screen_name) %>%
  pull(reply_to_status_id) -> ids

filter(d_, status_id %in% ids) %>% pull(is_retweet) %>% table()

filter(d_, screen_name %in% journ_ids$screen_name, !is_retweet) %>% pull(reply_to_status_id) %>% is.na() %>% table()
filter(d_, !screen_name %in% journ_ids$screen_name) %>% pull(reply_to_status_id) %>% is.na() %>% table()
