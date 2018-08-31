d <- map(list.files("data", pattern = "\\.rds$", full.names = TRUE), readRDS)
d <- bind_rows(d)

d[!duplicated(d$status_id), ] %>%
  group_by(screen_name) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  print(n = 200)
