ratelimit1 <- function(endpoint = NULL) {
  if (!exists(".rl1", envir = .GlobalEnv)) {
    assign(".rl1", new.env(), envir = .GlobalEnv)
  }
  if (exists("rl1", envir = .rl1)) {
    rl1 <- get("rl1", envir = .rl1)
  } else {
    rl1 <- rate_limit()
    assign("rl1", rl1, envir = .rl1)
  }
  elap <- as.numeric(difftime(Sys.time(), rl1$timestamp[1], units = "mins"))
  if (elap > 2) {
    rl1 <- rate_limit()
    assign("rl1", rl1, envir = .rl1)
  }
  if (!is.null(endpoint)) {
    rl1 <- filter(rl1, grepl(endpoint, query))
  }
  rl1
}

ratelimit2 <- function(endpoint = NULL) {
  if (!exists(".rl2", envir = .GlobalEnv)) {
    assign(".rl2", new.env(), envir = .GlobalEnv)
  }
  if (exists("rl2", envir = .rl2)) {
    rl2 <- get("rl2", envir = .rl2)
  } else {
    rl2 <- rate_limit(bearer_token())
    assign("rl2", rl2, envir = .rl2)
  }
  elap <- as.numeric(difftime(Sys.time(), rl2$timestamp[1], units = "mins"))
  if (elap > 2) {
    rl2 <- rate_limit(bearer_token())
    assign("rl2", rl2, envir = .rl2)
  }
  if (!is.null(endpoint)) {
    rl2 <- filter(rl2, grepl(endpoint, query))
  }
  rl2
}
