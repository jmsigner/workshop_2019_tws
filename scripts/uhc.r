library(tidyverse)
library(amt)

# Split in test and Training
add_train_test <- function(x, perc_train = 0.7, ignore_case = FALSE) {
  train_test <- function(x, perc_train) {
    n_train <- ceiling(nrow(x) * perc_train)
    idx <- sample.int(nrow(x), n_train)
    x[, "train_test"] <- "test"
    x[idx, "train_test"] <- "train"
    x
  }
  if (ignore_case) {
    train_test(x, perc_train)
  } else {
    dplyr::bind_rows(
      train_test(x[x$case_, ], perc_train), 
      train_test(x[!x$case_, ], perc_train)
    )
  }
  
}

trk <- read_rds("data/trk.rds")
env <- read_rds("data/env_covar.rds")


dat <- filter(trk, id == "M2")


dat <- dat %>% random_points() %>% extract_covariates(env) %>% add_train_test()


class(dat)
dat1 <- dat
class(dat1) <- class(dat)[-1]
dat1 %>% nest(-train_test)


dat %>% nest(train_test)
dat <- dat %>% group_by(train_test) %>% nest()
dat


m1 <- dat %>% random_points() %>% extract_covariates(env) %>% 
  fit_rsf(case_ ~ elevation + pop_den + forest)
summary(m1)
