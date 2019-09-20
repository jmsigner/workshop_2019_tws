#' ---
#' title: "Test Vignette for TWS workshop"
#' author: "John Fieberg & Johannes Signer"
#' date: "September 2019"
#' output:
#'  html_document:
#'    toc: yes
#' #   toc_float:
#' #       toc_collapsed: true
#' #    toc_depth: 3
#' ---
#' 

packages_needed <- c("knitr", "lubridate", "maptools", "raster", "move", 
                     "amt",  "tibble", "leaflet", "dplyr", "readr", "ggplot2", 
                     "glmmTMB", "lme4", "tidyr", "purrr", "glue", "sf", 
                     "here", "moveVis", "devtools", "sessioninfo", 
                     "broom", "tictoc", "maps", "rgeos", "tibble", 
                     "maptools", "easypackages", "wrswoR")
new_packages <- packages_needed[!(packages_needed %in% 
                                    installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages, repos = "https://cloud.r-project.org")
}

if (packageVersion("amt") < "0.0.6") {
  install.packages("amt", repos = "https://cloud.r-project.org")
}

#' Load packages:

#+warning=FALSE, message=FALSE
easypackages::libraries("tidyverse", "knitr", "lubridate", 
                        "amt", "dplyr", "here", "ggplot2")
opts_chunk$set(fig.width=12,fig.height=4.5, error=TRUE,cache = FALSE)

#' # Getting started: Loading data and create a track

env <- read_rds(here::here("data/env_covar.rds"))
trk <- read_rds(here::here("data/trk.rds"))

#' Filter for one animal
dat1 <- trk %>% filter(id == "M2")

#' Check sampling rate and resample to 10 minutes
summarize_sampling_rate(dat1)
dat2 <- track_resample(dat1, rate = minutes(10), tolerance = minutes(1))
dat2

#' # Resource Selection Analysis (RSA)
m1 <- dat2 %>% random_points() %>% extract_covariates(env) %>% 
  fit_rsf(case_ ~ elevation + pop_den + forest)
summary(m1)


#' # Step-Selection Analysis (SSA)
m1 <- dat2 %>% steps_by_burst() %>% random_steps() %>%  
  extract_covariates(env) %>% 
  fit_ssf(case_ ~ elevation + pop_den + forest + strata(step_id_))
summary(m1)

#' # Multiple animals
#' 
summarize_sampling_rate_many(trk, "id")
dat1 <- trk %>% nest(data = -id)
dat1
dat2 <- track_resample(dat1, rate = minutes(10), tolerance = minutes(1))
dat2

data(iris)
i2 <- iris %>% nest(data = -Species)

# This is a temporary fix.
class(dat1) <- c("tbl_df", "tbl", "data.frame")
dat1

class(dat1)
trk
n1 <- trk %>% amt::nest(data = c(x_, y_, t_, tod_))
class(n1)

#' 	
#' ## Session information:	
#' 	
sessioninfo::session_info()
proc.time() - ptm
