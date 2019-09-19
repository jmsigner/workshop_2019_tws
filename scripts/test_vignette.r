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
                     "maptools", "easypackages", "RStoolbox")
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

env <- read_rds("data/env_covar.rds")
trk <- read_rds("data/trk.rds")

#' Filter for one animal
dat1 <- trk %>% filter(id == "M2")

#' Check sampling rate and resample to 10 minutes
summarize_sampling_rate(dat1)

#' # Resource Selection Analysis (RSA)
summarize_sampling_rate_many
  





#' # Step-Selection Analysis (SSA)

#' # Multiple animals



#' 	
#' ## Session information:	
#' 	
sessioninfo::session_info()
proc.time() - ptm
