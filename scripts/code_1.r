#' ---
#' title: "TWS workshop: Coding Sessing 1"
#' author: "Avgar, Fieberg & Signer"
#' date: "September 2019"
#' output:
#'  html_document:
#'    toc: yes
#' #   toc_float:
#' #       toc_collapsed: true
#' #    toc_depth: 3
#' ---
#' 


#+ include = FALSE
library(knitr)
library(tidyverse)
library(lubridate)
library(raster)
library(amt)
library(here)

opts_chunk$set(fig.width=12,fig.height=4.5, error=TRUE,cache = FALSE)


#' # Introduction to `amt`
#' 

dat <- read_csv(here::here("data/raw/Martes pennanti LaPoint New York.csv"))

#' Check howm many individuals there are
unique(dat$`individual-local-identifier`)

#' and the number of observation for each individual.
table(dat$`individual-local-identifier`)

#' Next, we create an `amt` track

dat1 <- filter(dat, `individual-local-identifier` == "M2") 
dat1 %>% make_track(`location-long`, `location-lat`) 

#' Add a coordinate reference system (CRS)
dat1 %>% make_track(`location-long`, `location-lat`, crs = CRS("+init=epsg:4326")) 

#' Add time
dat2 <- dat1 %>% make_track(`location-long`, `location-lat`, timestamp, crs = CRS("+init=epsg:4326")) 
dat2

#' Remove fixes with missing coordinates (do investigate where the NA's come from)
dat2 <- filter(dat2, !is.na(x_), !is.na(y_))
dat2

#' Transform coordinates to a metric CRS (e.g., NAD83)
dat2 <- transform_coords(dat2, CRS("+init=epsg:5070"))
dat2

#' Work with `tracks`
dat2 %>% step_lengths() %>% head()  # Step lengths
dat2 %>% direction_rel() %>% head()  # Relative direction
dat2 %>% time_of_day()  # Time of day
dat2 %>% bbox()  # Bounding box of all relocations

#' We can also add columns to the `data.frame` with this information
dat3 <- dat2 %>% mutate(sl = step_lengths(.)) %>% time_of_day()

#' Now we may want to plot our the step length distribution for day an night

dat3 %>% ggplot(aes(x = sl)) +  geom_histogram() + facet_wrap(~ tod_)

#' Check the longest steps
dat3 %>% top_n(10, sl) %>% arrange(-sl)

#' Check adjacent relocations

dat3 %>% filter(floor_date(t_, "day") == ymd("2011-02-17")) %>% print(n = Inf)

#' What was the sampling rate?
summarize_sampling_rate(dat2)

#' Resample the data
dat3 <- track_resample(dat2, rate = hours(1), tolerance = minutes(10))
dat3

#' Bursts in `amt`: A `burst_` is a sequence of consecutive relocations with 
#' equal smpling rates

dat3
table(dat3$burst_)
dat3 %>% filter_min_n_burst()

#' From points to steps
dat3a <- dat3 %>% steps()
dat4 <- dat3 %>% steps_by_burst()
dat4[1:12, ]

dat3a[1:12, ]

dat5 <- dat4 %>% time_of_day()

#' Filter for one animal
env <- read_rds(here::here("data/env_covar.rds"))
trk <- read_rds(here::here("data/trk.rds"))
dat1 <- trk %>% filter(id == "M2")
dat2 <- track_resample(dat1, rate = minutes(10), tolerance = minutes(1))

dat2

plot(dat2)
plot(dat2 %>% random_points())

#' Extract covariates at random and observed points
dat3 <- dat2 %>% random_points() %>% extract_covariates(env)
dat3

#' Fit an RSF (this is just wrapper around `glm`)
m1 <- dat3 %>% fit_rsf(case_ ~ elevation + pop_den + forest)
summary(m1)

#' # Fitting Step Selection Functions (SSF)
#' 
env <- read_rds(here::here("data/env_covar.rds"))
trk <- read_rds(here::here("data/trk.rds"))

#' Filter for one animal
dat1 <- trk %>% filter(id == "M2")
dat2 <- track_resample(dat1, rate = minutes(10), tolerance = minutes(1))

dat3 <- dat2 %>% steps_by_burst() %>% random_steps() %>% extract_covariates(env)

#' Fit an SSF (this is just wrapper around `survival::clogit`)
m1 <- dat3 %>% fit_ssf(case_ ~ elevation + pop_den + forest + strata(step_id_))
summary(m1)

#' # Dealing with multiple animals

env <- read_rds(here::here("data/env_covar.rds"))
trk <- read_rds(here::here("data/trk.rds"))

summarize_sampling_rate_many(trk, "id")
dat1 <- trk %>% nest(data = -id)
# This is a temporary fix
class(dat1) <- c("tbl_df", "tbl", "data.frame")
dat1

#' We now repeat the same steps as before. Just that this time we have to loop
#' over each animal. There are several ways how to achieve this. We use the
#' `map` funciton (which is equivalent to the `lappaly` function).
dat2 <- dat1 %>% 
  mutate(dat_resample = map(data, ~ track_resample(., rate = minutes(30), tolerance = minutes(2))))
dat2

#' Now fit for each animal a RSF
#' 
dat2 <- dat2 %>% 
  mutate(rsf = map(dat_resample, ~ .x %>% random_points %>% extract_covariates(env) %>% 
                     fit_rsf(case_ ~ elevation + pop_den + forest)))
dat2 %>% mutate(rsf = map(rsf, ~ broom::tidy(.$model))) %>% 
  unnest(cols = rsf) %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(term, estimate, col = id)) + geom_point()

#' 	
#' ## Session information:	
#' 	
sessioninfo::session_info()
