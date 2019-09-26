#' ---
#' title: "TWS workshop: Coding Session 2"
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

#' # Dealing with multiple animals

env <- read_rds(here::here("data/env_covar.rds"))
trk <- read_rds(here::here("data/trk.rds"))

summarize_sampling_rate_many(trk, "id")
dat1 <- trk %>% nest(data = -id)
# This is a temporary fix
class(dat1) <- c("tbl_df", "tbl", "data.frame")
dat1

#' Add a column with the animal's sex
dat1 <- dat1 %>% mutate(sex = str_sub(id, 1, 1))

#' ## RSF
#' 
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
dat2 <- dat2 %>% mutate(rsf = map(rsf, ~ broom::tidy(.$model))) %>% 
  unnest(cols = rsf) %>% 
  filter(term != "(Intercept)") # remove estimate for the intercept


#' Plotting results

ggplot(dat2, aes(term, estimate, col = id)) + geom_point()

#' better color by sex
ggplot(dat2, aes(term, estimate, col = sex)) + geom_point()
ggplot(dat2, aes(sex, estimate)) + geom_point() + 
  facet_wrap(~ term, scale = "free", ncol = 2)

#' Do statistics with coeffficients

#' Bootstrap point and uncertainty estimate

#' Lets do one coefficient: elevation
dat2 %>% filter(term == "elevation") %>% pull(estimate)
pop_ele <- dat2 %>% filter(term == "elevation") %>% pull(estimate)

sample(pop_ele, 8, TRUE)
mean(sample(pop_ele, 8, TRUE))
b <- replicate(1e3, mean(sample(pop_ele, 8, TRUE)))
hist(b)

#' Now we can calcualte statistics from the bootstrap sample
mean(b)
sd(b)
quantile(b, prob = c(0.025, 0.975))

# Now for all three terms
res0 <- dat2 %>% select(id, term, estimate) %>% 
  nest(data = estimate) %>% 
  mutate(boot = map(data, ~ replicate(1e3, mean(sample(.$estimate, 8, replace = TRUE)))), 
         mean = map_dbl(boot, mean), 
         lci = map_dbl(boot, quantile, prob = 0.025), 
         uci = map_dbl(boot, quantile, prob = 0.975))

# Using this approach, we can easily add a second grouping variable (e.g., sex)
res1 <- dat2 %>% select(sex, term, estimate) %>% 
  nest(data = estimate) %>% 
  mutate(boot = map(data, ~ replicate(1e3, mean(sample(.$estimate, 8, replace = TRUE)))), 
         mean = map_dbl(boot, mean), 
         lci = map_dbl(boot, quantile, prob = 0.025), 
         uci = map_dbl(boot, quantile, prob = 0.975))
  
ggplot(res1, aes(sex, mean, ymin = lci, ymax = uci)) + geom_pointrange() +
  facet_wrap(~ term, scale = "free", ncol = 2)

# TODO: Plot again availability

#' ## Mixed effects model
#' 

library(glmmTMB)
mutate(dat_resample = map(data, ~ track_resample(., rate = minutes(30), tolerance = minutes(2))))

dat3 <- dat2 %>% 
  mutate(rsf = map(dat_resample, ~ .x %>% random_points %>% extract_covariates(env))) %>% 
  select(id, rsf) %>% unnest(cols = rsf)

# This will take a few minutes again
m1 <- glmmTMB(case_ ~ elevation + forest + pop_den + (1 + elevation + forest + pop_den | id), 
             data = dat3, family = binomial())

#' Compare the individual models and the fixed effects model
fixef(m1)[[1]][-1]
res0

summary(m1)


#' # Step-Selection Functions

env <- read_rds(here::here("data/env_covar.rds"))
trk <- read_rds(here::here("data/trk.rds"))

summarize_sampling_rate_many(trk, "id")
dat1 <- trk %>% nest(data = -id)
# This is a temporary fix
class(dat1) <- c("tbl_df", "tbl", "data.frame")
dat1

#' Add a column with the animal's sex
dat1 <- dat1 %>% mutate(sex = str_sub(id, 1, 1))

dat2 <- dat1 %>% 
  mutate(dat_resample = map(data, ~ track_resample(., rate = minutes(30), tolerance = minutes(2))))
dat2

#' Now fit for each animal a SSF
#' 
names(env)
dat2 <- dat2 %>% 
  mutate(ssf = map(dat_resample, ~ .x %>% steps_by_burst %>% 
                     random_steps %>% extract_covariates(env) %>% 
                     fit_ssf(case_ ~ elevation + pop_den + forest + strata(step_id_))))
dat2 <- dat2 %>% mutate(ssf = map(ssf, ~ broom::tidy(.$model))) %>% 
  unnest(cols = ssf) 

# Bootstrap coefficients 

res1 <- dat2 %>% select(term, estimate) %>% 
  nest(data = estimate) %>% 
  mutate(boot = map(data, ~ replicate(1e3, mean(sample(.$estimate, 8, replace = TRUE)))), 
         mean = map_dbl(boot, mean), 
         lci = map_dbl(boot, quantile, prob = 0.025), 
         uci = map_dbl(boot, quantile, prob = 0.975))
res1


#' Next, we will use the Poisson trick and fit a poisson regression
dat1
dat2 <- dat1 %>% 
  mutate(dat_resample = map(data, ~ track_resample(., rate = minutes(30), tolerance = minutes(2))))
dat2

#' Now we extract the covariates
dat2 <- dat2 %>% 
  mutate(ssf = map(dat_resample, ~ .x %>% steps_by_burst %>% 
                     random_steps %>% extract_covariates(env))) 

#' Next, unnest the data, and add for each step a unique id
dat3 <- dat2 %>% select(ssf, id) %>% unnest(cols = ssf) %>% 
  mutate(step_id = paste0(id, "-", step_id_))

#' The process is the same as for the mixed RSF:
#' 
#' 1. Set up model, but do not fit
#' 2. Set random intercept variance to large fixed value, set other variance components to 0
#' 3. Fit the model
#'
#' However, there are a few differences:
#' 
#' - we will use a Poisson likelihood rather than logistic 
#' - we won't need weights
#' - we will include fixed intercepts for each step_id
#' 
tic()
res1_ssf <- glmmTMB(case_ ~ elevation + pop_den + forest + (1|step_id) + (0 + elevation|id) +
                      (0 + pop_den | id) + ( 0 + forest  | id), family=poisson(), 
                    data = dat3, doFit=FALSE)

#' Set variance of random intercept to 10^6
res1_ssf$parameters$theta[1] <- log(1e3)
nvar_parm <- length(res1_ssf$parameters$theta)
res1_ssf$mapArg <- list(theta = factor(c(NA, 1:(nvar_parm - 1))))
res1_ssf <- glmmTMB:::fitTMB(res1_ssf)
summary(res1_ssf)
toc()


#' ### Comparisons
#' 

# Mean coefficient from individual fits 
ssf_coefs <- res0 %>% select(id, term, data) %>% unnest(cols = data) 
ssf_coefs %>%  group_by(term) %>% summarise(
  mean = mean(estimate), 
  se = sd(estimate) / sqrt(n()))

# Population mean coefficient from mixed modeL
summary(res1_ssf)$coef$cond[-1,]


#'Compare variance of individual esimtates to variance component from mixed model.
# Variance of coefficientS from individual fits
ssf_coefs %>% group_by(term) %>% summarize(var = var(estimate), sd = sd(estimate))

# Variance of coefficientS from mixed model
summary(res1_ssf)$varcor #var



#' Look at individual coefficients

comp <- bind_rows(
  tibble(
    id = ssf_coefs$id,
    term = ssf_coefs$term, 
    estimate = ssf_coefs$estimate, 
    method = rep("ind", nrow(ssf_coefs))
  ), 
  coef(res1_ssf)$cond$id[ , -1] %>% rownames_to_column("id") %>% 
    pivot_longer(-id, names_to = "term", values_to = "estimate") %>% 
    mutate(method = "ME")
)

comp %>% ggplot(aes(id, estimate, col = method)) + geom_point() +
  facet_wrap(~ term, scale = "free", ncol = 2)

#' Note here that a few coefficients fo relevation and popD show considerable shrinkage. These are coefficients with large SEs as seen by inspecting the individual fits.
#+fig.width=14, fig.height=6
ggplot(data=allests, 
       aes(x=id, y=Estimate, col=method))+
  geom_point(size=3.5, position=position_dodge(width=0.3))+
  xlab("")+ylab(expression(hat(beta)))+facet_wrap(~term)


#' 	
#' ## Session information:	
#' 	
sessioninfo::session_info()