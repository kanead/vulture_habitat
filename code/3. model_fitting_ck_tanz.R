library(raster)
library(lubridate)
library(tidyverse)
library(amt)
library(glmmTMB)
library(INLA)
library(lme4)

annotated_data <- read_csv("annotated/step selection corinne-4620997910743285856.csv") 
head(annotated_data)
raw_data <- read_csv("full/CorinneSSFAll.csv")
head(raw_data)

raw_data <- select(raw_data, case_, sl_, step_id_)

raw_data$veg <- annotated_data$`MODIS Land VCF 250m Yearly Terra Percent Non-Tree Vegetation`
raw_data$uplift <- annotated_data$`Movebank Orographic Uplift (from ASTER DEM and NARR)`

ssfdat <- raw_data

#' Center and scale variables
ssfdat<-ssfdat %>% mutate(elev=as.numeric(scale(veg)), 
                          popD=as.numeric(scale(uplift)))


#' Fit an SSF to a single animal
summary(fit_issf(case_ ~ veg+uplift+sl_+log(sl_)+strata(step_id_), 
                 data = subset(ssfdat, id=="M1")))


land_use <- raster("data/landuse_study_area.tif")
wet <- land_use == 90
names(wet) <- "wet"

# set a seed for reproducibility
set.seed(1234)
rsf <- dat_all %>%
  mutate(rp = map(trk, function(x) {
    dist_hrc <- distance_to_center(x, wet)
    cov <- stack(wet, dist_hrc)
    x <- x %>% track_resample(rate = minutes(10), tolerance = seconds(120)) %>%
      filter_min_n_burst()
    x <- x[1:100, ] %>% random_points() %>%
      extract_covariates(cov)
  })) %>% select(id, rp) %>% unnest() %>%
  mutate(y = as.numeric(case_), id = as.numeric(factor(id))) %>%
  select(id, y, wet, dist_cent)
ssf <- dat_all %>%
  mutate(rs = map(trk, function(x) {
    dist_hrc <- distance_to_center(x, wet)
    cov <- stack(wet, dist_hrc)
    x <- x %>% track_resample(rate = minutes(10), tolerance = seconds(120)) %>%
      filter_min_n_burst()
    x <- x[1:100, ] %>% steps_by_burst %>% random_steps() %>%
      extract_covariates(cov)
  })) %>% select(id, rs) %>% unnest() %>%
  mutate(y = as.numeric(case_), id = as.numeric(factor(id))) %>%
  select(id, y, wet, dist_cent, stratum = step_id_)

# INLA
rsf$id0 <- rsf$id1 <- rsf$id

rsf$weights <- ifelse(rsf$y == 1, 1, 1000)
rsf$dist_cent <- scale(rsf$dist_cent)

# formula
formula <- y ~ wet + dist_cent +
  f(id, model = "iid",
    hyper = list(theta = list(initial = log(1e-6), fixed = TRUE))) +
  f(id0, wet, values = 1:6, model = "iid",
    hyper = list(theta = list(initial = log(1), fixed = FALSE,
                              prior = "pc.prec", param = c(1, .05)))) +
  f(id1, dist_cent, values = 1:6, model="iid",
    hyper = list(theta = list(initial = log(1), fixed = FALSE,
                              prior = "pc.prec", param = c(1, .05))))

# Enable weighting
inla.setOption("enable.inla.argument.weights", TRUE)

# inla() call
m2 <- inla(formula, weights=rsf$weights, family = "binomial", data = rsf)

# glmmTMB

# Set up the model, but do not yet fit it
TMBStruc = glmmTMB(y ~ wet + dist_cent + (1|id) + (0 + wet | id ) + (0 + dist_cent | id),
                   weights = weights,
                   family=binomial,
                   data=rsf,
                   doFit=FALSE)
# Fix the standard deviation of the first random term, which is the (1|id) component
# in the above model equation
TMBStruc$parameters$theta[1] = log(1e3)
# Tell glmmTMB not to change the first entry of the vector of variances,
# and give all other variances another indicator to make sure they can be freely estimated
TMBStruc$mapArg = list(theta=factor(c(NA,1:2)))
# Then fit the model as follows:
  m3 = glmmTMB:::fitTMB(TMBStruc)
summary(m3)
