# Step Selection Function Analysis for Corinne Kendall's Tanzania Data

library(raster)
library(lubridate)
library(tidyverse)
library(amt)
library(glmmTMB)
library(INLA)
library(lme4)

#' load in the annotated data from that we got from Movebank
annotated_data <- read_csv("annotated/step selection corinne-1660591318242364550.csv") 
head(annotated_data)
#' load in the full data set we prepared earlier
raw_data <- read_csv("amt_tracks/CorinneSSFAll.csv")
head(raw_data)
#' extract the required variables from the full dataset 
raw_data <- dplyr::select(raw_data, case_, sl_, step_id_,t1_)
head(raw_data)
ssfdat <- raw_data

#' Attributes from annotated environmental data:
#' Name: GlobCover 2009 Land-Cover Classification
#' Description: Land cover class:
#' 11 = post-flooding or irrigated croplands
#' 14 = rainfed croplands
#' 20 = mosaic cropland (50-70%)/vegetation (grassland, shrubland, forest) (20-50%)
#' 30 = mosaic vegetation (grassland, shrubland, forest) (50-70%) / Cropland (20-50%)
#' 40 = closed to open (>15%) broadleaved evergreen and/or semi-deciduous forest (>5m)
#' 50 = closed (>40%) broadleaved deciduous forest (>5m)
#' 60 = open (15-40%) broadleaved deciduous forest (>5m)
#' 70 = closed (>40%) needleleaved evergreen forest (>5m)
#' 90 = open (15-40%) needleleaved deciduous or evergreen forest (>5m)
#' 100 = closed to open (>15%) mixed broadleaved and needleleaved forest (>5m)
#' 110 = mosaic forest/shrubland (50-70%)/grassland (20-50%)
#' 120 = mosaic grassland (50-70%)/forest/shrubland (20-50%)
#' 130 = closed to open (>15%) shrubland (<5m)
#' 140 = closed to open (>15%) grassland
#' 150 = sparse (>15%) vegetation (woody vegetation, shrubs, grassland)
#' 160 = closed (>40%) broadleaved forest regularly flooded (fresh water)
#' 170 = closed (>40%) broadleaved semi-deciduous and/or evergreen forest regularly flooded (saline water)
#' 180 = closed to open (>15%) vegetation (grassland, shrubland, woody vegetation) on regularly flooded or waterlogged soil (fresh, brackish or saline water)
#' 190 = artificial surfaces and associated areas (urban areas >50%)
#' 200 = bare areas
#' 210 = water bodies
#' 220 = permanent snow and ice
#' Unit: none
#' No data values: 255 (provider), NaN (interpolated)
#' Interpolation: nearest-neighbour

#' stick the landcover class and id variables onto this data set

names(annotated_data)[names(annotated_data) == 'GlobCover 2009 Land-Cover Classification'] <- 'LandClass'
ssfdat$LandClass<-as.character(annotated_data$LandClass)
ssfdat$id<-annotated_data$`individual-local-identifier`

#' can subset to diurnal times, this is optional 
ssfdat <- with( ssfdat , ssfdat[ hour(  t1_ ) >= 6 & hour(  t1_ ) <= 18 , ] )
# have a look at the hours to make sure they are diurnal
hour(ssfdat$t1_)

#' Group landcover classes (this is the classifcation from the amt authors)

ssfdat<-ssfdat %>% mutate(landC = fct_collapse(LandClass,
                                               agri = c("11", "14","20", "30"),
                                               forest =c("30","40","50","60", "70","80", "90","100"),
                                               shrub= c("110", "130", "150"),
                                               grass = c("120", "140"),
                                               wet= c("160"),
                                               other = c("170", "180", "190", "200", "210", "220")))

#' Look Distribution of variables for used and available
ggplot(ssfdat, aes(x=landC, y=..prop..,group=case_, colour=case_))+
  geom_bar(position="dodge", aes(fill=case_))+facet_wrap(~id, scales="free")

#' Fit an SSF to a single animal
levels(as.factor(ssfdat$id))
summary(fit_issf(case_ ~ landC+sl_+log(sl_)+strata(step_id_), 
                 data = subset(ssfdat, id=="163113")))

#' Since not all animals experience
#' all habitat types, lets just explore grass-habitat versus non-grass
ssfdat$grass<-ifelse(ssfdat$landC=="grass", 1, 0)

#' Fit an SSF model to data from each animal
#' here's the function
fitted_ssf <- function(data){
      mod <- fit_issf(case_ ~ grass+sl_+log(sl_)+strata(step_id_), data=data)
      return(mod)
}
#' fit it to our data
ssffits <-ssfdat %>%  nest(-id) %>% 
  dplyr::mutate(mod = purrr::map(data, fitted_ssf)) 

#' Look at first model
ssffits$mod[[1]]

#' Now, use tidy to extract information about the model fits
ssffits <- ssffits %>%
  dplyr::mutate(tidy = purrr::map(mod, ~broom::tidy(.x$model)),
                n = purrr::map(data, nrow) %>% simplify())

ssffits$tidy

#' Now, create data frame w/ the coefficients, etc
ssf_coefs <- ssffits %>%
  tidyr::unnest(tidy) %>%
  dplyr::select(-(std.error:conf.high)) 

ssf_coefs %>% tidyr::spread(term, estimate)

#' Plot coefficients from the model
ssf_coefs %>% 
  ggplot(., aes(x=1, y=estimate,fill = factor(id))) + 
  geom_dotplot(binaxis="y", stackdir="center")+geom_hline(yintercept=0)+
  facet_wrap(~term, scales="free")

#' Alternative method that does analysis in one go rather than
#' looping over the ids through a function
#' two-step procedure implemented in the TwoStepCLogit
#' package (Craiu et al. 2016)

#' Prepare covariates

ssfdat<-ssfdat %>%
  mutate(y = as.numeric(case_), id = as.numeric(factor(id))) %>%
  dplyr::select(id, y, grass, stratum = step_id_)
ssfdat$stratum_id <- paste(ssfdat$id, ssfdat$stratum)


library(TwoStepCLogit)
Ts.estim(y ~ grass + strata(stratum_id) + cluster(id),
         random = ~ grass,
         data = ssfdat,
         D="UN(1)")



