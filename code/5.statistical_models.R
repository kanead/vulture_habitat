#' Statistical models for vulture home range data
#' 
#' load packages
library(tidyverse)
library(lme4)

mydata <- read_csv("results/summary_all_tracks.csv",col_names = TRUE)
master <- read_csv("results/Data_summary_collaborator_master.csv", col_names = TRUE)
head(mydata)
head(master)

#' add the age variable from the master sheet
master <- master %>% 
  select(id, Age)

mydata <- left_join(mydata, master, by = "id")
head(mydata)
tail(mydata)

#' look at the column names 
names(mydata) 

#' turn age into a binary variable, you're an adult or you're not
mydata$Age <- if_else(mydata$Age == "Adult", "adult", "immature")
mydata$Age <- as.factor(mydata$Age)
levels(mydata$Age)

#' remove the undeployed tracks highlighted in the master sheet
undeployed <- c("175589", "175590", "175591", "175592", "175593", "#109018542", "#199123449", 
                "#568291554", "#623177320", "623187113", "#649690680", "#834449829", "#834450834", "#834451702")
mydata <- mydata %>% dplyr::filter(!id %in% undeployed)

#' keep the gyps only
not_gyps <- c("hv", "lf", "wh" ,"gyps") # don't know what species "gyps" is
mydata <- mydata %>% dplyr::filter(!species %in% not_gyps)

#' subset to tracks of duration greater than some specified number of days
mydata <- mydata %>% filter(duration > 225)

#' standardise covariates
#' seems to work ok without and is needed in order to log the home range data
#' mydata$std_kde <- (mydata$kde - mean(mydata$kde))/sd(mydata$kde)
#' mydata$std_duration <- (mydata$duration - mean(mydata$duration))/sd(mydata$duration)
#' head(mydata)

#' make character variables factors 
mydata$species <- as.factor(mydata$species)
mydata$region <- as.factor(mydata$region)
mydata$study <- as.factor(mydata$study)

#' fit the mixed effect model
#' need to add sex as a covariate
m1 <- glm(log10(kde) ~ species + duration + region + Age, data = mydata)
summary(m1)
plot(m1)

mydata[48,]

#' 33798 is a huge outlier so let's remove it 
outlier <- c("33798") 
mydata <- mydata %>% dplyr::filter(!id %in% outlier)

#' run the model without the outlier 
m2 <- glm(log10(kde) ~ species + duration + region + Age, data = mydata)
summary(m2)
plot(m2)
tidy(m2)


#' age comes out as significant 
#' take a look at the effect on the raw scale (not logged)
boxplot(mydata$kde~mydata$Age)

#' try the same model but with the MCP home range measure 
m3 <- glm(log10(mcps) ~ species + duration + region + Age, data = mydata)
summary(m3)
plot(m3)

#' Ruppell's Vulture comes out here as having a significantly greater home range size than
#' the other Gyps when duration is filtered to 200 days. It's > 0.05 if the filter is more stringent 
#' than this 
