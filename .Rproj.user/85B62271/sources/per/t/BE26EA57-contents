#Packages----
library(car)
library(mgcv)
library(lme4)
library(effects)
library(lattice)
library(MuMIn)
library(tidyverse)
library(lubridate)

#Load data----
dat <- read.csv("results/combined_data_week.csv", header=T)
head(dat)
str(dat)
names(dat)

#EDA----
plot(dat$mcp, dat$kdearea)
cor(dat$mcp, dat$kdearea) #strong correlation between the two measures of HR; probably OK to just focus on one

#dat3 <- dat[dat$kdearea<10, ] #exclude outliers (TMP)
dat3 <- dat #keep outliers (Adam has checked them, they are legit)
plot(dat3$mcp, dat3$kdearea)

#Collinearity
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
names(dat3)
png("results/Correlation_week.png", width=10000, height=10000, units="px")
pairs(dat3[,c(9:36,38,39,4)], lower.panel = panel.smooth, upper.panel = panel.cor,
      gap=0, row1attop=FALSE)
dev.off()

#>0.6 (~ >0.5)
#study area-sensor type
#study area-year
#~study area-slope
#sensor type-min act
#sensor type-year
#sd act-mean act
#corine mode-tree median (and ~mode)
#stats of slope, aspect, altitude with each other (inc. sd)
#slope-altitude
#~slope-altitude-aspect_sd
#~ndvi median/mean-ndvi sd
#ndvi mean and day length
#non linear: tree mean-tree sd
#non linear: month-ndvi

#will have to check VIF of final model as well

#what to do with ndvi and day length? Probably keep - VIF later looks OK

dat4 <- dat3
#treat factors as such
dat4$sex <- as.factor(dat4$sex)
dat4$sensor_type <- as.factor(dat4$sensor_type)
dat4$study_areas_id <- as.factor(dat4$study_areas_id)
dat4$animals_id <- as.factor(dat4$animals_id)

#standardise covariates
names(dat4)
for (i in c(30, 39, 9, 11, 13, 38, 40)){ #select columns used below
  dat4[,i] <- (dat4[,i] - mean(dat4[,i]))/sd(dat4[,i])
}
#transform response
hist(dat4$kdearea,breaks=100)
hist(log(dat4$kdearea),breaks=100)
dat4$log_kde <- log(dat4$kdearea)


#GLMM----
fit1 <- lmer(log_kde ~ mean_act + sensor_type + sex + age + ndvi_Mean + slope_Mean + tree_Mean  + mean_day_length + (1|study_areas_id/animals_id), data=dat4, REML=F, na.action=na.fail)
#same problem with convergence when including both predator and study area
plot(fit1)
summary(fit1)
vif(fit1)
effs <- allEffects(fit1)
plot(effs, 'mean_act')
plot(effs, 'sex')
plot(effs, 'age')
plot(effs, 'ndvi_Mean')
plot(effs, 'slope_Mean')
plot(effs, 'tree_Mean')
plot(effs, 'sensor_type')
#plot(effs, 'predator')
plot(effs, 'mean_day_length')
dotplot(ranef(fit1, condVar=TRUE))
#model averaging
dd <- dredge(fit1)
#convergence issues 
avg <- model.avg(dd, subset = delta < 4)
summary(avg)
#The subset (or conditional) average only averages over the models where the parameter appears. An alternative, the full average assumes that a variable is included in every model, but in some models the corresponding coefficient (and its respective variance) is set to zero. Unlike the subset average, it does not have a tendency of biasing the value away from zero. The full average is a type of shrinkage estimator and for variables with a weak relationship to the response they are smaller than subset estimators.

#model with interaction
fit1_2 <- lmer(log_kde ~ mean_act * sensor_type + sex + age + ndvi_Mean + slope_Mean + tree_Mean + predator + mean_day_length + (1|study_areas_id/animals_id), data=dat4, REML=F, na.action=na.fail)
plot(effect("mean_act:sensor_type", fit1_2))
AIC(fit1, fit1_2)
summary(fit1_2)

#GAMM----
fit2 <- gam(log_kde ~ s(mean_act, bs="ts", by = sensor_type) + sensor_type + sex + age + s(ndvi_Mean, bs="ts") + s(slope_Mean, bs="ts") + s(tree_Mean, bs="ts") + predator + s(mean_day_length, bs="ts")+ s(animals_id, bs="re"), data=dat4, na.action=na.fail, method="ML")
fit2_2 <- gam(log_kde ~ mean_act*sensor_type + sex + age + s(ndvi_Mean, bs="ts") + s(slope_Mean, bs="ts") + s(tree_Mean, bs="ts") + predator + s(mean_day_length, bs="ts")+ s(animals_id, bs="re"), data=dat4, na.action=na.fail, method="ML")
fit2_3 <- gam(log_kde ~ s(mean_act, bs="ts") + sensor_type + sex + age + s(ndvi_Mean, bs="ts") + s(slope_Mean, bs="ts") + s(tree_Mean, bs="ts") + predator + s(mean_day_length, bs="ts")+ s(animals_id, bs="re"), data=dat4, na.action=na.fail, method="ML")
plot(fit2, all.terms=T, scale=0)
plot(fit2_2, all.terms=T, scale=0)
plot(fit2_3, all.terms=T, scale=0)
gam.check(fit2)
gam.check(fit2_2)
summary(fit2)
summary(fit2_2)
summary(fit2_3)

#model averaging
dd2 <- dredge(fit2_2)
#convergence issues (even after standardising covariates)
avg2 <- model.avg(dd2, subset = delta < 4)
summary(avg2)

#with area R Eff
fit2_3 <- gam(log_kde ~ s(mean_act, bs="ts", by = sensor_type) + sensor_type + sex + age + s(ndvi_Mean, bs="ts") + s(slope_Mean, bs="ts") + s(tree_Mean, bs="ts") + predator + s(mean_day_length, bs="ts")+ s(study_areas_id, animals_id, bs="re"), data=dat4, na.action=na.fail, method="ML")
plot(fit2_3, all.terms=T, scale=0)
summary(fit2_3)
#doesn't seem to be making much difference


library(broom)
tidy(fit1_2)
tidy(fit2_2)
