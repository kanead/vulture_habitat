#########################################################################
#' Vulture comparative analysis
#' tutorials here https://www.jessesadler.com/post/gis-with-r-intro/
#' and here https://www.r-spatial.org/
#' 06 November 2018
#' 1_load_data - this loads in all of the tracking data and binds it
#########################################################################
#' Load the required packages
library(readr)
library(tidyverse)

#' Section 1: Load the data ----
data_path <- "raw_data"   # path to the data

files <- dir(data_path, pattern = "*.csv") # get file names

mydata <- files %>%
  # read in all the files, appending the path before the filename
  map(~ read_csv(file.path(data_path, .))) %>% 
  reduce(rbind)

#' filter the data to remove obvious outliers
mydata <- filter(mydata, lat < 20 & lat > -40 & long > 10)
head(mydata)
tail(mydata)
str(mydata)
levels(as.factor(mydata$study))
