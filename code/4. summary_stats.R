#' Code for extracting summary statistics from the comparative vulture project
#' Summarise the temporal resolution of the tracking data first 

#' Load the required packages
library(readr)
library(tidyverse)

#' Section 1: Load the data ----
data_path <- "track_resolution_summary"   # path to the data

files <- dir(data_path, pattern = "*.csv") # get file names

res_data <- files %>%
  # read in all the files, appending the path before the filename
  map(~ read_csv(file.path(data_path, .))) %>% 
  reduce(rbind)

res_data
#' associate a broad geographic location with each study

#' CK = east
#' masai = east
#' north = east
#' GA_Namibia = south
#' inter = south
#' Kerri = south
#' mend_Namibia = south
#' Morgan = south
#' Swazi = south
#' andre = south
#' ralph = south

res_data$region <- if_else(res_data$study == "CK" | res_data$study == "masai" | res_data$study == "north", "east", "south")

#'export the combined summary stats table for the temporal resolution of the data
write.csv(res_data, file="track_resolution_summary/summary_all_tracks.csv", row.names = FALSE)

#' run some analyses on the data
#' extract data for birds that were tracked for over a year and remove some suspect data
#'the double releases from Kerri

nonWantedLevels<-c("AG382", "AM89", "AM88", "AM87")
subset <- res_data %>% filter(!id %in% nonWantedLevels)
levels(as.factor(subset$id))
#' keep only the birds that were tracked for over a year 
subset <- filter(subset, duration > 365)
#' boxplots of the KDEs by speices 
ggplot(data = subset, mapping = aes(x = species, y = kde)) + geom_boxplot() + ylab("KDE 95%")
#' boxplots of the KDEs by region  
ggplot(data = subset, mapping = aes(x = region, y = kde)) + geom_boxplot() + ylab("KDE 95%")
#' group by species and get the mean kde
subset %>% group_by(species) %>% summarize(mean_kde = mean(kde))
#' group by species and get the max kde
subset %>% group_by(species) %>% summarize(max_kde = max(kde))
#' group by species and get the median kde
subset %>% group_by(species) %>% summarize(median_kde = median(kde))
#' extract one species and get its max kde 
filter(subset, species == "cv") %>% summarize(maxkde = max(kde))
#' group by region and get the median kde
subset %>% group_by(region) %>% summarize(median_kde = median(kde))

