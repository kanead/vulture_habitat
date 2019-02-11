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

#'export the combined summary stats table for the temporal resolution of the data
write.csv(res_data, file="track_resolution_summary/temporal_res_all_tracks.csv", row.names = FALSE)

