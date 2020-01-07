# Measure extent of overlap of vultures with protected areas

# load packages
library(wdpar)
library(tidyverse)
library(ggmap)
library(amt)
library(lubridate)
library(readr)

# download protected area data 
# (excluding areas represented as point localities)
# extract protected areas for 
global_raw_data <- wdpa_fetch("global", wait = TRUE, download_dir = "C:\\Users\\Adam\\Documents\\Science\\Methods & Stats\\GIS\\wdpar")

global_raw_data 

# find names of all countries in Africa (ISO3 format)
cn <-
  country_data %>%
  filter(continent == "Africa") %>%
  `[[`("iso3c") %>%
  unique() %>%
  sort()

# clean the data
global_raw_data <- wdpa_clean(global_raw_data)

# reproject data
pa_data <- st_transform(global_raw_data, 4326)

# plot it
plot(pa_data)

# load in all the regularised tracks

data_path <- "C:\\Users\\Adam\\Documents\\Science\\Manuscripts\\vulture_habitat\\Regularised vulture tracks"   # path to the data

files <- dir(data_path, pattern = "*.csv") # get file names
length(files)

mydata <- files %>%
  # read in all the files, appending the path before the filename
  map(~ read_csv(file.path(data_path, .))) %>% 
  reduce(rbind)

head(mydata)

# transform from albers to lat long
mydata$t_<-parse_date_time(x=mydata$t_,c("%Y-%m-%d %H:%M:%S"))
trk <-
  mk_track(
    mydata,
    .x = x_,
    .y = y_,
    .t = t_,
    id = id,
    crs = CRS(
      "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
    )
  )  %>%
  transform_coords(sp::CRS("+init=epsg:4326"))

# make sure the conversion worked
head(trk)

# turn the tracks into something the sf package can work with 
trk <- trk %>% select(x_, y_) 
trk
pts <- st_as_sf (trk, coords = c ("x_", "y_"), crs = 4326)
pts

# count the number of points that overlap with protected areas
res <- st_intersects (pts, pa_data)
# get the proportion of those
length (unlist (res)) / nrow (pts) 


