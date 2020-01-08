# Measure extent of overlap of vultures with protected areas

# load packages
library(wdpar)
library(tidyverse)
library(ggmap)
library(amt)
library(lubridate)
library(readr)
library(adehabitatHR)

swazi_raw_pa_data <- wdpa_fetch("Swaziland",  wait = TRUE, download_dir = "C:\\Users\\Adam\\Documents\\Science\\Methods & Stats\\GIS\\wdpar")

#' clean the data
swazi_pa_data <- wdpa_clean(swazi_raw_pa_data)

# reproject data
# swazi_pa_data <- st_transform(swazi_pa_data, 4326)
swazi_pa_data <- st_transform(swazi_pa_data, 102022)

# calculate percentage of land inside protected areas (km^2)
statistic <- swazi_pa_data %>%
  as.data.frame() %>%
  #select(-geometry) %>%
  group_by(IUCN_CAT) %>%
  summarize(area_km = sum(AREA_KM2)) %>% 
  mutate(percentage = (area_km / sum(area_km)) * 100) 
sum(statistic$area_km)

#' load in a track
setwd("C:\\Users\\Adam\\Documents\\Science\\Manuscripts\\vulture_habitat\\Regularised vulture tracks")   # path to the data

mydata <- read.csv("swazi_reg.csv", header = TRUE)
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
#  )  %>%
#  transform_coords(sp::CRS("+init=epsg:4326"))
)

head(trk)

# turn the tracks into something the sf package can work with 
sf_trk <- trk %>% dplyr::select(x_, y_,id) 
sf_trk
pts <- st_as_sf (sf_trk, coords = c ("x_", "y_"), crs = 102022) # 4326
pts

# count the number of points that overlap with protected areas
res <- st_intersects (pts, swazi_pa_data)
# get the proportion of those
length (unlist (res)) / nrow (pts) 

#' calculate overlap with home range

#' filter out one track to test
#' if you comment this out the area is the total of all tracks
#' encompassed by the protected areas
#' trk <- trk %>% filter(id == "ID2") %>% droplevels()

# Keep only the id column and the x and y coordinates
mydata.sp <- trk[, c("id", "x_", "y_")] 

coordinates(mydata.sp) <- c("x_", "y_")

# calculate the home range kde
kernel.ref <- kernelUD(mydata.sp, h = "href")  # href = the reference bandwidth
mydata.kernel.poly <- getverticeshr(kernel.ref, percent = 95, unout = "km2") 
print(mydata.kernel.poly)  # returns the area of each polygon

# look at overlap
kernel.sf <- st_as_sf(mydata.kernel.poly)
st_crs(kernel.sf) = 102022

#' this works for all of the IDs if you don't filter by a specific individual
#' else it gives the value for the individual 
intersection <- st_intersection(kernel.sf, swazi_pa_data)
intersection_area <-  sum(st_area(intersection[intersection])) 
intersection_area
504005258 /1e+6 # ID1
734901991/1e+6 # ID2

#' look at the intersection of each 
#' create a function to do it all together 
calculate_area <- function(x){
  intersection <- st_intersection(x, swazi_pa_data)
  intersection_area <- sum(st_area(intersection)) 
  print(intersection_area)
}

# Set field to split on
splitField <- "id"
ncSplit <- split(kernel.sf, f = kernel.sf[[splitField]])

#' use lapply on it
lapply(ncSplit, calculate_area)
504002253/1e+6
734901991/1e+6

#' what's the % of coverage?
(504002253/1e+6) / 48643.84 * 100
(734901991/1e+6) / 220454.44 * 100

#' plots 
plot(swazi_pa_data[,4] , axes = TRUE)
plot(kernel.sf$geometry, axes = TRUE, add = TRUE)

library(leaflet)
# https://mgritts.github.io/2016/05/13/adehabitat-visualization/
#' first convert to longitude and latitude
proj4string(mydata.kernel.poly) <- CRS("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

#' create the map 
sdf_poly <- spTransform(mydata.kernel.poly, CRS('+init=epsg:4326'))
leaflet(sdf_poly) %>% addTiles() %>%
  addPolygons()


