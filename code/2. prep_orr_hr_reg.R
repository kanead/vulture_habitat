# Orr's Vulture Tracking Dataset

library(lubridate)
library(SDLfilter)
library(amt)
library(sp)
library(janitor)
library(adehabitatLT)

# orr

# select orr data which is Orr's data from everything
orr_data <- filter(mydata, study == "orr")
orr_data

#' Check for duplicated observations (ones with same lat, long, time,
#'  and individual identifier).
ind2 <- orr_data %>% dplyr::select(long, lat, id) %>%
  duplicated
sum(ind2)
# remove them
orr_data$dups <- ind2
orr_data <- filter(orr_data, dups == "FALSE")
orr_data
tail(orr_data)
# set the time column
levels(factor(orr_data$id))
# can look at an individual level with
(filter(orr_data, id == "15"))

#' all of the data is in the format of day-month-year
#' time zone is UTC by default
orr_data$New_time <-
  parse_date_time(x = orr_data$time, c("%Y/%m/%d %H:%M:%S")) 

# keep only the new time data
orr_data <-
  dplyr::select(orr_data, New_time, long, lat, id, species, study)
orr_data <- rename(orr_data, time = New_time)
orr_data

#' filter extreme data based on a speed threshold
#' based on vmax which is km/hr
#' time needs to be labelled DateTime for these functions to work
names(orr_data)[names(orr_data) == 'time'] <- 'DateTime'
SDLfilterData <-
  ddfilter.speed(data.frame(orr_data), vmax = 100, method = 1)
length(SDLfilterData$DateTime)

#' rename everything as before
orr_data <- SDLfilterData
names(orr_data)[names(orr_data) == 'DateTime'] <- 'time'

# check the minimum time and the maximum time
min_time <- orr_data %>% group_by(id) %>% slice(which.min(time))
data.frame(min_time)

max_time <- orr_data %>% group_by(id) %>% slice(which.max(time))
data.frame(max_time)

#' determine the length of time each bird was tracked for
duration <-difftime(max_time$time, min_time$time, units = "days"); duration

# try the amt package
trk <-
  mk_track(
    orr_data,
    .x = long,
    .y = lat,
    .t = time,
    id = id,
    species = species,
    crs = CRS("+init=epsg:4326")
  ) %>%
  transform_coords(
    sp::CRS(
      #' we can transform the CRS of the data to an equal area projection
      "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
    )
  )

#' summarise the sampling rate
data_summary <- trk %>% nest(-id) %>% mutate(sr = map(data, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% unnest %>% arrange(id)


#' Calculate home range size for data that is regularised
mcps <- trk4 %>% nest(-id) %>%
  mutate(mcparea = map(data, ~ hr_mcp(., levels = c(0.95)) %>% hr_area)) %>%
  dplyr::select(id, mcparea) %>% unnest()

mcps$area <- mcps$area / 1000000
mcp_95 <- mcps %>% arrange(id)

#' Same for KDE
kde <- trk4 %>% nest(-id) %>%
  mutate(kdearea = map(data, ~ hr_kde(., levels = c(0.95)) %>% hr_area)) %>%
  dplyr::select(id, kdearea) %>% unnest()

kde$kdearea <-  kde$kdearea / 1000000
kde_95 <- kde %>% arrange(id)


#' combine the summary stats
data_summary$duration <- duration
data_summary$min_time <- min_time$time
data_summary$max_time <- max_time$time
data_summary$kde <- kde_95$kdearea
data_summary$mcps <- mcp_95$area
data_summary$species <- min_time$species
data_summary$study <- "orr"
data_summary

#' can export this data summary 
#' write.csv(data_summary, file="track_resolution_summary/orr_data_summary.csv", row.names = FALSE)