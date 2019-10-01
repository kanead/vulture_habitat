# Schabo's Vulture Tracking Dataset

library(lubridate)
library(SDLfilter)
library(amt)
library(sp)
library(janitor)
# Schabo

# select Schabo data which is Schabo's data from everything
Schabo_data <- filter(mydata, study == "Schabo")
Schabo_data

#' Check for duplicated observations (ones with same lat, long, time,
#'  and individual identifier).
ind2 <- Schabo_data %>% select(long, lat, id) %>%
  duplicated
sum(ind2)
# remove them
Schabo_data$dups <- ind2
Schabo_data <- filter(Schabo_data, dups == "FALSE")
Schabo_data
tail(Schabo_data)
# set the time column
levels(factor(Schabo_data$id))
# can look at an individual level with
(filter(Schabo_data, id == "Cerhu"))

#' all of the data is in the format of day-month-year
#' time zone is UTC by default
Schabo_data$New_time <-
  parse_date_time(x = Schabo_data$time, c("%d/%m/%Y %H:%M:%S")) 

# keep only the new time data
Schabo_data <-
  select(Schabo_data, New_time, long, lat, id, species, study)
Schabo_data <- rename(Schabo_data, time = New_time)
Schabo_data

#' filter extreme data based on a speed threshold
#' based on vmax which is km/hr
#' time needs to be labelled DateTime for these functions to work
names(Schabo_data)[names(Schabo_data) == 'time'] <- 'DateTime'
SDLfilterData <-
  ddfilter.speed(data.frame(Schabo_data), vmax = 70, method = 1)
length(SDLfilterData$DateTime)

#' rename everything as before
Schabo_data <- SDLfilterData
names(Schabo_data)[names(Schabo_data) == 'DateTime'] <- 'time'

# check the minimum time and the maximum time
min_time <- Schabo_data %>% group_by(id) %>% slice(which.min(time))
data.frame(min_time)

max_time <- Schabo_data %>% group_by(id) %>% slice(which.max(time))
data.frame(max_time)

#' determine the length of time each bird was tracked for
duration <-difftime(max_time$time, min_time$time, units = "days"); duration

# try the amt package
trk <-
  mk_track(
    Schabo_data,
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


#' Calculate home range size for data that is not regularised
mcps <- trk %>% nest(-id) %>%
  mutate(mcparea = map(data, ~ hr_mcp(., levels = c(0.95)) %>% hr_area)) %>%
  select(id, mcparea) %>% unnest()

mcps$area <- mcps$area / 1000000
mcp_95 <- mcps %>% arrange(id)

#' Same for KDE
kde <- trk %>% nest(-id) %>%
  mutate(kdearea = map(data, ~ hr_kde(., levels = c(0.95)) %>% hr_area)) %>%
  select(id, kdearea) %>% unnest()

kde$kdearea <-  kde$kdearea / 1000000
kde_95 <- kde %>% arrange(id)


#' combine the summary stats
data_summary$duration <- duration
data_summary$min_time <- min_time$time
data_summary$max_time <- max_time$time
data_summary$kde <- kde_95$kdearea
data_summary$mcps <- mcp_95$area
data_summary$species <- min_time$species
data_summary$study <- "Schabo"
data_summary

#' can export this data summary 
#' write.csv(data_summary, file="track_resolution_summary/Schabo_data_summary.csv", row.names = FALSE)