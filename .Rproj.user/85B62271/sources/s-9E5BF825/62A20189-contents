# Glynn's Vulture Tracking Dataset

library(lubridate)
library(SDLfilter)
library(amt)
library(sp)
library(janitor)
# Glynn

# select Glynn data which is Glynn's data from everything
Glynn_data <- filter(mydata, study == "Glynn")
Glynn_data

#' Check for duplicated observations (ones with same lat, long, time,
#'  and individual identifier).
ind2 <- Glynn_data %>% select(long, lat, id) %>%
  duplicated
sum(ind2)
# remove them
Glynn_data$dups <- ind2
Glynn_data <- filter(Glynn_data, dups == "FALSE")
Glynn_data
tail(Glynn_data)
# set the time column
levels(factor(Glynn_data$id))
# can look at an individual level with
(filter(Glynn_data, id == "136377"))

#' all of the data is in the format of day-month-year
#' time zone is UTC by default
Glynn_data$New_time <-
  parse_date_time(x = Glynn_data$time, c("%d/%m/%Y %H:%M")) 

# keep only the new time data
Glynn_data <-
  select(Glynn_data, New_time, long, lat, id, species, study)
Glynn_data <- rename(Glynn_data, time = New_time)
Glynn_data

#' filter extreme data based on a speed threshold
#' based on vmax which is km/hr
#' time needs to be labelled DateTime for these functions to work
names(Glynn_data)[names(Glynn_data) == 'time'] <- 'DateTime'
SDLfilterData <-
  ddfilter.speed(data.frame(Glynn_data), vmax = 70, method = 1)
length(SDLfilterData$DateTime)

#' rename everything as before
Glynn_data <- SDLfilterData
names(Glynn_data)[names(Glynn_data) == 'DateTime'] <- 'time'

# check the minimum time and the maximum time
min_time <- Glynn_data %>% group_by(id) %>% slice(which.min(time))
data.frame(min_time)

max_time <- Glynn_data %>% group_by(id) %>% slice(which.max(time))
data.frame(max_time)

#' determine the length of time each bird was tracked for
duration <-difftime(max_time$time, min_time$time, units = "days"); duration

# try the amt package
trk <-
  mk_track(
    Glynn_data,
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
data_summary$study <- "Glynn"
data_summary

#' can export this data summary 
#' write.csv(data_summary, file="track_resolution_summary/Glynn_data_summary.csv", row.names = FALSE)