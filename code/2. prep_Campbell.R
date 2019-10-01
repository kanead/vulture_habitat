# Campbell's Vulture Tracking Dataset

library(lubridate)
library(SDLfilter)
library(amt)
library(sp)
library(janitor)
# Campbell

# select Campbell data which is Campbell's data from everything
Campbell_data <- filter(mydata, study == "Campbell")
Campbell_data

#' Check for duplicated observations (ones with same lat, long, time,
#'  and individual identifier).
ind2 <- Campbell_data %>% select(long, lat, id) %>%
  duplicated
sum(ind2)
# remove them
Campbell_data$dups <- ind2
Campbell_data <- filter(Campbell_data, dups == "FALSE")
Campbell_data
tail(Campbell_data)
# set the time column
levels(factor(Campbell_data$id))
# can look at an individual level with
(filter(Campbell_data, id == "101411"))

#' all of the data is in the format of day-month-year
#' time zone is UTC by default
Campbell_data$New_time <-
  parse_date_time(x = Campbell_data$time, c("%d/%m/%Y %H:%M")) 

# keep only the new time data
Campbell_data <-
  select(Campbell_data, New_time, long, lat, id, species, study)
Campbell_data <- rename(Campbell_data, time = New_time)
Campbell_data

#' filter extreme data based on a speed threshold
#' based on vmax which is km/hr
#' time needs to be labelled DateTime for these functions to work
names(Campbell_data)[names(Campbell_data) == 'time'] <- 'DateTime'
SDLfilterData <-
  ddfilter.speed(data.frame(Campbell_data), vmax = 70, method = 1)
length(SDLfilterData$DateTime)

#' rename everything as before
Campbell_data <- SDLfilterData
names(Campbell_data)[names(Campbell_data) == 'DateTime'] <- 'time'

# check the minimum time and the maximum time
min_time <- Campbell_data %>% group_by(id) %>% slice(which.min(time))
data.frame(min_time)

max_time <- Campbell_data %>% group_by(id) %>% slice(which.max(time))
data.frame(max_time)

#' determine the length of time each bird was tracked for
duration <-difftime(max_time$time, min_time$time, units = "days"); duration

#' a couple of Campbell's data points have NAs for timestamps 
#' find them
Campbell_data %>%
       rowid_to_column() %>%
       filter(is.na(time))

#' remove them
Campbell_data <- Campbell_data %>%
  slice(c(-10136,-10137))

#' check they're gone 
Campbell_data %>%
  rowid_to_column() %>%
  filter(is.na(time))

# try the amt package
trk <-
  mk_track(
    Campbell_data,
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
data_summary$study <- "Campbell"
data_summary

#' can export this data summary 
#' write.csv(data_summary, file="track_resolution_summary/Campbell_data_summary.csv", row.names = FALSE)