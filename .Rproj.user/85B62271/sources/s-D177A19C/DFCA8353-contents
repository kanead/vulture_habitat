# Corinne's Mara Vulture Tracking Dataset

library(lubridate)
library(SDLfilter)
library(amt)
library(sp)
library(janitor)
# Corinne_mara

# select Corinne_mara data which is Corinne's Mara data from everything
Corinne_mara <- filter(mydata, study == "Corinne_mara")
Corinne_mara

#' Check for duplicated observations (ones with same lat, long, time,
#'  and individual identifier).
ind2 <- Corinne_mara %>% select(long, lat, id) %>%
  duplicated
sum(ind2)
# remove them
Corinne_mara$dups <- ind2
Corinne_mara <- filter(Corinne_mara, dups == "FALSE")
Corinne_mara
tail(Corinne_mara)
# set the time column
levels(factor(Corinne_mara$id))
# can look at an individual level with
(filter(Corinne_mara, id == "AG082"))

#' all of the data is in the format of day-month-year
#' time zone is UTC by default
Corinne_mara$New_time <-
  parse_date_time(x = Corinne_mara$time, c("%d/%m/%Y %H:%M")) 

# keep only the new time data
Corinne_mara <-
  select(Corinne_mara, New_time, long, lat, id, species, study)
Corinne_mara <- rename(Corinne_mara, time = New_time)
Corinne_mara

#' filter extreme data based on a speed threshold
#' based on vmax which is km/hr
#' time needs to be labelled DateTime for these functions to work
names(Corinne_mara)[names(Corinne_mara) == 'time'] <- 'DateTime'
SDLfilterData <-
  ddfilter.speed(data.frame(Corinne_mara), vmax = 70, method = 1)
length(SDLfilterData$DateTime)

#' rename everything as before
Corinne_mara <- SDLfilterData
names(Corinne_mara)[names(Corinne_mara) == 'DateTime'] <- 'time'

# check the minimum time and the maximum time
min_time <- Corinne_mara %>% group_by(id) %>% slice(which.min(time))
data.frame(min_time)

max_time <- Corinne_mara %>% group_by(id) %>% slice(which.max(time))
data.frame(max_time)

#' determine the length of time each bird was tracked for
duration <-difftime(max_time$time, min_time$time, units = "days"); duration

# try the amt package
trk <-
  mk_track(
    Corinne_mara,
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
data_summary$study <- "Corinne_mara"
data_summary

#' can export this data summary 
#' write.csv(data_summary, file="track_resolution_summary/Corinne_Mara_data_summary.csv", row.names = FALSE)