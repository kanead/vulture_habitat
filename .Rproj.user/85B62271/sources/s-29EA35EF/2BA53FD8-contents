# Andre Vulture Tracking Dataset 

library(lubridate)
library(SDLfilter)
library(amt)
library(sp)

# andre

# select andre data which is the data we tracked in andreland
andre_data <- filter(mydata, study == "andre")
andre_data

#' Check for duplicated observations (ones with same lat, long, timestamp,
#'  and individual identifier).
ind2<-andre_data %>% select(time, long, lat, id) %>%
  duplicated 
sum(ind2) 
# remove them 
andre_data$dups <- ind2
andre_data <- filter(andre_data,dups=="FALSE")
andre_data

# set the time column
levels(factor(andre_data$id))
# can look at an individual level with 
(filter(andre_data,id=="170948"))

# all of the data is in the format of day-month-year 
andre_data$New_time<-parse_date_time(x=andre_data$time,c("%d/%m/%Y %H:%M"))

# keep only the new time data
andre_data <- select(andre_data, New_time,long,lat,id,species,study)
andre_data <- rename(andre_data, time = New_time)
andre_data

#' estimate vmax for threshold speed 
#' names(andre_data)[names(andre_data) == 'time'] <- 'DateTime'
#' speed.est.data <- andre_data %>% filter(id == "ID2") %>%  select(id,DateTime,lat,long)
#' speed.est.data$qi = 5
#' est.vmax(sdata = data.frame(speed.est.data))

#' filter extreme data based on a speed threshold 
#' based on vmax which is km/hr
#' time needs to be labelled DateTime for these functions to work
names(andre_data)[names(andre_data) == 'time'] <- 'DateTime'
SDLfilterData<-ddfilter.speed(data.frame(andre_data), vmax = 70, method = 1)
length(SDLfilterData$DateTime)

#' rename everything as before
andre_data <- SDLfilterData
names(andre_data)[names(andre_data) == 'DateTime'] <- 'time'

# check the minimum time and the maximum time
min_time <- andre_data %>% group_by(id) %>% slice(which.min(time))
data.frame(min_time)

max_time <- andre_data %>% group_by(id) %>% slice(which.max(time))
data.frame(max_time)

#' determine the length of time each bird was tracked for
duration <- difftime(max_time$time, min_time$time, units = "days");duration

#' try the amt package 
trk <-
  mk_track(
    andre_data,
    .x = long,
    .y = lat,
    .t = time,
    id = id,
    species = species,
    crs = CRS("+init=epsg:4326"))  %>%
  transform_coords(
    sp::CRS( #' we can transform the CRS of the data to an equal area projection
      #' https://epsg.io/102022
      "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
    )
  )


#' summarise the sampling rate
data_summary <- trk %>% nest(-id) %>% mutate(sr = map(data, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% unnest %>% arrange(id) ; data_summary


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
data_summary$study <- "andre"
data_summary

#' can export this data summary 
#' write.csv(data_summary, file="track_resolution_summary/andre_data_summary.csv", row.names = FALSE)

#' We can map the data
#' turn back to lat long
trk_map <-
  mk_track(
    andre_data,
    .x = long,
    .y = lat,
    .t = time,
    id = id,
    species = species,
    crs = CRS("+init=epsg:4326")
  )

#' plot all of the data on the one graph
library(ggmap)
qmplot(x_,
       y_,
       data = trk_map,
       maptype = "toner-lite",
       colour = id)
#  color = I("red"))

#' plot each track on a separate panel using facet
(
  qmplot(
    x_,
    y_,
    data = trk_map,
    maptype = "toner-background",
    colour = id
  ) +
    facet_wrap( ~ id)
)
