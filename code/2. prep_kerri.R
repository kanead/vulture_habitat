# Kerri's Vulture Tracking Dataset 

library(lubridate)
library(SDLfilter)
library(amt)
library(sp)

# kerri
# select kerri data which is kerri's data from the full data set
kerri_data <- filter(mydata, study == "kerri")
kerri_data

#' Check for duplicated observations (ones with same lat, long, timestamp,
#'  and individual identifier).
ind2<-kerri_data %>% select(long, lat, id) %>%
  duplicated 
sum(ind2) 
# remove them 
kerri_data$dups <- ind2
kerri_data <- filter(kerri_data,dups=="FALSE")
kerri_data

# set the time column
levels(factor(kerri_data$id))
# can look at an individual level with 
filter(kerri_data,id=="27233665")

#' some data are GMT some are GMT + 2, some are unknown
#' GMT+2 
#' 5008, AG313, AG314, AG329, AG330, AG331, AG332, AG349
#' AG350, AG351, AG352, AG353, AG356, AG382, AM220, AM222, AM226, AM227, AM233, AM234, AM235, AM240, AM264
#' AM267, AM272, AM295, AM86, AM87, AM88, AM89
#' AM22 has seconds listed for some time stamps so do AM227, AM234, AM235, AM240, AM264, AM267, AM272
#' GMT
#' Ingelheim, 22959306, 27230695, 27233665, 33640, 33798
#' Unknown and excluded here for the time being
#' LFV_009, X009
#' 
#' Some data have time arranged from most recent 
#' Ingelheim
#' 
#' date format for these is month-day-year 
#' "AM222" "AM226" "AM227" "AM234" "AM235" "AM240" "AM264" "AM267" "AM272" "AM295"
#' 
#' date format for these is day-month-year, no seconds 
#' 22959306, 27230695, 27233665, 33640, 33798, 5008, AG313, AG314, AG329, AG330, Ag331, AG332, AG349, AG350
#' AG351, AG352, AG353, AG356, AG382, AM220, AM233, AM86, AM87, AM88, AM89, Ingelheim, LFV_009, X009
#' 

group1 <- c("AM222", "AM226", "AM227", "AM234", "AM235", "AM240", "AM264", "AM267", "AM272", "AM295")
temp1 <- kerri_data %>% filter(id %in% group1)
temp1$New_time<-parse_date_time(x=temp1$time,c("%m/%d/%Y %H:%M:%S"), tz = "africa/johannesburg")

group2 <- c("5008", "AG313", "AG314", "AG329", "AG330", "Ag331", "AG332", "AG349", "AG350", "AG351", 
            "AG352", "AG353", "AG356", "AG382", "AM220", "AM233", "AM86", "AM87", "AM88", "AM89")
temp2 <- kerri_data %>% filter(id %in% group2)
temp2$New_time<-parse_date_time(x=temp2$time,c("%d/%m/%Y %H:%M"), tz = "africa/johannesburg")

group3 <- c("Ingelheim", "22959306", "27230695", "27233665", "33640", "33798")
temp3 <- kerri_data %>% filter(id %in% group3)
temp3$New_time<-parse_date_time(x=temp3$time,c("%d/%m/%Y %H:%M"))

# stick them back together again
kerri_data <- full_join(temp1, temp2) %>%
  full_join(., temp3) 

# Morgan's data is in reverse order of time
# sort by the bird ID and reverse the order
kerri_data <- kerri_data %>% group_by(id)  %>% 
  arrange(New_time, .by_group = TRUE)
kerri_data

#' make sure the time zones match
#' AG330 and AM220 are in South African Time so its New_time in UTC should be 2 hours behind its time
#' Ingelheim and 33798 are in UTC so its New_time and time should match
filter(kerri_data, id == "AG330")
filter(kerri_data, id == "AM220")
filter(kerri_data, id == "Ingelheim")
filter(kerri_data, id == "33798")

# keep only the new time data
kerri_data <- select(kerri_data, New_time,long,lat,id,species,study)
kerri_data <- rename(kerri_data, time = New_time)
kerri_data
tz(kerri_data$time)

#' filter extreme data based on a speed threshold 
#' based on vmax which is km/hr
#' time needs to be labelled DateTime for these functions to work
library(SDLfilter)
names(kerri_data)[names(kerri_data) == 'time'] <- 'DateTime'
SDLfilterData<-ddfilter.speed(data.frame(kerri_data), vmax = 70, method = 1)
length(SDLfilterData$DateTime)

#' rename everything as before
kerri_data <- SDLfilterData
names(kerri_data)[names(kerri_data) == 'DateTime'] <- 'time'

# check the minimum time and the maximum time
min_time <- kerri_data %>% group_by(id) %>% slice(which.min(time))
data.frame(min_time)

max_time <- kerri_data %>% group_by(id) %>% slice(which.max(time))
data.frame(max_time)

#' determine the length of time each bird was tracked for
duration <- difftime(max_time$time, min_time$time, units = "days"); duration

# try the amt package 
trk <-
  mk_track(
    kerri_data,
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
data_summary$study <- "Kerri"
data_summary

#' can export this data summary 
#' write.csv(data_summary, file="track_resolution_summary/kerri_data_summary.csv", row.names = FALSE)

#' We can map the data
#' turn back to lat long
trk_map <-
  mk_track(
    kerri_data,
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