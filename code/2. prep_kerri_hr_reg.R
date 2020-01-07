# Kerri's Vulture Tracking Dataset 

library(lubridate)
library(SDLfilter)
library(amt)
library(sp)
library(adehabitatLT)

# kerri
# select kerri data which is kerri's data from the full data set
kerri_data <- filter(mydata, study == "kerri")
kerri_data

#' Check for duplicated observations (ones with same lat, long, timestamp,
#'  and individual identifier).
ind2<-kerri_data %>% dplyr::select(long, lat, id) %>%
  duplicated 
sum(ind2) 
# remove them 
kerri_data$dups <- ind2
kerri_data <- filter(kerri_data,dups=="FALSE")
kerri_data

# set the time column
levels(factor(kerri_data$id))
# can look at an individual level with 
filter(kerri_data,id=="AG330")

#' ---
#' Some of Kerri's tags were used on more than one bird
#' doubles <-c("AM234", "AG382", "AM89", "AM88", "AM87")
#' e.g. #' AG382 deployments Adult	12/04/2010 & Juvenile	24/06/2010
#' I remove them for now as the tracks don't seem to correspond to changes in position

#' ---
#' Some data are GMT some are GMT + 2, some are unknown
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

#' ---
#' Date format for these is month-day-year 
#' "AM222" "AM226" "AM227" "AM234" "AM235" "AM240" "AM264" "AM267" "AM272" "AM295"
#' 
#' Date format for these is day-month-year, no seconds 
#' 22959306, 27230695, 27233665, 33640, 33798, 5008, AG313, AG314, AG329, AG330, Ag331, AG332, AG349, AG350
#' AG351, AG352, AG353, AG356, AG382, AM220, AM233, AM86, AM87, AM88, AM89, Ingelheim, LFV_009, X009
#' 

group1 <- c("AM222", "AM226", "AM227", "AM235", "AM240", "AM264", "AM267", "AM272", "AM295")
temp1 <- kerri_data %>% filter(id %in% group1)
temp1$New_time<-parse_date_time(x=temp1$time,c("%m/%d/%Y %H:%M:%S"), tz = "africa/johannesburg")

group2 <- c("5008", "AG313", "AG314", "AG329", "AG330", "Ag331", "AG332", "AG349", "AG350", "AG351", 
            "AG352", "AG353", "AG356", "AM220", "AM233", "AM86")
temp2 <- kerri_data %>% filter(id %in% group2)
temp2$New_time<-parse_date_time(x=temp2$time,c("%d/%m/%Y %H:%M"), tz = "africa/johannesburg")

group3 <- c("Ingelheim", "22959306", "27230695", "27233665", "33640", "33798")
temp3 <- kerri_data %>% filter(id %in% group3)
temp3$New_time<-parse_date_time(x=temp3$time,c("%d/%m/%Y %H:%M"))

# stick them back together again
kerri_data <- full_join(temp1, temp2) %>%
  full_join(., temp3) 

# Some of Kerri's data is in reverse order of time
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
kerri_data <- dplyr::select(kerri_data, New_time,long,lat,id,species,study)
kerri_data <- rename(kerri_data, time = New_time)
kerri_data
tz(kerri_data$time)

#' Kerri's metadata shows there are some data at the start of the files that are recorded before the
#' tag was deployed on the bird, need to trim the start of these
#' # deployment dates
# "22959306"	"18/03/2016" same
# "27230695"	"13/02/2016" same
# "27233665"	"31/01/2016" same
# "33640"	  	"27/04/2015" same
# "33798"	  	"28/04/2015" same
# "5008"	  	"29/09/2016" fix
# "AG313"	  	"08/11/2009" fix
# "AG314"	    "08/11/2009" fix
# "AG329"	    "02/12/2009" fix
# "AG330"   	"02/12/2009" fix
# "AG332"	  	"21/11/2009" fix
# "AG349"	    "02/12/2009" fix
# "AG350"	  	"02/12/2009" fix
# "AG351"	  	"09/12/2009" fix
# "AG352"	  	"09/12/2009" fix
# "AG353"	  	"17/01/2010" fix
# "AG356"	  	"10/02/2013" fix #' looks like this date exceeds the tracking data dates
# "AM222"	  	"02/03/2007" fix
# "AM226"	  	"02/03/2007" fix
# "AM227"	  	"02/03/2007" fix
# "AM233"	  	"24/04/2006" fix
# "AM235"	  	"15/03/2007" fix
# "AM240"	  	"15/03/2007" fix
# "AM264"	  	"29/01/2008" fix
# "AM272"	  	"19/12/2007" fix
# "AM295"	  	"27/11/2007" fix
# "AM86"	  	"04/12/2005" fix
# "Ingelheim"	"19/03/2012" same
# "LFV009"		"02/04/2015"
# "X009"			"10/04/2015"

#' check the start dates by printing the first row of each bird id
first_row <- kerri_data[!duplicated(kerri_data$id),]
print(first_row, n = 30)

filter(kerri_data, id == "AG356")
tail(filter(kerri_data, id == "AM86"))

kerri_data <- kerri_data %>% filter(
               id == "22959306"  |
               id == "27230695"  |
               id == "27233665"  |
               id == "33640"  |
               id == "33798"  |
               id == "Ingelheim"  |
               id == "5008" & time >= as.Date("2016-09-29 00:00:00") | #
               id == "AG313" & time >= as.Date("2009-11-08 00:00:00")| #
               id == "AG314" & time >= as.Date("2009-11-08 00:00:00")| #
               id == "AG329" & time >= as.Date("2009-12-02 00:00:00")| #
               id == "AG330" & time >= as.Date("2009-12-02 00:00:00")| #
               id == "AG332" & time >= as.Date("2009-11-21 00:00:00")| #
               id == "AG349" & time >= as.Date("2009-12-02 00:00:00")| #
               id == "AG350" & time >= as.Date("2009-12-02 00:00:00")| #
               id == "AG351" & time >= as.Date("2009-12-09 00:00:00")| #
               id == "AG352" & time >= as.Date("2009-12-09 00:00:00")| #
               id == "AG353" & time >= as.Date("2010-01-17 00:00:00")| #
               id == "AG356" & time >= as.Date("2013-02-10 00:00:00")| #
               id == "AM222" & time >= as.Date("2007-03-02 00:00:00")| #
               id == "AM226" & time >= as.Date("2007-03-02 00:00:00")| #
               id == "AM227" & time >= as.Date("2007-03-02 00:00:00")| #
               id == "AM233" & time >= as.Date("2006-04-24 00:00:00")| #
               id == "AM235" & time >= as.Date("2007-03-15 00:00:00")| #
               id == "AM240" & time >= as.Date("2007-03-15 00:00:00")| #
               id == "AM264" & time >= as.Date("2009-01-29 00:00:00")| #
               id == "AM272" & time >= as.Date("2007-12-19 00:00:00")| #
               id == "AM295" & time >= as.Date("2007-11-27 00:00:00")| #
               id == "AM86" & time >= as.Date("2005-12-04 00:00:00"))  #

#' filter extreme data based on a speed threshold 
#' based on vmax which is km/hr
#' time needs to be labelled DateTime for these functions to work
library(SDLfilter)
names(kerri_data)[names(kerri_data) == 'time'] <- 'DateTime'
SDLfilterData<-ddfilter.speed(data.frame(kerri_data), vmax = 100, method = 1)
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


#' measure the time difference between points for each bird ID using dplyr
#' - Group your data by ID
#' - Compute time diffs between each timestamp in your group (the 1st time diff is NA)
#' - Create a new ID that counts no. of prior time gaps that are large (e.g. > 24 hours)
#' - Split the ID into newID by using an underscore separator

length(levels(as.factor(trk$id)))
#' need to add the arrange function here otherwise the order gets messed up
trk2 <- trk %>%
  group_by(id) %>%
  mutate(timeDiff = c(NA, difftime(tail(t_, -1), head(t_, -1), units = "hours"))) %>%
  mutate(newID = paste(id, cumsum(!is.na(timeDiff) &
                                    timeDiff > 24), sep = "_")) %>% arrange(id, t_) %>%
  ungroup()
head(trk2)
tail(trk2)

#' check the number of newIDs
levels(as.factor(trk2$newID))
length(levels(as.factor(trk2$newID)))

#' how long are the tracks now that some of them have been split
sapply(split(trk2$x_, trk2$newID), length)

#' create a trajectory object using adehabitatLT
trk_ltraj <-
  as.ltraj(xy = trk2[, c("x_", "y_")],
           date = trk2$t_,
           id = trk2$newID)

#' rediscretization of the trajectory
tstep <-
  14400 # time step we want for the rediscretization, in seconds, 14400 secs = 4 hours
newtr <- redisltraj(trk_ltraj, u = tstep, type = "time")
head(newtr[1])
head(newtr[5])
class(newtr)

#' convert to class data frame
trk3 <- ld(newtr)
head(trk3)
class(trk3$date)

#' we should group the IDs that were split if they had big gaps back together into their original ID structure
#' this involves accessing the name of the new ID that occurs before the underscore
trk3 <- separate(trk3,
                 col = id,
                 sep = "_",
                 into = c("ID", "NA"))
head(trk3)
levels(as.factor(trk3$ID))
length(levels(as.factor(trk3$ID)))

#' remove the resultant NA column that occurs after the split
trk3 <- dplyr::select(trk3, x, y, date, ID)
head(trk3)

#' turn it back into a trk
trk4 <-
  mk_track(
    trk3,
    .x = x,
    .y = y,
    .t = date,
    id = ID,
    crs = CRS(
      "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
    )
  )
trk4

trk4 <- trk4 %>% arrange(id)
#' export this regularised track
write.csv(x = trk4, file = "regularised/kerri_reg.csv", row.names = FALSE)

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
data_summary$study <- "Kerri"
data_summary

#' can export this data summary 
write.csv(data_summary, file="track_resolution_summary/kerri_data_summary.csv", row.names = FALSE)

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