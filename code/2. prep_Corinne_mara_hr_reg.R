# Corinne's Mara Vulture Tracking Dataset

library(lubridate)
library(SDLfilter)
library(amt)
library(sp)
library(janitor)
library(adehabitatLT)

# Corinne_mara

# select Corinne_mara data which is Corinne's Mara data from everything
Corinne_mara <- filter(mydata, study == "Corinne_mara")
Corinne_mara

#' Check for duplicated observations (ones with same lat, long, time,
#'  and individual identifier).
ind2 <- Corinne_mara %>% dplyr::select(long, lat, id) %>%
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
  dplyr::select(Corinne_mara, New_time, long, lat, id, species, study)
Corinne_mara <- rename(Corinne_mara, time = New_time)
Corinne_mara

#' remove underscore from an id
Corinne_mara$id <- str_replace_all(Corinne_mara$id, "[[:punct:]]", "")
levels(as.factor(Corinne_mara$id))

#' filter extreme data based on a speed threshold
#' based on vmax which is km/hr
#' time needs to be labelled DateTime for these functions to work
names(Corinne_mara)[names(Corinne_mara) == 'time'] <- 'DateTime'
SDLfilterData <-
  ddfilter.speed(data.frame(Corinne_mara), vmax = 100, method = 1)
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

#' measure the time difference between points for each bird ID using dplyr
#' - Group your data by ID
#' - Compute time diffs between each timestamp in your group (the 1st time diff is NA)
#' - Create a new ID that counts no. of prior time gaps that are large (e.g. > 24 hours)
#' - Split the ID into newID by using an underscore separator

length(levels(as.factor(trk$id)))
levels(as.factor(trk$id))

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
write.csv(x = trk4, file = "regularised/corinne_mara_reg.csv", row.names = FALSE)

#' Calculate home range size for data that is not regularised
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
data_summary$study <- "Corinne_mara"
data_summary

#' can export this data summary 
write.csv(data_summary, file="track_resolution_summary/Corinne_Mara_data_summary.csv", row.names = FALSE)