# Corinne Kendall's Tanzania Vulture Tracking Dataset

library(lubridate)
library(SDLfilter)
library(amt)
library(sp)

# ck_tanz

# select ck_tanz data which is Corinne's data from everything
ck_tanz_data <- filter(mydata, study == "ck_tanz")
ck_tanz_data

#' Check for duplicated observations (ones with same lat, long, time,
#'  and individual identifier).
ind2 <- ck_tanz_data %>% select(long, lat, id) %>%
  duplicated
sum(ind2)
# remove them
ck_tanz_data$dups <- ind2
ck_tanz_data <- filter(ck_tanz_data, dups == "FALSE")
ck_tanz_data

# set the time column
levels(factor(ck_tanz_data$id))
# can look at an individual level with
(filter(ck_tanz_data, id == "#109018542"))

#' all of the data is in the format of day-month-year
#' time zone is UTC by default
#' raw data is 2 hours off the real time
#' CK said the recorder started at 6am with hourly points
#' including a relocation at midnight

ck_tanz_data$New_time <-
  parse_date_time(x = ck_tanz_data$time, c("%d/%m/%Y %H:%M"), tz = "africa/dar_es_salaam") + hours(2)

# keep only the new time data
ck_tanz_data <-
  select(ck_tanz_data, New_time, long, lat, id, species, study)
ck_tanz_data <- rename(ck_tanz_data, time = New_time)
ck_tanz_data

#' filter extreme data based on a speed threshold
#' based on vmax which is km/hr
#' time needs to be labelled DateTime for these functions to work
names(ck_tanz_data)[names(ck_tanz_data) == 'time'] <- 'DateTime'
SDLfilterData <-
  ddfilter.speed(data.frame(ck_tanz_data), vmax = 70, method = 1)
length(SDLfilterData$DateTime)

#' rename everything as before
ck_tanz_data <- SDLfilterData
names(ck_tanz_data)[names(ck_tanz_data) == 'DateTime'] <- 'time'

# check the minimum time and the maximum time
min_time <- ck_tanz_data %>% group_by(id) %>% slice(which.min(time))
data.frame(min_time)

max_time <- ck_tanz_data %>% group_by(id) %>% slice(which.max(time))
data.frame(max_time)

#' determine the length of time each bird was tracked for
duration <-difftime(max_time$time, min_time$time, units = "days"); duration

#' #199122325 has a data point that occurs over water, the filter doesn't delete this value 
test <- ck_tanz_data %>% filter(id == "#199122325")
plot(test$long, test$lat)

ck_tanz_data <- ck_tanz_data  %>%
  filter(!((id == "#199122325" & lat < -10)))

# try the amt package
trk <-
  mk_track(
    ck_tanz_data,
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
data_summary$study <- "CK"
data_summary

#' can export this data summary 
#' write.csv(data_summary, file="track_resolution_summary/ck_data_summary.csv", row.names = FALSE)

#' We can map the data
#' turn back to lat long
trk_map <-
  mk_track(
    ck_tanz_data,
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

#' Save the class here (and apply it later after adding columns to the
#' object)
trk.class <- class(trk)

# nest by id
nesttrk <- trk %>% nest(-id)
nesttrk

#' We can add a columns to each nested column of data using purrr::map
trk <- trk %>% nest(-id) %>%
  mutate(
    dir_abs = map(
      data,
      ~ direction_abs(., full_circle = TRUE, zero = "N")
      %>% as_degree()
    ) ,
    dir_rel = map(data, ~ direction_rel(.)
                  %>% as_degree()),
    sl = map(data, step_lengths),
    nsd_ = map(data, nsd)
  ) %>% unnest()


#' Now, calculate month, year, hour, week of each observation and append these to the dataset
#' Unlike the movement charactersitics, these calculations can be done all at once,
#' since they do not utilize successive observations (like step lengths and turn angles do).
trk <- trk %>%
  mutate(
    week = week(t_),
    month = month(t_, label = TRUE),
    year = year(t_),
    hour = hour(t_)
  )


#' Now, we need to again tell R that this is a track (rather
#' than just a data frame)
class(trk)
class(trk) <- trk.class

#' Lets take a look at what we created
trk <- trk %>% group_by(id)
trk

#' look at net-squared displacement
ggplot(trk, aes(x = t_, y = nsd_)) + geom_point() +
  facet_wrap(~ id, scales = "free")

#' some data points look a little off
#' we can identify them to investiage further and remove them
#' if needs be
filter(trk, id == "#199122325" & nsd_ < 1e+11)

trk <- trk  %>%
  filter(!((id == "#199122325" & nsd_ > 1e+11)))

#' plot it again
ggplot(trk, aes(x = t_, y = nsd_)) + geom_point() +
  facet_wrap(~ id, scales = "free")

#' export the plot
ck_tanz_net_disp <-
  ggplot(trk, aes(x = t_, y = nsd_)) + geom_point() +
  facet_wrap(~ id, scales = "free")

# ggsave("plots/ck_tanz_net_disp.pdf")
# ggsave("plots/ck_tanz_net_disp.png")

#' ### Absolute angles (for each movement) relative to North
#' We could use a rose diagram (below) to depict the distribution of angles.
#+fig.height=12, fig.width=12
ggplot(trk, aes(x = dir_abs, y = ..density..)) +
  geom_histogram(breaks = seq(0, 360, by = 20)) +
  coord_polar(start = 0) + theme_minimal() +
  scale_fill_brewer() + ylab("Density") + ggtitle("Angles Direct") +
  scale_x_continuous(
    "",
    limits = c(0, 360),
    breaks = seq(0, 360, by = 20),
    labels = seq(0, 360, by = 20)
  ) +
  facet_wrap( ~ id)

#' ### Turning angles
#'
#' Note: a 0 indicates the animal continued to move in a straight line, a 180
#' indicates the animal turned around (but note, resting + measurement error often can
#' make it look like the animal turned around).
#+fig.height=12, fig.width=12
ggplot(trk, aes(x = dir_rel, y = ..density..)) +
  geom_histogram(breaks = seq(-180, 180, by = 20)) +
  coord_polar(start = 0) + theme_minimal() +
  scale_fill_brewer() + ylab("Density") + ggtitle("Angles Direct") +
  scale_x_continuous(
    "",
    limits = c(-180, 180),
    breaks = seq(-180, 180, by = 20),
    labels = seq(-180, 180, by = 20)
  ) +
  facet_wrap(~ id)

#' ### Turning angles as histograms
#+fig.height=12, fig.width=12
ggplot(trk, aes(x = dir_rel)) +
  geom_histogram(breaks = seq(-180, 180, by = 20)) +
  theme_minimal() +
  scale_fill_brewer() + ylab("Count") + ggtitle("Angles Relative") +
  scale_x_continuous(
    "",
    limits = c(-180, 180),
    breaks = seq(-180, 180, by = 20),
    labels = seq(-180, 180, by = 20)
  ) + facet_wrap( ~ id, scales = "free")

#' We can also use lat, long, which will allow us to determine
#' time of day
trk_ll <- mk_track(
  ck_tanz_data,
  .x = long,
  .y = lat,
  .t = time,
  id = id,
  crs = CRS("+init=epsg:4326")
)

# Now it is easy to calculate day/night with either movement track
trk_ll <- trk_ll %>% time_of_day()

#' can verify with this site
#' https://keisan.casio.com/exec/system/1224686065
#' amt says 2015-09-15 06:00:00 35.02417 -7.78217 is at night
#' website confirms this with sunrise at 06:33

#' We can add a columns to each nested column of data using purrr::map
trk_ll <- trk_ll %>% nest(-id) %>%
  mutate(
    dir_abs = map(data, direction_abs, full_circle = TRUE, zero = "N"),
    dir_rel = map(data, direction_rel),
    sl = map(data, step_lengths),
    nsd_ = map(data, nsd)
  ) %>% unnest()


#+fig.height=12, fig.width=12, warning=FALSE, message=FALSE
ggplot(trk_ll, aes(x = tod_, y = log(sl))) +
  geom_boxplot() + geom_smooth() + facet_wrap( ~ id)

#' We can map the data
#' turn back to lat long
# try the amt package
trk_map <-
  mk_track(
    ck_tanz_data,
    .x = long,
    .y = lat,
    .t = time,
    id = id,
    species = species,
    crs = CRS("+init=epsg:4326")
  )

#' remove the weird point from #199122325
trk_map <- trk_map  %>%
  filter(!((id == "#199122325" & y_ < -10)))

#' map the data
library(leaflet)
levels(as.factor(trk_map$id))
leaflet() %>% addTiles() %>%
  addCircleMarkers(
    data = filter(trk_map, id == "#199122325"),
    lat = ~ y_,
    lng = ~ x_,
    color = "blue"
  )

#' plot all data on one map
ggplot(trk_map, aes(x = x_, y = y_, color = as.factor(id))) +
  geom_point() + xlab ("longitude") + ylab ("latitude")

library(ggmap)

qmplot(x_,
       y_,
       data = trk_map,
       maptype = "toner-lite",
       color = I("red"))

#' Calculate home range size for data that is not regularised

#' convert our regularised data back into a trk type data frame
mcps <- trk %>% nest(-id) %>%
  mutate(mcparea = map(data, ~ hr_mcp(., levels = c(0.95)) %>% hr_area)) %>%
  select(id, mcparea) %>% unnest()

mcps$area <- mcps$area / 1000000
mcps %>% arrange(id)

#+fig.height=12, fig.width=12, warning=FALSE, message=FALSE
ggplot(mcps, aes(x = id, y = area)) + geom_point() +
  geom_smooth()

#' Same for KDE
kde <- trk %>% nest(-id) %>%
  mutate(kdearea = map(data, ~ hr_kde(., levels = c(0.95)) %>% hr_area)) %>%
  select(id, kdearea) %>% unnest()

kde$kdearea <-  kde$kdearea / 1000000
kde %>% arrange(id)


#+fig.height=12, fig.width=12, warning=FALSE, message=FALSE
ggplot(kde, aes(x = id, y = kdearea)) + geom_point() +
  geom_smooth()

#' ## SSF prep
#'
#' SSFs assume that data have been collected at regular time intervals.
#' We can use the track_resample function to regularize the trajectory so that
#' all points are located within some tolerence of each other in time. To figure
#' out a meaningful tolerance range, we should calculate time differences between
#' locations & look at as a function of individual.
(timestats <-
    trk %>% nest(-id) %>% mutate(sr = map(data, summarize_sampling_rate)) %>%
    dplyr::select(id, sr) %>% unnest)

#' add species names to the time intervals
timestats <- timestats %>%
  left_join(trk %>%
              distinct(id, species), by = 'id')

#' add the study name
timestats$study <- "ck_tanz"
head(timestats)

#' make sure it matches up
filter(trk, id == "#333860073") # hv
filter(trk, id == "#333860224") # hv
filter(trk, id == "#333858323") # wh
filter(trk, id == "#109018542") # wb

#' export the data
#' write.csv(timestats, file = "track_resolution_summary/ck_tanz_res.csv", row.names = FALSE)

#' let's select just the AWBVs
trk <- filter(trk, species == "wb")
trk
levels(as.factor(trk$species))

#' need to run timestats again for the subsetted dataframe so that the IDs match up below
#' because we removed everything that wasn't a white back
(timestats <-
    trk %>% nest(-id) %>% mutate(sr = map(data, summarize_sampling_rate)) %>%
    dplyr::select(id, sr) %>% unnest)

#' Time intervals range depending on the individual.
#' Lets add on the time difference to each obs.
trk <-
  trk %>% group_by(id) %>% mutate(dt_ = t_ - lag(t_, default = NA))
trk

#'  Loop over the individuals and do the following:
#'
#' - Regularize trajectories using an appropriate time window (see e.g., below)
#' - calculate new dt values
#' - Create bursts using individual-specific time intervals
#' - Generate random steps within each burst
#'
#' The random steps are generated using the following approach:
#'
#' 1. Fit a gamma distribution to step lenghts
#' 2. Fit a von mises distribution to turn angles
#' 3. Use these distribution to draw new turns and step lengths, form new simulated steps
#' and generate random x,y values.
#'

#+warning=FALSE
ssfdat <- NULL
temptrk <- with(trk, track(
  x = x_,
  y = y_,
  t = t_,
  id = id
))
uid <- unique(trk$id) # individual identifiers
luid <- length(uid) # number of unique individuals
for (i in 1:luid) {
  # Subset individuals & regularize track
  temp <- temptrk %>% filter(id == uid[i]) %>%
    track_resample(rate = hours(1),
                   tolerance = minutes(10)) #' tolerance is 10 minutes
  
  # Get rid of any bursts without at least 3 points
  temp <- filter_min_n_burst(temp, 3)
  
  # burst steps
  stepstemp <- steps_by_burst(temp)
  
  # create random steps using fitted gamma and von mises distributions and append
  rnd_stps <- stepstemp %>%  random_steps(n = 10)
  
  # append id
  rnd_stps <- rnd_stps %>% mutate(id = uid[i])
  
  # append new data to data from other individuals
  ssfdat <- rbind(rnd_stps, ssfdat)
}
ssfdat <- as_tibble(ssfdat)
ssfdat

#' ## Write out data for further annotating
#'
#' Need to rename variables so everything is in the format Movebank requires for annotation of generic time-location
#' records (see https://www.movebank.org/node/6608#envdata_generic_request). This means, we need the following variables:
#'
#' - location-lat (perhaps with addition of Easting/Northing in UTMs)
#' - location-long (perhaps with addition of Easting/Northing in UTMs)
#' - timestamp (in Movebank format)
#'
#' Need to project to lat/long, while also keeping lat/long. Then rename
#' variables and write out the data sets. With the SSFs, we have the extra complication of
#' having a time and location at both the start and end of the step.
#'
#' For the time being, we will assume we want to annotate variables at the end of the step
#' but use the starting point of the step as the timestamp.
#'
#' You could also calculate the midpoint of the timestep like this:
#' data$timestamp.midpoint <- begintime + (endtime-begintime)/2
#'
#' we want the x2_ and y2_ columns for Movebank
head(ssfdat)
ncol(ssfdat)
ssfdat2 <- SpatialPointsDataFrame(
  ssfdat[, c("x2_", "y2_")],
  ssfdat,
  proj4string = CRS(
    "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  )
)

#' converts the end point of each step into long and lat
ssf.df <-
  data.frame(spTransform(ssfdat2, CRS("+proj=longlat +datum=WGS84")))

names(ssf.df)[names(ssf.df) == 'id'] <-
  'individual.local.identifier'
names(ssf.df)[names(ssf.df) == 'x2_.1'] <- 'location-long'
names(ssf.df)[names(ssf.df) == 'y2_.1'] <- 'location-lat'
head(ssf.df)

#' can plot a track to make sure it looks ok after all the converting
leaflet() %>% addTiles() %>%
  addCircleMarkers(
    data = filter(
      ssf.df,
      individual.local.identifier == "#199122325" & case_ == "TRUE"
    ),
    lat = ~ ssf.df[, 15],
    lng = ~ ssf.df[, 14],
    color = "blue"
  )

ssf.df$timestamp <- ssf.df$t1_
ssf.df %>% select('location-lat', x1_, x2_, y1_, y2_, 'location-long') %>% head


#' These points then need to be annotated prior to fitting ssfs.
#' Can subset to certain essential columns so as take up less space, making it easier to annotate (and also possible to upload to github)
ssf.df.out <-
  ssf.df %>% select(
    "timestamp",
    "location-long",
    "location-lat",
    "individual.local.identifier",
    "case_"
  )
head(ssf.df.out)
write.csv(ssf.df.out, file = "for_annotation/CorinneSSFannotate.csv", row.names = FALSE)

#' we can export the whole file with all of the extra columns we added
head(ssf.df)
write.csv(ssf.df, file = "amt_tracks/CorinneSSFAll.csv", row.names = FALSE)
