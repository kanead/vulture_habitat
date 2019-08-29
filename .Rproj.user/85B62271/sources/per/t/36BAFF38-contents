#' Plot an animation of the data
#' Repeat the start of the edits for the Swazi data for instance

#' Swaziland Vulture Tracking Dataset

library(lubridate)
library(SDLfilter)
library(amt)
library(sp)
library(ggplot2)
library(moveVis)
library(move)
library(magrittr)
library(raster)

#' swazi

#' select swazi data which is the data we tracked in Swaziland
swazi_data <- filter(mydata, study == "swazi")
swazi_data

#' Check for duplicated observations (ones with same lat, long, timestamp,
#'  and individual identifier).
ind2 <- swazi_data %>% dplyr::select(time, long, lat, id) %>%
  duplicated
sum(ind2)
# remove them
swazi_data$dups <- ind2
swazi_data <- filter(swazi_data, dups == "FALSE")
swazi_data

# set the time column
levels(factor(swazi_data$id))
# can look at an individual level with
(filter(swazi_data, id == "ID1"))

# all of the data is in the format of day-month-year
swazi_data$New_time <-
  parse_date_time(x = swazi_data$time, c("%d/%m/%Y %H:%M"))

# keep only the new time data
swazi_data <-
  dplyr::select(swazi_data, New_time, long, lat, id, species, study)
swazi_data <- rename(swazi_data, time = New_time)
swazi_data

# check the minimum time and the maximum time
min_time <- swazi_data %>% group_by(id) %>% slice(which.min(time))
data.frame(min_time)

max_time <- swazi_data %>% group_by(id) %>% slice(which.max(time))
data.frame(max_time)

#' filter extreme data based on a speed threshold
#' based on vmax which is km/hr
#' time needs to be labelled DateTime for these functions to work
names(swazi_data)[names(swazi_data) == 'time'] <- 'DateTime'
SDLfilterData <-
  ddfilter.speed(data.frame(swazi_data), vmax = 60, method = 1)
length(SDLfilterData$DateTime)

#' rename everything as before
swazi_data <- SDLfilterData
names(swazi_data)[names(swazi_data) == 'DateTime'] <- 'time'

# try the amt package
trk <-
  mk_track(
    swazi_data,
    .x = long,
    .y = lat,
    .t = time,
    id = id,
    species = species,
    crs = CRS("+init=epsg:4326")
  )

# Now it is easy to calculate day/night with either movement track
trk <- trk %>% time_of_day()
head(trk)

#' subset a sample of the data using head for an example
sample_data <- dplyr::filter(trk, id == "ID1" & tod_ == "day")

#' transform the data into a move object
sample_data <-
  move(
    x = sample_data$x_,
    y = sample_data$y_,
    time = sample_data$t_,
    proj = CRS("+proj=longlat +ellps=WGS84")
  )

#'
#' ### Make frames of tracking data for animation.
#' Evaluate tracking data for sampling rates if unknown. Use this information to help decide
#' the temporal resolution at which to align the data for the animation.
unique(timestamps(sample_data))
timeLag(sample_data, unit = "mins")

#' pick specific rows
sample_data <- slice(data.frame(sample_data), 235:435)

#' turn it back into a move object
sample_data <-
  move(
    x = sample_data$x,
    y = sample_data$y,
    time = sample_data$time,
    proj = CRS("+proj=longlat +ellps=WGS84")
  )

#' Align tracking data to uniform temporal resolution for interpretation by frames_spatial.
vultures <-
  align_move(sample_data,
             res = 1,
             unit = "mins",
             spaceMethod = "greatcircle")

#'
#' ## Create animation with static web basemap.
#' Create map frames for animation (see p.25 of MoveVis manual). Note that equidistant = TRUE
#' does not mean that the map will be displayed in an equidistant projection (i.e. that
#' preserves distances). Instead it causes frames_spatial to stretch the displayed area to an
#' square extent. If equidistant = FALSE, the extent is displayed in the projection-native axis
#' ratio.

frames <-
  frames_spatial(
    vultures,
    map_service = "osm",
    map_type = "watercolor",
    equidistant = FALSE,
    path_legend = T,
    path_legend_title = "Vulture",
    alpha = 0.5
  )

#' Have a look at one of the frames.
frames[[1]]

#' Note: Working with the frames I sometimes get a message "Error in grid.Call....polygon edge
#' not found" but if I rerun the same code the command succeeds.

#' Add labels and a progress bar.
frames.l <-
  add_labels(frames, x = "Longitude", y = "Latitude") %>% # axis labels
  add_progress() %>% # progress bar
  add_scalebar() %>% # scale bar
  add_northarrow() %>% # north arrow
  add_timestamps(vultures, type = "label") # timestamps

#' Record an animation using defaults shown in in moveVis manual p.18.
animate_frames(
  frames.l,
  "animations/swazi_vulture_test.gif",
  fps = 25,
  width = 500,
  height = 800,
  res = 100,
  display = TRUE,
  overwrite = TRUE,
  verbose = TRUE
)

#' Running animate_frames takes a while, you can go grab a coffee. When it finishes, check out
#' your gif. That was easy, thanks moveVis!
