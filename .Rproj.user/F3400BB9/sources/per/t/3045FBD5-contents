#' Compare home range analysis from amt package with adehabitatHR
#' Load in the data from 1_load_data.R
#' Check with the Morgan's data
#' https://mltconsecol.github.io/TU_LandscapeAnalysis_Documents/Assignments_web/Assignment09_AnimalMovement.pdf
#' Morgan Pfeiffer Vulture Tracking Dataset

library(lubridate)
library(SDLfilter)
library(amt)
library(sp)
library(adehabitatHR)
library(rgdal)
library(raster)

# select Morgan's data
morgan_data <- filter(mydata, study == "pfeiffer")
morgan_data

# drop missing rows
morgan_data <- morgan_data %>% drop_na

#' Check for duplicated observations (ones with same lat, long, timestamp,
#'  and individual identifier).
ind2 <- morgan_data %>% dplyr::select(long, lat, id) %>%
  duplicated
sum(ind2)
# remove them
morgan_data$dups <- ind2
morgan_data <- filter(morgan_data, dups == "FALSE")
morgan_data

#' set the time column
#' some are in day/month/year format e.g. X016_Complete; X020_Final; X021_Final; X022_Complete; X032_Final; X033_Complete;
#' some are in month/day/year format e.g. X023; X027; X042; X050; X051; X052; X053; X055; X056; X057; X071
#' Morgan said X051 never transmitted data so I remove that here
levels(factor(morgan_data$id))
temp1 <- filter(
  morgan_data,
  id == "X016_Complete" |
    id == "X021_Final" |
    id == "X020_Final" |
    id == "X021_Final" |
    id == "X022_Complete" |
    id == "X032_Final" |
    id == "X033_Complete"
)
tail(temp1)
head(temp1)
temp1
temp1$New_time <- parse_date_time(x = temp1$time, c("%d/%m/%Y %H:%M"))
tail(temp1)

temp2 <- filter(
  morgan_data,
  id == "X023" |
    id == "X027" |
    id == "X042" |
    id == "X050" |
    id == "X052" |
    id == "X053" |
    id == "X055" |
    id == "X056" |
    id == "X057" |
    id == "X071"
)
tail(temp2)
head(temp2)
temp2
temp2$New_time <- parse_date_time(x = temp2$time, c("%m/%d/%Y %H:%M"))
tail(temp2)

# stick them back together again
morgan_data <- full_join(temp1, temp2)
morgan_data

# Morgan's data is in reverse order of time
# sort by the bird ID and reverse the order
morgan_data <- morgan_data %>% group_by(id)  %>%
  arrange(New_time, .by_group = TRUE)
morgan_data

# keep only the new time data
morgan_data <-
  dplyr::select(morgan_data, New_time, long, lat, id, species, study)
morgan_data <- rename(morgan_data, time = New_time)
#' The time zone should be UTC +2 (South African Standard Time)
morgan_data$time <- force_tz(morgan_data$time, "africa/johannesburg")

#' filter extreme data based on a speed threshold
#' based on vmax which is km/hr
#' time needs to be labelled DateTime for these functions to work
library(SDLfilter)
names(morgan_data)[names(morgan_data) == 'time'] <- 'DateTime'
SDLfilterData <-
  ddfilter.speed(data.frame(morgan_data), vmax = 70, method = 1)
length(SDLfilterData$DateTime)

#' rename everything as before
morgan_data <- SDLfilterData
names(morgan_data)[names(morgan_data) == 'DateTime'] <- 'time'
morgan_data <- dplyr::select(morgan_data, id, long, lat, time)

#' Prepare the data for adehabitatHR
#' There are three main arguments we are specifying:
#' The Coordinates (as longitude and latitude),
#' the data that will go in the table (morgan.spdf), and the projection information

morgan.spdf <-
  SpatialPointsDataFrame(
    coords = as.data.frame(cbind(morgan_data$long,
                                 morgan_data$lat)),
    data = morgan_data,
    proj4string =
      CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  )

#' The data are in a degree-based coordinate system. One last thing we’ll have to do is transform them to
#' a meter-based coordinate system, for purposes of area calculations and such with home ranges

morgan.spdf <- spTransform(
  morgan.spdf,
  CRS(
    "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  )
)

#' You can plot the points as you would any other Spatial object
#' it might take a minute as there are a lot of points
plot(morgan.spdf)

# First: 95% MCP
morgan.mcp <- mcp(morgan.spdf[, 1],
                  percent = 95,
                  unin = "m",
                  unout = "km2")
morgan.mcp #' values match amt
plot(morgan.mcp, col = c(1:length(levels(as.factor(morgan_data$id))), axes = TRUE))

#' Kernel Density Estimate
morgan.Khref <- kernelUD(morgan.spdf[, 1], h = "href")

#' # You can plot the KDE for each individual separately:
image(morgan.Khref)

#' The values of the smoothing parameters are stored in the slot "h" of each
#' element of the list.
morgan.Khref[[1]]@h
morgan.Khref[[2]]@h

#' measure the size of the home range using this kernel estimator
morgan.Khref.poly <- getverticeshr(morgan.Khref, percent = 95)
print(morgan.Khref.poly)  # returns the area of each polygon

#' calculate the home range size step wise up to 95%
ii <- kernel.area(morgan.Khref, percent = seq(50, 95, by = 5))
ii # values don't match amt

#' Considering Paths of Animal Movement and Dealing with Temporal Autocorrelation (Brownian Bridge Movement Model)
morgan.traj <-
  as.ltraj(coordinates(morgan.spdf),
           date = morgan.spdf$time
           ,
           id = morgan.spdf$id)

#' View what this looks like
morgan.traj

#' You can plot the trajectory for each individual - the numbers correspond to the ID in the zeb.traj object above
plot(morgan.traj[1])
plot(morgan.traj[2])

#' The Brownian movement model requires input of some parameters.
#' • sig1: a first smoothing parameter related to the speed of animals;
#' • sig2: a second smoothing parameter related to imprecision of the location data.
#' The second may be known or estimated (it is not available in the metadata, but for purposes of this lab,
#'                                    we’ll estimate 100 meters). The first, however, can be directly estimated from the data using a maximum
#' likelihood estimate with the ‘liker’ function. This requires that we provide a range of potential estimates for
#' sig1, ‘rangesig1’.

likmorgan <- liker(morgan.traj, sig2 = 100, rangesig1 = c(1, 200))

#' We can now model the home ranges using the Brownian Bridge Movement Model, using the respective sig1
#' values for each individual. The function for this is ‘kernelbb’. We will enter the sig1 values as a list after the
#' argument ‘sig1’:
#' This works on all individuals at once
morgan.bb <- kernelbb(morgan.traj,
                      sig1 = c(116.3,107.4,113.5,138.8,
                               109.0,57.6,73.3,100.6,
                               66.1,18.5,99.6,117.1,
                               130.7,99.0,1,103.6),
                      sig2 = 100)

# Again, you can plot the modeled home range as with the Kernel Density
# Estimates.
image(morgan.bb)

# And you can make it into it's own raster object, to write it to an
# external file, etc.
X027 <- (raster(as(morgan.bb$X027, "SpatialPixelsDataFrame")))

# Plot the individual raster; you might need to close out of the previous
# plot for this to appear appropriately
plot(X027)
# Extract the points for only individual X027
X027 <- morgan.spdf[morgan.spdf$id == "X027",]

# Plot those points on the raster
plot(X027, add = TRUE, cex = 0.01)

#' Get boundaries of 95% probability surface for a home range estimate
morganBB.95 <- getverticeshr(morgan.bb, 95, unout = "km2")

# Plot the bounds for the first individual (X027)
plot(morganBB.95[6,],
     border = "red",
     lwd = 2,
     add = TRUE)

#' As with the prevous home range estimators, you can also extract the area of the specified home range for the
#' Brownian Bridge Movement Model based at the desired level (we will use the 95% level again):
morganBB.95@data
