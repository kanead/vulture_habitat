#' Compare home range analysis from amt package with adehabitatHR
#' Load in the data from 1_load_data.R
#' Check with the Swaziland data
#' https://mltconsecol.github.io/TU_LandscapeAnalysis_Documents/Assignments_web/Assignment09_AnimalMovement.pdf
#' Swaziland Vulture Tracking Dataset

library(lubridate)
library(SDLfilter)
library(amt)
library(sp)
library(adehabitatHR)
library(rgdal)
library(raster)
# swazi

# select swazi data which is the data we tracked in Swaziland
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

#' filter extreme data based on a speed threshold
#' based on vmax which is km/hr
#' time needs to be labelled DateTime for these functions to work
names(swazi_data)[names(swazi_data) == 'time'] <- 'DateTime'
SDLfilterData <-
  ddfilter.speed(data.frame(swazi_data), vmax = 70, method = 1)
length(SDLfilterData$DateTime)

#' rename everything as before
swazi_data <- SDLfilterData
names(swazi_data)[names(swazi_data) == 'DateTime'] <- 'time'
swazi_data <- dplyr::select(swazi_data, id, long, lat, time)

#' Prepare the data for adehabitatHR
#' There are three main arguments we are specifying:
#' The Coordinates (as longitude and latitude),
#' the data that will go in the table (swazi.spdf), and the projection information

swazi.spdf <-
  SpatialPointsDataFrame(
    coords = as.data.frame(cbind(swazi_data$long,
                                 swazi_data$lat)),
    data = swazi_data,
    proj4string =
      CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  )

#' The data are in a degree-based coordinate system. One last thing we’ll have to do is transform them to
#' a meter-based coordinate system, for purposes of area calculations and such with home ranges

swazi.spdf <- spTransform(
  swazi.spdf,
  CRS(
    "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  )
)

#' You can plot the points as you would any other Spatial object
#' it might take a minute as there are a lot of points
plot(swazi.spdf)

# First: 95% MCP
swazi.mcp <- mcp(swazi.spdf[, 1],
                 percent = 95,
                 unin = "m",
                 unout = "km2")
swazi.mcp #' values match amt
plot(swazi.mcp, col = c(1:2), axes = TRUE)

#' Kernel Density Estimate
swazi.Khref <- kernelUD(swazi.spdf[, 1], h = "href")

#' # You can plot the KDE for each individual separately:
image(swazi.Khref)

#' The values of the smoothing parameters are stored in the slot "h" of each
#' element of the list. 
swazi.Khref[[1]]@h
swazi.Khref[[2]]@h

#' measure the size of the home range using this kernel estimator
swazi.Khref.poly <- getverticeshr(swazi.Khref, percent = 95) 
print(swazi.Khref.poly)  # returns the area of each polygon

#' calculate the home range size step wise up to 95%
ii <- kernel.area(swazi.Khref, percent=seq(50, 95, by=5))
ii # values don't match amt

#' Considering Paths of Animal Movement and Dealing with Temporal Autocorrelation (Brownian Bridge Movement Model)
swazi.traj <- as.ltraj(coordinates(swazi.spdf), date=swazi.spdf$time
                     ,id=swazi.spdf$id)

#' View what this looks like
swazi.traj

#' You can plot the trajectory for each individual - the numbers correspond to the ID in the zeb.traj object above
plot(swazi.traj[1])
plot(swazi.traj[2])

#' The Brownian movement model requires input of some parameters.
#' • sig1: a first smoothing parameter related to the speed of animals;
#' • sig2: a second smoothing parameter related to imprecision of the location data.
#' The second may be known or estimated (it is not available in the metadata, but for purposes of this lab,
#'                                    we’ll estimate 100 meters). The first, however, can be directly estimated from the data using a maximum
#' likelihood estimate with the ‘liker’ function. This requires that we provide a range of potential estimates for
#' sig1, ‘rangesig1’.

likSwazi <- liker(swazi.traj, sig2 = 100, rangesig1 = c(1, 50))

#' We can now model the home ranges using the Brownian Bridge Movement Model, using the respective sig1
#' values for each individual. The function for this is ‘kernelbb’. We will enter the sig1 values as a list after the
#' argument ‘sig1’:
#' This works on all individuals at once
swazi.bb <- kernelbb(swazi.traj, sig1 = c(18.2162, 22.6306),
                   sig2 = 100)

# Again, you can plot the modeled home range as with the Kernel Density
# Estimates.
image(swazi.bb)

# And you can make it into it's own raster object, to write it to an
# external file, etc.
ID2 <- (raster(as(swazi.bb$ID1, "SpatialPixelsDataFrame")))

# Plot the individual raster; you might need to close out of the previous
# plot for this to appear appropriately
plot(ID2)
# Extract the points for only individual ID2
ID2 <- swazi.spdf[swazi.spdf$id == "ID2", ]

# Plot those points on the raster
plot(ID2, add = TRUE, cex = 0.01)

#' Get boundaries of 95% probability surface for a home range estimate
swaziBB.95 <- getverticeshr(swazi.bb, 95, unout = "km2")

# Plot the bounds for the first individual (ID2)
plot(swaziBB.95[2, ], border = "red", lwd = 2, add = TRUE)

#' As with the prevous home range estimators, you can also extract the area of the specified home range for the
#' Brownian Bridge Movement Model based at the desired level (we will use the 95% level again):
swaziBB.95@data
