#' Dynamic Brownian Bridge Models Using move Package
#' load the move package
library(move)
library(ggmap)

#' transform the CRS
#' try the amt package
trk <-
  mk_track(
    mend_data,
    .x = long,
    .y = lat,
    .t = time,
    id = id,
    species = species,
    crs = CRS("+init=epsg:4326")
  )  %>%
  transform_coords(
    sp::CRS(
      #' we can transform the CRS of the data to an equal area projection
      #' https://epsg.io/102022
      "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
    )
  )

track <- data.frame(trk)
track <- arrange(track, id)
#' select one individual because the vector size is too big
track <- filter(track, id == "WBV1__44782")
#' create a move object 
loc <-
  move(
    x = track$x_,
    y = track$y_,
    time = track$t_,
    proj = CRS( "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),
    data = track,
    animal = track$id
  )

#' Now create a dBBMM object
dbbmm <-
  brownian.bridge.dyn(
    object = loc,
    location.error = 18,
    window.size = 31,
    margin = 15,
    raster = 100
  )

writeRaster(dbbmm, filename='brownian_bridges/mend_data_WBV1__44782.tif', overwrite=TRUE)

plot(dbbmm)
contour(dbbmm, add=T, levels=c(.5,.95))
raster2contour(dbbmm)
show(dbbmm)

bbmm.contour = data.frame(x = dbbmm$x, y = dbbmm$y, probability = dbbmm$probability)


#' compare dbmm plot to tracks
#' We can map the data
#' turn back to lat long
trk_map <-
  mk_track(
    mend_data,
    .x = long,
    .y = lat,
    .t = time,
    id = id,
    species = species,
    crs = CRS("+init=epsg:4326")
  )

trk_map <- filter(trk_map, id == "WBV1__44782")

#' plot all of the data on the one graph
qmplot(x_,
       y_,
       data = trk_map,
       maptype = "toner-lite",
       colour = id, 
       legend = "none",
       xlab = "longitude", 
       ylab = "latitude")


bb.95 <- getverticeshr(tata, percent = 95)
bb.95

