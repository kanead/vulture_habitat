#' Dynamic Brownian Bridge Models Using move Package
#' load the move package
require(move)


#' transform the CRS
#' try the amt package
trk <-
  mk_track(
    swazi_data,
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
    window.size = 101,
    margin = 7,
    raster = 100
  )

writeRaster(dbbmm, filename='brownian_bridges/swazi_dbbm.tif', overwrite=TRUE)

plot(dbbmm)
contour(dbbmm, add=T, levels=c(.5,.95))
raster2contour(dbbmm)
