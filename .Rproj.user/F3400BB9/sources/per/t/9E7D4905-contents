#' Dynamic Brownian Bridge Models Using move Package
#' load the move package
require(move)

#' filter one of the tracks from the Swazi data set
move_test <- filter(swazi_data, id == "ID1")
#' take a sample of data so it runs
move_test <- head(move_test,500)

plot(move_test$long, move_test$lat)

#' transform into a move object
ss1 <-
  move(
    x = move_test$long,
    y = move_test$lat,
    time = move_test$time,
    proj = CRS("+proj=longlat +ellps=WGS84"),
    data = move_test,
    animal = "ID1"
  )

#' take a look
ss1
#' transform into a spatial points object
r <- spTransform(ss1, center = T)
r

ID1_dbbmm <- brownian.bridge.dyn(
  r,
  dimSize = 150,
  location.error = 0.0001,
  ext = 1,
#  time.step = 60,
  margin = 15
)


plot(ID1_dbbmm, xlab="location_long", ylab="location_lat")
lines(spTransform(ricky[1:500,], center=TRUE), col=3, lwd=2)
contour(ID1_dbbmm, levels=c(.5, .95), col=c(6,2), add=TRUE, lwd=2)

ID1_cont <- getVolumeUD(ID1_dbbmm)
ID1_cont <- ID1_cont<=.95
area <- sum(values(ID1_cont))
area
