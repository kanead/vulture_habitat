#' recurse package to analyze revisitations in animal movement data
require(recurse)
require(scales)
require(sp)
require(move)

#' load in the regularised tracks
mydata <- read_csv("regularised/swazi_reg.csv", col_names = TRUE)
head(mydata)

#' extract one bird
mydata <- mydata %>% filter(id == "ID2") 

#' turn into a move object
mydata <- move(x=mydata$x_, y=mydata$y_, 
             time=as.POSIXct(mydata$t_, format="%Y-%m-%d %H:%M:%S", tz="UTC"), 
             data=mydata, proj=CRS("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"), 
             animal="ID1", sensor="GPS")

mydata

vulturevisit = getRecursions(mydata, 100) 

par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
plot(vulturevisit, mydata, legendPos = c(550000, -3000000))

hist(vulturevisit$revisits, breaks = 20, main = "", xlab = "Revisits (radius = 100)")
summary(vulturevisit$revisits)
head(vulturevisit$revisitStats)
