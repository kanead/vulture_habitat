#' How many countries does the bird track overlap with?
#' Swaziland Vulture Tracking Dataset 
#' load packages
library(rworldmap)
library(sp)
library(lubridate)
library(SDLfilter)
library(amt)

# swazi

# select swazi data which is the data we tracked in Swaziland
swazi_data <- filter(mydata, study == "swazi")
swazi_data

#' Check for duplicated observations (ones with same lat, long, timestamp,
#'  and individual identifier).
ind2<-swazi_data %>% select(time, long, lat, id) %>%
  duplicated 
sum(ind2) 
# remove them 
swazi_data$dups <- ind2
swazi_data <- filter(swazi_data,dups=="FALSE")
swazi_data

# set the time column
levels(factor(swazi_data$id))
# can look at an individual level with 
(filter(swazi_data,id=="ID1"))

# all of the data is in the format of day-month-year 
swazi_data$New_time<-parse_date_time(x=swazi_data$time,c("%d/%m/%Y %H:%M"))

# keep only the new time data
swazi_data <- select(swazi_data, New_time,long,lat,id,species,study)
swazi_data <- rename(swazi_data, time = New_time)
swazi_data

#' estimate vmax for threshold speed 
#' names(swazi_data)[names(swazi_data) == 'time'] <- 'DateTime'
#' speed.est.data <- swazi_data %>% filter(id == "ID2") %>%  select(id,DateTime,lat,long)
#' speed.est.data$qi = 5
#' est.vmax(sdata = data.frame(speed.est.data))

#' filter extreme data based on a speed threshold 
#' based on vmax which is km/hr
#' time needs to be labelled DateTime for these functions to work
names(swazi_data)[names(swazi_data) == 'time'] <- 'DateTime'
SDLfilterData<-ddfilter.speed(data.frame(swazi_data), vmax = 70, method = 1)
length(SDLfilterData$DateTime)

#' rename everything as before
swazi_data <- SDLfilterData
names(swazi_data)[names(swazi_data) == 'DateTime'] <- 'time'

#' get the map of the world 
m = getMap()

coordinates(swazi_data) = ~long+lat
proj4string(swazi_data) = proj4string(m)

#' Now I can extract any of the columns from the map data corresponding to each point with over:
  
over(swazi_data,m)$NAME
levels(over(swazi_data,m)$NAME)

#' I can add the NAME to the source points:
swazi_data$country = over(swazi_data,m)$NAME
swazi_data$country <- droplevels(swazi_data$country)
head(swazi_data$country)
countries <- levels(swazi_data$country)
write.csv(countries, file = "results/swazi_countries_overlap.csv", row.names = F)
