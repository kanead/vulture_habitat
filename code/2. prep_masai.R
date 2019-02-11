# Masai Mara Vulture Tracking Dataset 

library(lubridate)
library(SDLfilter)
library(amt)
library(sp)

# mara

# select mara data which is Masai Mara data from everything
masai_data <- filter(mydata, study == "masai")
masai_data

#' Check for duplicated observations (ones with same lat, long, timestamp,
#'  and individual identifier).
ind2<-masai_data %>% select(long, lat, id) %>%
  duplicated 
sum(ind2) 
# remove them 
masai_data$dups <- ind2
masai_data <- filter(masai_data,dups=="FALSE")
masai_data

# set the time column
levels(factor(masai_data$id))
# can look at an individual level with 
(filter(masai_data,id=="171605"))

# all of the data is in the format of day-month-year 
masai_data$New_time<-parse_date_time(x=masai_data$time,c("%d/%m/%Y %H:%M"))

# keep only the new time data
masai_data <- select(masai_data, New_time,long,lat,id,species,study)
masai_data <- rename(masai_data, time = New_time)
masai_data

# check the minimum time and the maximum time
min_time <- masai_data %>% group_by(id) %>% slice(which.min(time))
data.frame(min_time)


max_time <- masai_data %>% group_by(id) %>% slice(which.max(time))
data.frame(max_time)


#' filter extreme data based on a speed threshold 
#' based on vmax which is km/hr
#' time needs to be labelled DateTime for these functions to work
library(SDLfilter)
names(masai_data)[names(masai_data) == 'time'] <- 'DateTime'
SDLfilterData<-ddfilter.speed(data.frame(masai_data), vmax = 60, method = 1)
length(SDLfilterData$DateTime)

#' rename everything as before
masai_data <- SDLfilterData
names(masai_data)[names(masai_data) == 'DateTime'] <- 'time'


# try the amt package 
trk <- mk_track(masai_data, .x=long, .y=lat, .t=time, id = id, species=species,
                crs = CRS("+init=epsg:4326"))

# Now it is easy to calculate day/night with either movement track
trk <- trk %>% time_of_day()


#' Save the class here (and apply it later after adding columns to the 
#' object)
trk.class<-class(trk)

# nest by id
nesttrk<-trk%>%nest(-id)
nesttrk

#' We can add a columns to each nested column of data using purrr::map
trk<-trk %>% nest(-id) %>% 
  mutate(dir_abs = map(data, direction_abs,full_circle=TRUE, zero="N"), 
         dir_rel = map(data, direction_rel), 
         sl = map(data, step_lengths),
         nsd_=map(data, nsd))%>%unnest()

#' Now, calculate month, year, hour, week of each observation and append these to the dataset
#' Unlike the movement charactersitics, these calculations can be done all at once, 
#' since they do not utilize successive observations (like step lengths and turn angles do).
trk<-trk%>% 
  mutate(
    week=week(t_),
    month = month(t_, label=TRUE), 
    year=year(t_),
    hour = hour(t_)
  )


#' Now, we need to again tell R that this is a track (rather 
#' than just a data frame)
class(trk)
class(trk)<-trk.class

#' Lets take a look at what we created
trk <- trk %>% group_by(id)
trk

# look at net-squared displacement 
ggplot(trk, aes(x = t_, y=nsd_)) + geom_point()+
  facet_wrap(~id, scales="free")

masai_disp <- ggplot(trk, aes(x = t_, y=nsd_)) + geom_point()+
  facet_wrap(~id, scales="free")

ggsave("plots/masai_disp.pdf")
ggsave("plots/masai_disp.png")

#' ## SSF prep
#' 
#' SSFs assume that data have been collected at regular time intervals.
#' We can use the track_resample function to regularize the trajectory so that
#' all points are located within some tolerence of each other in time. To figure
#' out a meaningful tolerance range, we should calculate time differences between
#' locations & look at as a function of individual.
(timestats<-trk %>% nest(-id) %>% mutate(sr = map(data, summarize_sampling_rate)) %>%
    dplyr::select(id, sr) %>% unnest)

#' add species names to the time intervals
timestats <- timestats %>% 
  left_join(trk %>% 
              distinct(id, species), by = 'id')

#' add the study name 
timestats$study <- "masai"
head(timestats)

#' make sure it matches up 
filter(trk,id=="171606") # wb
filter(trk,id=="171610") # rv


#' export the data 
write.csv(timestats, file="track_resolution_summary/masai_res.csv", row.names = FALSE)


#' Time intervals range depending on the individual. 
#' Lets add on the time difference to each obs.
trk<-trk %>% group_by(id) %>% mutate(dt_ = t_ - lag(t_, default = NA))
trk

# let's select just the AWBVs 
trk <- filter(trk,species=="wb")

# need to run timestats again for the subsetted dataframe so that the IDs match up below
(timestats<-trk %>% nest(-id) %>% mutate(sr = map(data, summarize_sampling_rate)) %>%
    dplyr::select(id, sr) %>% unnest)

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
ssfdat<-NULL
temptrk<-with(trk, track(x=x_, y=y_, t=t_, id=id))
uid<-unique(trk$id) # individual identifiers
luid<-length(uid) # number of unique individuals
for(i in 1:luid){
  # Subset individuals & regularize track
  temp<-temptrk%>% filter(id==uid[i]) %>% 
    track_resample(rate=hours(round(timestats$median[i])), 
                   tolerance=minutes(30))
  
  # Get rid of any bursts without at least 2 points
  temp<-filter_min_n_burst(temp, 2)
  
  # burst steps
  stepstemp<-steps_by_burst(temp)
  
  # create random steps using fitted gamma and von mises distributions and append
  rnd_stps <- stepstemp %>%  random_steps(n = 15)
  
  # append id
  rnd_stps<-rnd_stps%>%mutate(id=uid[i])
  
  # append new data to data from other individuals
  ssfdat<-rbind(rnd_stps, ssfdat)
}
ssfdat<-as_tibble(ssfdat)
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
#' # we want the x2_ and y2_ columns for Movebank 
head(ssfdat)
ncol(ssfdat)
ssfdat2 <- SpatialPointsDataFrame(ssfdat[,c("x2_","y2_")], ssfdat, 
                                  proj4string=CRS("+proj=longlat +datum=WGS84"))  
ssf.df <- data.frame(spTransform(ssfdat2, CRS("+proj=longlat +datum=WGS84"))) 

names(ssf.df)[names(ssf.df) == 'id'] <- 'individual.local.identifier'
names(ssf.df)[names(ssf.df) == 'x2_.1'] <- 'location-long'
names(ssf.df)[names(ssf.df) == 'y2_.1'] <- 'location-lat'
head(ssf.df)

ssf.df$timestamp<-ssf.df$t1_
ssf.df %>% select('location-lat', x1_, x2_, y1_, y2_, 'location-long') %>% head


#' These points then need to be annotated prior to fitting ssfs. Let's 
#' Can subset to certain essential columns so as take up less space, making it easier to annotate (and also possible to upload to github)
ssf.df.out<-ssf.df %>% select("timestamp", "location-long", "location-lat","individual.local.identifier","case_")
head(ssf.df.out)
write.csv(ssf.df.out, file="for_annotation/MasaiSSFannotate.csv", row.names = FALSE)

#' we can export the whole file with all of the extra columns we added 
write.csv(ssf.df, file="amt_tracks/MasaiSSFAll.csv", row.names = FALSE)

