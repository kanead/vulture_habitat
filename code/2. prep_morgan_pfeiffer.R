# Morgan Pfeiffer's SA Vulture Tracking Dataset 

library(lubridate)
library(SDLfilter)
library(amt)
library(sp)

# select Morgan's data
morgan_data <- filter(mydata, study == "pfeiffer")
morgan_data

# drop missing rows
morgan_data<- morgan_data %>% drop_na

#' Check for duplicated observations (ones with same lat, long, timestamp,
#'  and individual identifier).
ind2<-morgan_data %>% select(long, lat, id) %>%
  duplicated 
sum(ind2) 
# remove them 
morgan_data$dups <- ind2
morgan_data <- filter(morgan_data,dups=="FALSE")
morgan_data

#' set the time column
#' some are in day/month/year format e.g. X016_Complete; X020_Final; X021_Final; X022_Complete; X032_Final; X033_Complete;  
#' some are in month/day/year format e.g. X023; X027; X042; X050; X051; X052; X053; X055; X056; X057; X071
#' Morgan said X051 never transmitted data so I remove that here
levels(factor(morgan_data$id))
temp1<-filter(morgan_data,
               id == "X016_Complete" |
               id=="X021_Final" | 
               id == "X020_Final" |
               id == "X021_Final" |
               id == "X022_Complete" |
               id == "X032_Final" |
               id == "X033_Complete" ); tail(temp1) ;head(temp1)
temp1
temp1$New_time<-parse_date_time(x=temp1$time,c("%d/%m/%Y %H:%M"))
tail(temp1)

temp2<-filter(morgan_data, 
                id == "X023" |
                id == "X027" |
                id == "X042" |
                id == "X050" |
                id == "X052" |
                id == "X053" |
                id == "X055" |
                id == "X056" |
                id == "X057" |
                id == "X071" ); tail(temp2) ;head(temp2)
temp2
temp2$New_time<-parse_date_time(x=temp2$time,c("%m/%d/%Y %H:%M"))
tail(temp2)

# stick them back together again
morgan_data <- full_join(temp1,temp2)
morgan_data

# Morgan's data is in reverse order of time
# sort by the bird ID and reverse the order
morgan_data <- morgan_data %>% group_by(id)  %>% 
  arrange(New_time, .by_group = TRUE)
morgan_data

# keep only the new time data
morgan_data <- select(morgan_data, New_time,long,lat,id,species,study)
morgan_data <- rename(morgan_data, time = New_time)
#' The time zone should be UTC +2 (South African Standard Time)
morgan_data$time<-force_tz(morgan_data$time, "africa/johannesburg")

#' estimate vmax for threshold speed 
#' names(morgan_data)[names(morgan_data) == 'time'] <- 'DateTime'
#' speed.est.data <- morgan_data %>% select(id,DateTime,lat,long)
#' speed.est.data$qi = 5
#' est.vmax(sdata = data.frame(speed.est.data))

#' filter extreme data based on a speed threshold 
#' based on vmax which is km/hr
#' time needs to be labelled DateTime for these functions to work
library(SDLfilter)
names(morgan_data)[names(morgan_data) == 'time'] <- 'DateTime'
SDLfilterData<-ddfilter.speed(data.frame(morgan_data), vmax = 70, method = 1)
length(SDLfilterData$DateTime)

#' rename everything as before
morgan_data <- SDLfilterData
names(morgan_data)[names(morgan_data) == 'DateTime'] <- 'time'

# check the minimum time and the maximum time
min_time <- morgan_data %>% group_by(id) %>% slice(which.min(time))
data.frame(min_time)

max_time <- morgan_data %>% group_by(id) %>% slice(which.max(time))
data.frame(max_time)

#' determine the length of time each bird was tracked for
duration<-difftime(max_time$time, min_time$time, units = "days");duration

# try the amt package
trk <-
  mk_track(
    morgan_data,
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
  dplyr::select(id, sr) %>% unnest %>% arrange(id) ; data_summary


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
data_summary$study <- "Morgan"
data_summary

#' can export this data summary 
#' write.csv(data_summary, file="track_resolution_summary/morgan_data_summary.csv", row.names = FALSE)


#' We can map the data
#' turn back to lat long
trk_map <-
  mk_track(
    morgan_data,
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
#' ## Some plots of movement characteristics

#' ### Absolute angles (for each movement) relative to North 
#' We could use a rose diagram (below) to depict the distribution of angles. 
#+fig.height=12, fig.width=12
ggplot(trk, aes(x = dir_abs, y=..density..)) + geom_histogram(breaks = seq(0,360, by=20))+
  coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Density") + ggtitle("Angles Direct") + 
  scale_x_continuous("", limits = c(0, 360), breaks = seq(0, 360, by=20), 
                     labels = seq(0, 360, by=20))+
  facet_wrap(~id)

#' ### Turning angles 
#' 
#' Note: a 0 indicates the animal continued to move in a straight line, a 180
#' indicates the animal turned around (but note, resting + measurement error often can
#' make it look like the animal turned around).
#+fig.height=12, fig.width=12
ggplot(trk, aes(x = dir_rel, y = ..density..)) + geom_histogram(breaks = seq(-180, 180, by =
                                                                               20)) +
  coord_polar(start = 0) + theme_minimal() +
  scale_fill_brewer() + ylab("Density") + ggtitle("Angles Direct") +
  scale_x_continuous(
    "",
    limits = c(-180, 180),
    breaks = seq(-180, 180, by = 20),
    labels = seq(-180, 180, by = 20)
  ) +
  facet_wrap( ~ id)

#' ### Turning angles as histograms
#+fig.height=12, fig.width=12
ggplot(trk, aes(x = dir_rel)) +  geom_histogram(breaks = seq(-180,180, by=20))+
  theme_minimal() + 
  scale_fill_brewer() + ylab("Count") + ggtitle("Angles Relative") + 
  scale_x_continuous("", limits = c(-180, 180), breaks = seq(-180, 180, by=20),
                     labels = seq(-180, 180, by=20))+facet_wrap(~id, scales="free")

#' ### Net-squared displacement over time for each individual
#+fig.height=12, fig.width=12
ggplot(trk, aes(x = t_, y=nsd_)) + geom_point()+
  facet_wrap(~id, scales="free")

morgan_disp <- ggplot(trk, aes(x = t_, y=nsd_)) + geom_point()+
  facet_wrap(~id, scales="free")

ggsave("plots/morgan_disp.pdf")
ggsave("plots/morgan_disp.png")


#' ## Explore movement characteristics by (day/night, hour, month)
#' 
#' ### step length distribution by day/night
#' 
#+fig.height=12, fig.width=12, warning=FALSE, message=FALSE
ggplot(trk, aes(x = tod_, y = log(sl))) + 
  geom_boxplot()+geom_smooth()+facet_wrap(~id)

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
timestats$study <- "morgan"
head(timestats)

#' make sure it matches up 
filter(trk,id=="X027") # cv
filter(trk,id=="X016_Complete") # cv


#' export the data 
write.csv(timestats, file="track_resolution_summary/morgan_res.csv", row.names = FALSE)

#' Time intervals range from every 2 to 15 minutes on average, depending
#' on the individual.  Lets add on the time difference to each obs.
trk<-trk %>% group_by(id) %>% mutate(dt_ = t_ - lag(t_, default = NA))
trk

# let's select just the CVs 
trk <- filter(trk,species=="cv")
trk

# select individuals that have temporal resolution of ~ 15 mins 
trk<- filter(trk,id=="X027"|
         id=="X042"|
         id=="X023"|
         id=="X050"|
         id=="X051"|
         id=="X053"|
         id=="X052"|
         id=="X071"|
         id=="X056"|
         id=="X057"|
         id=="X055")

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
    track_resample(rate=hours(1),
                   tolerance=minutes(10))
  
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
#' we want the x2_ and y2_ columns for Movebank 
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
write.csv(ssf.df.out, file="for_annotation/MorganSSFannotate.csv", row.names = FALSE)

#' we can export the whole file with all of the extra columns we added 
write.csv(ssf.df, file="amt_tracks/MorganSSFAll.csv", row.names = FALSE)

#' ### Space use (MCP or KDE) by week, month, and year
#' 
#' Note:  this code will only work for your critters if you
#' have multiple observations for each combination of
#' (month, year).  If you don't have many observations, you could
#' try:  nest(-id, -year) and unnest(id,year)
#' mcps.week<-trk %>% nest(-id,-year,  -month, -week) %>%  
#' mutate(mcparea = map(data, ~hr_mcp(., levels = c(0.95)) %>% hr_area)) %>% 
#'  select(id, year, month, week, mcparea) %>% unnest()

#' convert our regularised data back into a trk type data frame
home_range_data <-
  ssfdat %>% filter(case_ == "TRUE") %>% select(x1_, y1_, t1_, id) %>% group_by(id)

home_range_trk <-
  mk_track(
    home_range_data,
    .x = x1_,
    .y = y1_,
    .t = t1_,
    id = id,
    crs = CRS("+init=epsg:4326")
  )


mcps <-home_range_trk %>% nest(-id) %>%  
  mutate(mcparea = map(data, ~hr_mcp(., levels = c(0.95)) %>% hr_area)) %>% 
  select(id, mcparea) %>% unnest()

#+fig.height=12, fig.width=12, warning=FALSE, message=FALSE
ggplot(mcps, aes(x = id, y = area)) + geom_point()+
  geom_smooth()

#' Same for KDE
kde <-home_range_trk %>% nest(-id) %>%  
  mutate(kdearea = map(data, ~hr_kde(., levels=c(0.95)) %>% hr_area)) %>%
  select(id, kdearea) %>% unnest()

#+fig.height=12, fig.width=12, warning=FALSE, message=FALSE
ggplot(kde, aes(x = id, y = kdearea)) + geom_point()+
  geom_smooth()


#' map the data
library(leaflet)
levels(as.factor(home_range_data$id))
leaflet() %>% addTiles() %>% 
  addCircleMarkers(data = filter(home_range_data, id == "X071"),
                   lat = ~y1_, lng = ~x1_,
                   color = "blue")


