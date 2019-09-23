#' map the whole data set from 1_load_data
#' load packages
library(sf)
library(sp)
library(hablar)
library(rnaturalearth)
# change the lat long columns to numeric class
mydata <- mydata %>%
  convert(num(long:lat))
str(mydata)

# Spatial data with sf
# Create sf object with geo_data data frame and CRS
points_sf <- st_as_sf(mydata, coords = c("long", "lat"), crs = 4326)
class(points_sf)
str(points_sf)

# Get coastal and country world maps as Spatial objects
coast_sp <- ne_coastline(scale = "medium")
countries_sp <- ne_countries(scale = "medium")

# Convert them to sf format
coast_sf <- ne_coastline(scale = "medium", returnclass = "sf")
countries_sf <- ne_countries(scale = "medium", returnclass = "sf")

#' map a sample of the data
Samp <- mydata %>% sample_n(size=1000000)
Samp

plot(Samp$long, Samp$lat)

library(ggmap)
qmplot(long,
       lat,
       data = Samp,
       maptype = "toner-lite")

#' using leaflet
#' Library
library(leaflet)
#' Remove the suspected hybrid species
Samp <- Samp %>% filter(species != "cv_wb")
# Call RColorBrewer::display.brewer.all() to see all possible palettes
pal <- colorFactor(
  palette = 'Set3',
  domain = mydata$species
)
# Final Map
m <- leaflet(Samp) %>% 
  addTiles()  %>% 
  # setView( lat=-27, lng=170 , zoom=4) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~long, ~lat, 
                    fillOpacity = 0.7, color = ~pal(species), radius=3, stroke=FALSE)
m 
