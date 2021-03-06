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
Samp <- mydata %>% sample_n(size=100000)
Samp

#' plot(Samp$long, Samp$lat)

#library(ggmap)
#qmplot(long,
#       lat,
#       data = Samp,
#       maptype = "toner-lite")

#' using leaflet
#' Library
library(leaflet)
#' Remove the suspected hybrid species and unknown vul
species_for_removal<-c("cv_wb", "vul")
Samp <- Samp %>% filter(!species %in% species_for_removal) %>% droplevels()
levels(as.factor(Samp$id))
levels(as.factor(Samp$species))
# Call RColorBrewer::display.brewer.all() to see all possible palettes
pal <- colorFactor(
  palette = 'Accent',
  domain = Samp$species
)
# Final Map
m <- leaflet(Samp) %>%
  addTiles()  %>%
  # setView( lat=-27, lng=170 , zoom=4) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(
    ~ long,
    ~ lat,
    fillOpacity = 0.7,
    color = ~ pal(species),
    radius = 3,
    stroke = FALSE
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~ species,
    title = "Species",
    opacity = 1
  )
m 

#' map with just the gyps
not_gyps <-c("hv", "lf", "wh")
gyps <- Samp %>% filter(!species %in% not_gyps) %>% droplevels()
# Final Map
m1 <- leaflet(gyps) %>%
  addTiles()  %>%
  # setView( lat=-27, lng=170 , zoom=4) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(
    ~ long,
    ~ lat,
    fillOpacity = 0.7,
    color = ~ pal(species),
    radius = 3,
    stroke = FALSE
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~ species,
    title = "Species",
    opacity = 1
  )
m1 

#' Plot the white back IDs by colour 
not_wbs <-c("cv", "rv")
wb <- gyps %>% filter(!species %in% not_wbs) %>% droplevels()
#' need a new palette here
# Call RColorBrewer::display.brewer.all() to see all possible palettes
# pal1 <- colorFactor(viridis(15), domain = wb$study) # I'm assuming the variable ward contains the names of the communities.
# If you want to set your own colors manually: https://www.rapidtables.com/web/color/color-scheme.html
pal1 <- colorFactor(
  palette = c('#A2C936', '#C9A736', '#3682C9', '#36B1C9', '#36C9BB',"#36C98E", "#36C94C"
              , "#C95336", "#C93636", "#7836C9", "#C9365F", "#4C36C9", "#000000", "#FFFFFF", "#FFA8AD"),
  domain = wb$study
)

# Final Map
m2 <- leaflet(wb) %>%
  addTiles()  %>%
  # setView( lat=-27, lng=170 , zoom=4) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(
    ~ long,
    ~ lat,
    fillOpacity = 0.7,
    color = ~ pal1(study),
    radius = 3,
    stroke = FALSE
  ) %>%
  addLegend(
    "bottomright",
    pal = pal1,
    values = ~ study,
    title = "Study",
    opacity = 1
  )
m2 
