#' Code for extracting summary statistics from the comparative vulture project

#' Load the required packages
library(readr)
library(tidyverse)

#' Section 1: Load the data ----
data_path <- "track_resolution_summary"   # path to the data

files <- dir(data_path, pattern = "*.csv") # get file names

res_data <- files %>%
  # read in all the files, appending the path before the filename
  map( ~ read_csv(file.path(data_path, .))) %>%
  reduce(rbind)
levels(as.factor((res_data$study)))
res_data
#' associate a broad geographic location with each study

#' CK = east
#' mara = east
#' north = east
#' Corinne_mara = east
#' GA_Namibia = south
#' inter = south
#' Kerri = south
#' mend_Namibia = south
#' Morgan = south
#' Swazi = south
#' andre = south
#' ralph = south
#' glynn = south
#' orr = south 
#' galligan = south 
#' Schabo = south 
#' louis = south
#' Campbell = south

res_data$region <-
  if_else(
    res_data$study == "CK" |
      res_data$study == "mara" |
      res_data$study == "north" |
      res_data$study == "Corinne_mara" ,
    "east",
    "south"
  )

#'export the combined summary stats table for the temporal resolution of the data
write.csv(res_data, file = "results/summary_all_tracks.csv", row.names = FALSE)
#' res_data <- read.csv("results/summary_all_tracks.csv", header = TRUE)

#' run some analyses on the data
#' extract data for birds that were tracked for over a year and remove some suspect data
#'the double releases from Kerri

nonWantedLevels <- c("AG382", "AM89", "AM88", "AM87")
subset <- res_data %>% dplyr::filter(!id %in% nonWantedLevels)
levels(as.factor(subset$id))
length(levels(as.factor(subset$id)))
length(levels(as.factor(subset$study)))

#' 188 birds from 16 studies 

#' how long were the birds tracked for?
p0 <-
  ggplot(data = subset,
         mapping = aes(
           x = species,
           y = duration,
           fill = factor(species)
         )) + geom_boxplot(alpha = 0.5, show.legend = FALSE) + ylab("duration (days)") +
  theme_bw()
duration_data_all <- p0 + theme(legend.position = "none")
duration_data_all
ggsave("plots/duration_data_all.png")

#' extract just the gyps for all durations
species_to_keep <- c("cv", "rv", "wb")
gyps <- subset %>% dplyr::filter(species %in% species_to_keep)
#' how long were the gyps tracked for
duration_data_gyps <- ggplot(data = gyps,
                             mapping = aes(
                               x = species,
                               y = duration,
                               fill = factor(species)
                             )) + geom_boxplot(alpha = 0.5, show.legend = FALSE) + ylab("duration (days)") +
  theme_bw() + theme(
    legend.position = "none",
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )
duration_data_gyps
ggsave("plots/duration_data_gyps.png")
#' how many data points?
sum(gyps$n)
#' get the mean duration
gyps %>% summarize(mean_duration = mean(duration))
#' group by species and 
gyps %>% group_by(species) %>% summarize(mean_duration = mean(duration))
#' keep only the birds that were tracked for over a year
year <- dplyr::filter(subset, duration > 365) 
#' boxplots of the KDEs by speices
ggplot(data = year, mapping = aes(x = species, y = kde)) + geom_boxplot() + ylab("KDE 95%")
#' boxplots of the KDEs by region
ggplot(data = year, mapping = aes(x = region, y = kde)) + geom_boxplot() + ylab("KDE 95%")
#' group by species and get the mean kde
year %>% group_by(species) %>% summarize(mean_kde = mean(kde))
#' group by species and get the max kde
year %>% group_by(species) %>% summarize(max_kde = max(kde))
#' group by species and get the median kde
year %>% group_by(species) %>% summarize(median_kde = median(kde))
#' extract one species and get its max kde
dplyr::filter(year, species == "cv") %>% summarize(maxkde = max(kde))
#' group by region and get the median kde
year %>% group_by(region) %>% summarize(median_kde = median(kde))


#' subset to Gyps that were tracked for over a year
gyps_year <- year %>% dplyr::filter(species %in% species_to_keep)
length(gyps_year$id)
p1 <-
  ggplot(data = gyps_year,
         mapping = aes(
           x = species,
           y = kde,
           fill = factor(species)
         )) + geom_boxplot(alpha = 0.5, show.legend = FALSE) + ylab("KDE 95% km2") + theme_bw()
p1
#' remove outliers
# compute lower and upper whiskers
ylim1 = boxplot.stats(gyps_year$kde)$stats[c(1, 5)]

# scale y limits based on ylim1
kde_gyps_year = p1 + coord_cartesian(ylim = ylim1 * 1.05) + theme(
  legend.position = "none",
  axis.text = element_text(size = 20),
  axis.title = element_text(size = 20)
)
kde_gyps_year
ggsave("plots/kde_gyps_year.png")

#' plot white backs by region
wb <- year %>% dplyr::filter(species == c("wb"))
p3 <-
  ggplot(data = wb,
         mapping = aes(
           x = region,
           y = kde,
           fill = factor(region)
         )) + geom_boxplot(alpha = 0.5, show.legend = FALSE) + ylab("KDE 95% km2") + theme_bw()
p3
#' remove outliers
# compute lower and upper whiskers
ylim1 = boxplot.stats(wb$kde)$stats[c(1, 5)]

# scale y limits based on ylim1
wb_region = p3 + coord_cartesian(ylim = ylim1 * 1.05) + theme(
  legend.position = "none",
  axis.text = element_text(size = 20),
  axis.title = element_text(size = 20)
)
wb_region
ggsave("plots/wb_region.png")

#' group by region and get the median kde for the white backs
wb %>% group_by(region) %>% summarize(median_kde = median(kde))
