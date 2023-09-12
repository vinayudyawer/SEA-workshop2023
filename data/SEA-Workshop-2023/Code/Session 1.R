## Code for Session 1
## ------------------------------------------------------------------------------------------------------------------------------ ##

## Load useful libraries
library(tidyverse)
library(lubridate)
library(sf)
library(ggspatial)
library(mapview)
library(leaflet)

## ------------------------------------------------------------------------------------------------------------------------------ ##
## Import and explore datasets using tidyverse
## ------------------------------------------------------------------------------------------------------------------------------ ##
## Tidyverse

blacktip <- read_csv('data/Session 1/Blacktip_ClevelandBay.csv')

# You can also use read_csv to input data directly from a website URL
blacktip <- 
  read_csv('https://raw.githubusercontent.com/vinayudyawer/SEA-workshop2023/main/data/Session%201/Blacktip_ClevelandBay.csv')

head(blacktip)

## ------------------------------------------------------------------------------------------------------------------------------ ##
## Pipes

# To see what class our data is in, we could use this code...
class(blacktip)

# Alternatively in the tidyverse we could use this code...
blacktip %>% class()

# Now insert functions into the pipe chain
blacktip %>% View()
blacktip %>% head() # first 6 rows by default
blacktip %>% tail(10) # specify we want to look at the last 10 rows

blacktip %>% nrow() # number of rows in the data frame
blacktip %>% ncol() # number of columns in the data frame
blacktip %>% str() # provides internal structure of an R object
blacktip %>% summary() # provides result summary of the data frame

blacktip %>% 
  subset(transmitter_name == "Colin") %>% # subset dataset to include only detections by 'Colin'
  with(table(station_name)) %>% # create a table with the number of rows (i.e. detections) per receiver
  barplot(las = 2, xlab = "Receiver station", ylab = "Number of Detections") # barplot of number of Colin's detections recorded per receiver

## ------------------------------------------------------------------------------------------------------------------------------ ##
## dplyr

# select
blacktip <- 
  blacktip %>% 
  select(date_time, latitude, longitude, receiver, station_name, transmitter_name, transmitter, sensor_value) %>% # columns we want to include
  select(-sensor_value) # the minus symbol denotes columns we want to drop

head(blacktip)

# filter and arrange
blacktip %>%
  filter(transmitter_name == "Ana") %>%
  arrange(date_time) # arrange Ana's detections in chronological order

blacktip %>%
  filter(transmitter_name == "Bruce") %>%
  arrange(desc(date_time)) # arrange Bruce's detections in descending chronological order

# group_by and summarise
blacktip %>%
  group_by(transmitter_name) %>%
  summarise(NumDetections = n()) # summarise number of detections per tagged shark

blacktip %>%
  group_by(transmitter_name, station_name) %>%
  summarise(NumDetections = n()) # summarise number of detections per shark at each receiver

# mutate
blacktip <- 
  blacktip %>%
  mutate(date = as.Date(date_time)) %>% # adding a column to the blacktip data with date of each detection
  mutate(transmitter = NULL) # removing the `Transmitter` column

head(blacktip)

## ------------------------------------------------------------------------------------------------------------------------------ ##
## lubridate

blacktip <-
  blacktip %>% 
  mutate(local_date_time = with_tz(date_time, tzone = "Australia/Brisbane")) %>% # convert to local "Australia/Brisbane" date time (UTC + 10hrs)
  mutate(date = date(local_date_time)) # use lubridate to update local date time into a date field

## ------------------------------------------------------------------------------------------------------------------------------ ##
## ggplot2

help.search("geom_", package = "ggplot2")

blacktip %>%
  group_by(transmitter_name, date) %>% 
  summarise(daily_detections = n()) %>% # use summarise to calculate numbers of detections per day per animal
  ggplot(mapping = aes(x = transmitter_name, y = daily_detections)) + # define the aesthetic map (what to plot)
  xlab("Tag") + ylab("Number of detections per day") +
  geom_boxplot() # define the geometric object (how to plot it).. in this case a boxplot

blacktip %>%
  ggplot(mapping = aes(x = local_date_time, y = transmitter_name)) + 
  xlab("Date") + ylab("Tag") +
  geom_point()

## **Additional Task: Now that you’ve plotted the raw dates, can you figure out how to plot daily detections of our tagged sharks**

blacktip %>%
  ggplot(mapping = aes(x = local_date_time, y = station_name)) + 
  xlab("Date") + ylab("Receiver station") +
  geom_point() +
  facet_wrap(~transmitter_name, nrow=1) # This time plot seperate boxplots for each shark

## **Additional Task: Can you now plot this with a different colour for each shark?**

## ------------------------------------------------------------------------------------------------------------------------------ ##
## Working with Spatial objects using sf, ggspatial and mapview
## ------------------------------------------------------------------------------------------------------------------------------ ##

# Import datasets
blacktip <- 
  read_csv('https://raw.githubusercontent.com/vinayudyawer/SEA-workshop2023/main/data/Session%201/Blacktip_ClevelandBay.csv')
statinfo <- 
  read_csv('https://raw.githubusercontent.com/vinayudyawer/SEA-workshop2023/main/data/Session%201/Station_information.csv')


cb_stations <- 
  statinfo %>% 
  filter(installation %in% "Cleveland Bay") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F)

blacktip_sf <-
  blacktip %>% 
  group_by(transmitter_name, station_name, longitude, latitude) %>% 
  summarise(num_det = n()) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F)

head(blacktip_sf)

## ------------------------------------------------------------------------------------------------------------------------------ ##
## plotting spatial objects using ggplot2
ggplot(cb_stations) +
  geom_sf()

## ggspatial

# Open Street Map basemap
ggplot() +
  annotation_map_tile(type = "osm", zoom = 12) +
  layer_spatial(data = cb_stations)

# Carto (light version)
ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 12) +
  layer_spatial(data = cb_stations)

## ------------------------------------------------------------------------------------------------------------------------------ ##
## Non-standard basemaps 

# ESRI satellite imagery
esri_sat <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                   'World_Imagery/MapServer/tile/${z}/${y}/${x}.jpeg')

ggplot() +
  annotation_map_tile(type = esri_sat, zoom = 12) +
  layer_spatial(data = cb_stations)

#ESRI topographical imagery
esri_topo <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                    'World_Topo_Map/MapServer/tile/${z}/${y}/${x}.jpeg')

ggplot() +
  annotation_map_tile(type = esri_topo, zoom = 12) +
  layer_spatial(data = cb_stations)

## ------------------------------------------------------------------------------------------------------------------------------ ##
## Faceting a map

ggplot() +
  annotation_map_tile(type = esri_topo, zoom = 12) +
  layer_spatial(data = blacktip_sf, aes(size = num_det, col = transmitter_name)) +
  layer_spatial(data = cb_stations, pch = 21) +
  facet_wrap(~transmitter_name, nrow = 1) +
  annotation_scale() +
  theme(legend.position = "bottom")

## ------------------------------------------------------------------------------------------------------------------------------ ##
## mapview

mapview(cb_stations)

# combining map layers

m <-
  mapview(cb_stations, 
          alpha.reg = 0, 
          alpha = 1, 
          color = "grey", 
          map.types = "Esri.WorldImagery", 
          legend = F, homebutton = F, 
          cex = 5) +
  mapview(blacktip_sf, 
          zcol = "transmitter_name", 
          alpha = 0, 
          alpha.reg = 1,
          burst = T, 
          legend = F, homebutton = F, 
          cex = 5)

m

# exploring the different aspects of a mapview object
m@object
m@map

# adding other features to map
mm <-
  m@map %>% 
  addLayersControl(
    baseGroups = unique(blacktip_sf$transmitter_name),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(unique(blacktip_sf$transmitter_name))

mm

# saving output as an interactive html file

mapview::mapshot(mm, url = "Blacktip_interactive_map.html", remove_controls = NULL, selfcontained = TRUE)

## ------------------------------------------------------------------------------------------------------------------------------ ##
## End of Session 1
## ------------------------------------------------------------------------------------------------------------------------------ ##


