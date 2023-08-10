##----------------------------------------------------##
## Using remora to explore and analyse telemetry data ##
##----------------------------------------------------##

## Install packages
install.packages("remotes")
remotes::install_github("IMOS-AnimalTracking/remora", build_vignettes = TRUE)

## Once downloaded you can explore the functionality of the package using vignettes that describe the different functions

library(remora)
browseVignettes(package = "remora")

## Load other useful packages
library(tidyverse)
library(sf)
library(mapview)
library(ggspatial)


## 1. Input, explore and format data from IMOS repository to use in remora
detections <- read_csv("data/IMOS Workshop_Bull-shark-sample-dataset/IMOS_detections.csv")
detections

tag_metadata <- read_csv("data/IMOS Workshop_Bull-shark-sample-dataset/IMOS_transmitter_deployment_metadata.csv")
tag_metadata

rec_metadata <- read_csv("data/IMOS Workshop_Bull-shark-sample-dataset/IMOS_receiver_deployment_metadata.csv")
rec_metadata

measurement <- read_csv("data/IMOS Workshop_Bull-shark-sample-dataset/IMOS_animal_measurements.csv")
measurement 

files <- list(det = "data/IMOS Workshop_Bull-shark-sample-dataset/IMOS_detections.csv",
              rmeta = "data/IMOS Workshop_Bull-shark-sample-dataset/IMOS_receiver_deployment_metadata.csv",
              tmeta = "data/IMOS Workshop_Bull-shark-sample-dataset/IMOS_transmitter_deployment_metadata.csv",
              meas = "data/IMOS Workshop_Bull-shark-sample-dataset/IMOS_animal_measurements.csv")

files

## 2. Interactively explore your data
## Create and explore a receiver array report
shinyReport(type = "receivers")

## Create and explore a transmitter report
shinyReport(type = "transmitters")


## 3. Run the Quality control process to flag false or inaccurate detctions
tag_qc <- runQC(x = files, .parallel = TRUE, .progress = TRUE)
tag_qc

grabQC(tag_qc, what = "QCflags")

qc_data <- grabQC(tag_qc, what = "dQC", flag = c("valid", "likely valid"))
qc_data

# Visualise the QC'd detections
plotQC(tag_qc)


## 3. Use remora to access and append environmental data to quality controlled data

## the remora package can access environmental data from the IMOS server

imos_variables()

## 3.1 In-situ data from mooring data from the National Mooring Network

moor_temp <- mooringTable(sensorType = "temperature")

moor_temp %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  mapview(popup = paste("Site code", moor_temp$site_code,"<br>",
                        "URL:", moor_temp$url, "<br>",
                        "Standard names:", moor_temp$standard_names, "<br>",
                        "Coverage start:", moor_temp$time_coverage_start, "<br>",
                        "Coverage end:", moor_temp$time_coverage_end),
          col.regions = "red", color = "white", layer.name = "IMOS Mooring")

# Find the closest mooring to our animal detections in both space and time

# identify nearest mooring in space
det_dist <- getDistance(trackingData = qc_data,
                        moorLocations = moor_temp,
                        X = "receiver_deployment_longitude",
                        Y = "receiver_deployment_latitude",
                        datetime = "detection_datetime")

# identify moorings that have overlapping data with detections
mooring_overlap <- getOverlap(det_dist)

mooring_overlap <-
  mooring_overlap %>% 
  filter(Poverlap == 1)

moor_temp %>% filter(site_code %in% mooring_overlap$moor_site_code) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  mapview(zcol = "site_code")

## Download mooring data from closest moorings
moorIDs <- unique(mooring_overlap$moor_site_code)

moor_data <- mooringDownload(moor_site_codes = moorIDs,
                             sensorType = "temperature",
                             fromWeb = TRUE,
                             file_loc = "imos.cache/moor/temperature")

## Plot depth time of temperature from one mooring along with the detection data
start_date <- "2020-01-01"
end_date <- "2020-02-01"

plotDT(moorData = moor_data$CH050, 
       moorName = "CH050",
       dateStart = start_date, dateEnd = end_date,
       varName = "temperature",
       trackingData = det_dist,
       speciesID = "Carcharhinus leucas",
       IDtype = "species_scientific_name",
       detStart = start_date, detEnd = end_date)


## 3.2 Remote sensing data stored in the IMOS database

imos_variables()

## Extracting current data from the IMOS database (rs_sst_interpolated)

extracted_data <- 
  qc_data %>%
  filter(installation_name %in% c("IMOS-ATF Coffs Harbour line"))

sst_extract <-
  extractEnv(df = extracted_data,
             X = "receiver_deployment_longitude", 
             Y = "receiver_deployment_latitude", 
             datetime = "detection_datetime",
             env_var = "rs_sst_interpolated",
             cache_layers = TRUE,
             crop_layers = TRUE,
             full_timeperiod = FALSE,
             folder_name = "sst",
             .parallel = TRUE)

sst_extract$rs_sst_interpolated

## plot SST data with detection data
summarised_data <-
  sst_extract %>% 
  mutate(date = as.Date(detection_datetime)) %>% 
  group_by(transmitter_id, date) %>% 
  summarise(num_det = n(),
            mean_sst = mean(rs_sst_interpolated, na.rm = T))

ggplot(summarised_data, aes(x = date, y = transmitter_id, size = num_det, color = mean_sst)) +
  geom_point() +
  scale_color_viridis_c() +
  labs(subtitle = "Interpolated sea surface temperature", x = "Date", 
       y = NULL, color = "SST (˚C)", size = "Number of\nDetections") +
  theme_bw()




