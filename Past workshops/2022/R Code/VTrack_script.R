##-------------------------------------------------------------##
## Using VTrack to explore patterns in residency and dispersal ##
##-------------------------------------------------------------##

## Install packages
install.packages("devtools")
devtools::install_github("rossdwyer/VTrack")

## Load other useful packages
library(VTrack)
library(tidyverse)
library(lubridate)
library(sf)
library(mapview)


## 1. Input, explore and format data from IMOS repository to use in VTrack


detections <- read_csv("data/IMOS Workshop_Bull-shark-sample-dataset/IMOS_detections.csv")

detections <-
  detections %>% 
  transmute(transmitter_id = transmitter_id,
            station_name = station_name,
            receiver_name = receiver_name,
            detection_timestamp = detection_datetime,
            longitude = receiver_deployment_longitude,
            latitude = receiver_deployment_latitude,
            sensor_value = transmitter_sensor_raw_value,
            sensor_unit = transmitter_sensor_unit)

detections

tag_metadata <- 
  read_csv("data/IMOS Workshop_Bull-shark-sample-dataset/IMOS_transmitter_deployment_metadata.csv") %>% 
  left_join(read_csv("data/IMOS Workshop_Bull-shark-sample-dataset/IMOS_animal_measurements.csv"))

tag_metadata <-
  tag_metadata %>% 
  transmute(tag_id = transmitter_deployment_id,
            transmitter_id = transmitter_id,
            scientific_name = species_scientific_name,
            common_name = species_common_name,
            tag_project_name = tagging_project_name,
            release_id = transmitter_deployment_id,
            release_latitude = transmitter_deployment_latitude,
            release_longitude = transmitter_deployment_longitude,
            ReleaseDate = transmitter_deployment_datetime,
            tag_expected_life_time_days = transmitter_estimated_battery_life,
            tag_status = transmitter_status,
            sex = animal_sex,
            measurement = measurement_value)

tag_metadata

station_info <- read_csv("data/IMOS Workshop_Bull-shark-sample-dataset/IMOS_receiver_deployment_metadata.csv")

station_info <-
  station_info %>% 
  transmute(station_name = station_name,
            receiver_name = receiver_name,
            installation_name = installation_name,
            project_name = receiver_project_name,
            deploymentdatetime_timestamp = receiver_deployment_datetime,
            recoverydatetime_timestamp = receiver_recovery_datetime,
            station_latitude = receiver_deployment_latitude,
            station_longitude = receiver_deployment_longitude,
            status = active)

station_info

## 2. Setup data so VTrack can read and access correct data

input_data <- setupData(Tag.Detections = detections,
                        Tag.Metadata = tag_metadata,
                        Station.Information = station_info,
                        source = "IMOS",
                        crs = sp::CRS("+init=epsg:4326"))

summary(input_data)

input_data$Tag.Detections

input_data$Tag.Metadata

input_data$Station.Information


## 3. Create a simple detection plot and map

## use the VTrack function for a simple abacus plot
abacusPlot(input_data)

## plot your own!
combined_data <- 
  input_data$Tag.Detections %>% 
  left_join(input_data$Station.Information)

combined_data %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(Transmitter, Station.Name, date, Installation) %>% 
  summarise(num_detections = n()) %>% 
  ggplot(aes(x = date, y = Transmitter, size = num_detections, color = Installation)) +
  geom_point() +
  labs(size = "Number of Detections", color = "Installation Name") +
  theme_bw()

## Map the data
combined_data %>% 
  group_by(Station.Name, Latitude, Longitude, Transmitter, Installation) %>% 
  summarise(num_detections = n()) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  mapview(cex = "num_detections", zcol = "Installation")


## 4. Examine patterns in detections and dispersal

## Summarise detections patterns
det_sum <- detectionSummary(ATTdata = input_data, sub = "%Y-%m")
summary(det_sum)

det_sum$Overall
det_sum$Subsetted

## Plot results to understand monthly patterns in detection index between sexes

monthly_detection_index <-
  det_sum$Subsetted %>% 
  mutate(date = lubridate::ymd(paste(subset, 01, "-")),
         month = month(date, label = T, abbr = T)) %>% 
  group_by(Sex, month) %>% 
  summarise(mean_DI = mean(Detection.Index),
            se_DI = sd(Detection.Index)/sqrt(n()))

monthly_detection_index %>% 
  ggplot(aes(x = month, y = mean_DI, group = Sex, color = Sex,
             ymin = mean_DI - se_DI, ymax = mean_DI + se_DI)) +
  geom_point() +
  geom_path() +
  geom_errorbar(width = 0.2) +
  theme_bw()

## Summarise dispersal patterns
disp_sum <- dispersalSummary(ATTdata = input_data)

disp_sum

monthly_dispersal <-
  disp_sum %>% 
  mutate(month = month(Date.Time, label = T, abbr = T)) %>% 
  group_by(Sex, month) %>% 
  summarise(mean_disp = mean(Consecutive.Dispersal),
            se_disp = sd(Consecutive.Dispersal)/sqrt(n()))

monthly_dispersal %>% 
  ggplot(aes(x = month, y = mean_disp, group = Sex, color = Sex,
             ymin = mean_disp - se_disp, ymax = mean_disp + se_disp)) +
  geom_point() +
  geom_path() +
  geom_errorbar(width = 0.2) +
  theme_bw()








