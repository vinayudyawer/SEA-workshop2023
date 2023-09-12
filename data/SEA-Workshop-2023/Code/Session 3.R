## Code for Session 3
## ------------------------------------------------------------------------------------------------------------------------------ ##

## Load useful libraries
library(tidyverse)
library(VTrack)
library(lubridate)
library(sf)
library(mapview)
library(remora)


## ------------------------------------------------------------------------------------------------------------------------------ ##
## Explore patterns of detection with VTrack
## ------------------------------------------------------------------------------------------------------------------------------ ##

## Input, explore and format data from the IMOS repository to use in VTrack

detections <- 
  read_csv('https://raw.githubusercontent.com/vinayudyawer/SEA-workshop2023/main/data/Session%203/IMOS_detections.csv')

tag_metadata <- 
  read_csv('https://raw.githubusercontent.com/vinayudyawer/SEA-workshop2023/main/data/Session%203/IMOS_transmitter_deployment_metadata.csv') %>% 
  left_join(read_csv('https://raw.githubusercontent.com/vinayudyawer/SEA-workshop2023/main/data/Session%203/IMOS_animal_measurements.csv'))

station_info <- 
  read_csv('https://raw.githubusercontent.com/vinayudyawer/SEA-workshop2023/main/data/Session%203/IMOS_receiver_deployment_metadata.csv')

# modify column names to be ready by VTrack
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

# setup the data to be read by VTrack
input_data <- setupData(Tag.Detections = detections,
                        Tag.Metadata = tag_metadata,
                        Station.Information = station_info,
                        source = "IMOS",
                        crs = sp::CRS("EPSG:4326"))

summary(input_data)

# explore the different components of the data
input_data$Tag.Detections
input_data$Tag.Metadata
input_data$Station.Information

## ------------------------------------------------------------------------------------------------------------------------------ ##
## Examine patterns in detection and dispersal

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

## Summarise detections patterns
det_sum <- detectionSummary(ATTdata = input_data, sub = "%Y-%m")

summary(det_sum)

# explore the detection summary results
det_sum$Overall
det_sum$Subsetted

# examine the temporal patterns in detection
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
  labs(x = "Month of year", y = "Mean Detection Index") +
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
  labs(x = "Month of year", y = "Mean Dispersal distance (m)") +
  theme_bw()


## ------------------------------------------------------------------------------------------------------------------------------ ##
## Explore your data with remora
## ------------------------------------------------------------------------------------------------------------------------------ ##

## Create and explore a receiver array report
shinyReport(type = "receivers")

## Create and explore a transmitter report
shinyReport(type = "transmitters")

# learn more about the functions from the vignettes
vignette("shinyReport_receivers", package = "remora")
vignette("shinyReport_transmitters", package = "remora")

# learn about all the other features of the package
browseVignettes(package = "remora")

## ------------------------------------------------------------------------------------------------------------------------------ ##
## End of Session 3
## ------------------------------------------------------------------------------------------------------------------------------ ##

