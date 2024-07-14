# CODE: 06_Calculate_Other_Sensors.R
# CODE PURPOSE: Calculate proportion of birds tracked with argos/gps that also recorded other sensor data
# In theory, these should be reflected in the sensor types in the downloaded movebank reference data...
# But we were having issues with mismatch between reference data and actual data downloaded for Argos/GPS...
# So we calculated separately

setwd()
Sys.setenv(TZ = 'UTC')


# 1) LOAD PACKAGES #################################################
library(tidyverse)


# 2) LOAD DATA #########################################
## 2a) ALL EVENT ####
event_dat_all <-  readRDS("./Data/Merged/04_all_event_dat.rds")


# 3) SUMMARIZE SENSOR DATA AVAILABLE ###########################################################
# Note, these are birds that have sensor data paired with geographic location data
# Note -- species counts below are not set up to deal with unknown dowitcher (but they didn't have exra sensor information so not adjusting for now)

## 3a) CALCULATE TOTAL # INDS & SP ####
tot_inds <- event_dat_all %>% 
  distinct(sc.individual.id) %>% 
  count() %>%
  pull()
tot_sp_count <- 36 #manual addition including other data types

## 3b) ALTITUDE ####
altitude_counts <- event_dat_all %>%
  filter(!is.na(height.above.ellipsoid) | #get cells with some altitude data
           !is.na(height.above.msl)|
           !is.na(height.above.ground.level)) %>% 
  dplyr::select(individual.taxon.canonical.name, sc.dataset.id, sc.individual.id) %>%
  distinct() %>%
  summarize(n.indiv = nrow(.),
            percent.ind = round(n.indiv/tot_inds*100, digits = 1),
            n.studies = length(unique(sc.dataset.id)),
            n.species = length(unique(individual.taxon.canonical.name))) %>%
  mutate(sensor.type = "altitude")
str(altitude_counts)

## 3b) BAROMETRIC PRESSURE ####
pressure_counts <- event_dat_all %>%
  filter(!is.na(barometric.pressure)) %>% #barometric air pressure
  dplyr::select(individual.taxon.canonical.name, study.id, sc.individual.id) %>%
  distinct() %>%
  arrange(individual.taxon.canonical.name, sc.individual.id) %>%
  summarize(n.indiv = nrow(.),
            percent.ind = round(n.indiv/tot_inds*100, digits = 1),
            n.studies = length(unique(study.id)),
            n.species = length(unique(individual.taxon.canonical.name))) %>%
  mutate(sensor.type = "barometric.pressure")

## 3c) EXTERNAL TEMPERATURE ####
temp_counts <- event_dat_all %>%
  filter(!is.na(external.temperature)) %>% 
  dplyr::select(individual.taxon.canonical.name, study.id, sc.individual.id) %>%
  distinct() %>%
  arrange(individual.taxon.canonical.name, sc.individual.id) %>%
  summarize(n.indiv = nrow(.),
            percent.ind = round(n.indiv/tot_inds*100, digits = 1),
            n.studies = length(unique(study.id)),
            n.species = length(unique(individual.taxon.canonical.name))) %>%
  mutate(sensor.type = "external.temperature")

## 3d) ACCELERATION ####
acc_counts <- event_dat_all %>%
  filter(!is.na(acceleration.x)|!is.na(acceleration.raw.x)) %>% 
  dplyr::select(individual.taxon.canonical.name, sc.dataset.id, sc.individual.id) %>%
  distinct() %>%
  arrange(individual.taxon.canonical.name, sc.individual.id) %>%
  summarize(n.indiv = nrow(.),
            percent.ind = round(n.indiv/tot_inds*100, digits = 1),
            n.studies = length(unique(sc.dataset.id)),
            n.species = length(unique(individual.taxon.canonical.name))) %>%
  mutate(sensor.type = "accelerometer")

## 3e) GYROSCOPE ####
gyro_counts <- event_dat_all %>%
  filter(!is.na(angular.velocity.x)|
           !is.na(angular.velocity.y)) %>% 
  dplyr::select(individual.taxon.canonical.name, sc.dataset.id, sc.individual.id) %>%
  distinct() %>%
  arrange(individual.taxon.canonical.name, sc.individual.id) %>%
  summarize(n.indiv = nrow(.),
            percent.ind = round(n.indiv/tot_inds*100, digits = 1),
            n.studies = length(unique(sc.dataset.id)),
            n.species = length(unique(individual.taxon.canonical.name))) %>%
  mutate(sensor.type = "gyroscope")

## 3f) CHECK FOR MAGNETOMETER ####
mag_counts <- event_dat_all %>%
  filter(!is.na(magnetic.field.x)|
           !is.na(magnetic.field.raw.x)) %>% 
  dplyr::select(individual.taxon.canonical.name, sc.dataset.id, sc.individual.id) %>%
  distinct() %>%
  arrange(individual.taxon.canonical.name, sc.individual.id) %>%
  summarize(n.indiv = nrow(.),
            percent.ind = round(n.indiv/tot_inds*100, digits = 1),
            n.studies = length(unique(sc.dataset.id)),
            n.species = length(unique(individual.taxon.canonical.name))) %>%
  mutate(sensor.type = "magnetometer")

## 3g) MORTALITY STATUS ####
mortality_counts <- event_dat_all %>%
  filter(!is.na(mortality.status)) %>% 
  dplyr::select(individual.taxon.canonical.name, sc.dataset.id, sc.individual.id) %>%
  distinct() %>%
  arrange(individual.taxon.canonical.name, sc.individual.id) %>%
  summarize(n.indiv = nrow(.),
            percent.ind = round(n.indiv/tot_inds*100, digits = 1),
            n.studies = length(unique(sc.dataset.id)),
            n.species = length(unique(individual.taxon.canonical.name))) %>%
  mutate(sensor.type = "mortality")

## 3h) COMBINE SENSORS INTO ONE LIST ####
sensor_counts <- rbind(altitude_counts, pressure_counts, temp_counts, acc_counts, gyro_counts, mag_counts, mortality_counts) 
rm(altitude_counts, pressure_counts, temp_counts, acc_counts, gyro_counts, mag_counts, mortality_counts)

## 3i) SAVE SENSOR SUMMARY DATA ####
write.csv(sensor_counts, "./Data/Calculated/csv/06_sensor_counts.csv")
saveRDS(sensor_counts, "./Data/Calculated/csv/06_sensor_counts.rds")


# 4) COUNT SENSOR TYPES PER INDIVIDUAL ######################################################
## 4a) GET BIRDS WITH ADDITIONAL SENSOR DATA ####
ids <- event_dat_all %>% filter(!is.na(height.above.msl)| # The estimated height of the tag above mean sea level, typically estimated by the tag
                                  !is.na(height.above.ellipsoid)|#The estimated height above the ellipsoid, typically estimated by the tag
                                  !is.na(height.above.ground.level)|
                                  !is.na(barometric.pressure)|
                                  !is.na(external.temperature)|
                                  !is.na(acceleration.x)| #accelerometer
                                  !is.na(acceleration.raw.x)|
                                  !is.na(angular.velocity.x)| #gyroscope
                                  !is.na(magnetic.field.x)| #magnetometer
                                  !is.na(magnetic.field.raw.x)|
                                  !is.na(mortality.status)) %>% #mortality/motion sensor
  dplyr::select(individual.taxon.canonical.name, sc.dataset.id, sc.individual.id) %>%
  distinct() %>%
  pull(sc.individual.id)

## 4b) COUNT NUMBER & % OF INDIVIDUALS EXTRA SENSOR INFO ####
ids %>% length()
round((ids %>% length())/tot_inds*100, digits = 1)

## 4c) COUNT NUMBER OF SPECIES WITH ADDITIONAL SENSOR INFO ####
sp_sensors <- event_dat_all %>% filter(!is.na(height.above.msl)| # The estimated height of the tag above mean sea level, typically estimated by the tag. 
                                         !is.na(height.above.ellipsoid)|#The estimated height above the ellipsoid, typically estimated by the tag.  ) %>% 
                                         !is.na(height.above.ground.level)|
                                         !is.na(barometric.pressure)|
                                         !is.na(external.temperature)|
                                         !is.na(acceleration.x)| #accelerometer
                                         !is.na(acceleration.raw.x)|
                                         !is.na(angular.velocity.x)| #gyroscope
                                         !is.na(magnetic.field.x)| #magnetometer
                                         !is.na(magnetic.field.raw.x)|
                                         !is.na(mortality.status) & species.code != "UNDO") %>%
  distinct(species.code)

sp_sensors %>% 
  count() %>%
  mutate(n_sp_sensor = n,
         percent_sp_sensor = round((n_sp_sensor)/tot_sp_count*100, digits = 1))

## 4d) FOR EACH ID DETERMINE WHICH SENSORS ####
bird_dat_sensors <- event_dat_all %>%
  filter(sc.individual.id %in% ids) %>%
  dplyr::select(sc.individual.id, sc.dataset.id, individual.taxon.canonical.name,
                height.above.msl, height.above.ellipsoid, 
                height.above.ground.level, 
                barometric.pressure, external.temperature, 
                acceleration.x, acceleration.raw.x,
                angular.velocity.x,
                magnetic.field.x, 
                magnetic.field.raw.x,
                mortality.status) %>%
  mutate(across(where(is.numeric), ~case_when(!is.na(.) ~ 1, .default = NA))) %>%
  mutate(altitude.sensor = case_when(height.above.msl|height.above.ellipsoid|height.above.ground.level == 1 ~ 1, .default = NA)) %>% #
  dplyr::select(-c(height.above.msl, height.above.ellipsoid, height.above.ground.level)) %>% 
  mutate(accelerometer.sensor = case_when(acceleration.raw.x | acceleration.x == 1 ~ 1, .default = NA)) %>%
  dplyr::select(-acceleration.raw.x, -acceleration.x) %>%
  mutate(magnetometer.sensor = case_when(magnetic.field.x | magnetic.field.raw.x == 1 ~ 1, .default = NA)) %>%
  dplyr::select(-magnetic.field.x, -magnetic.field.raw.x) %>%
  rename(bar.pressure.sensor = barometric.pressure, 
         ex.temp.sensor = external.temperature,
         gyroscope.sensor = angular.velocity.x,
         mortality.sensor = mortality.status) %>%
  pivot_longer(cols = contains("sensor"), names_to = "sensor", values_to = "observation") %>%
  distinct(sc.individual.id, sc.dataset.id, individual.taxon.canonical.name, sensor, observation) %>%
  filter(observation == 1) %>%
  pivot_wider(names_from = sensor, values_from = observation)

## 4e) SAVE ####
write.csv(bird_dat_sensors, "./Data/Calculated/csv/06_individual_sensor_observations.csv")
saveRDS(bird_dat_sensors, "./Data/Calculated/rds/06_individual_sensor_observations.rds")
