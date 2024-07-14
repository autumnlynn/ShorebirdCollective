# CODE: 15_aniMotum_Metadata.R
# CODE PURPOSE: ADD METADATA INTO THE MODELED ANIMOTUM LOCATIONS...
# AND FORMAT FOR DATA VIEWER


# 0) SET WORKING DIRECTORY ###############################
setwd()
Sys.setenv(TZ = 'UTC')


# 1) LOAD PACKAGES #########################################################
library(tidyverse)
library(sf)
sf_use_s2(FALSE)
# Specify map projection
sc_crs <- "+proj=laea +lon_0=-101.6 +lat_0=19.22 +datum=WGS84 +units=m +no_defs"


# 2) LOAD MODELED DATA #####################################################
mod_dat_sf_lst <- readRDS("./Data/Modeled/14_MODELED_SF.rds")


# 3) FORMAT MODELED DATA ##################################################
## 3a) UNLIST INTO ONE SF ####
mod_single_sf <- dplyr::bind_rows(mod_dat_sf_lst) # Note, only works if data are the same projection (all are bc consistent foieGras world mercator output) 

## 3b) RE-ADD MONTH AND YEAR IDENTIFIERS ####
mod_single_sf <- mod_single_sf %>%
  mutate(detection.date = as.Date(lubridate::date(timestamp)),
         detection.year = lubridate::year(timestamp),
         detection.week = lubridate::week(timestamp),
         detection.week.since.jul1 = case_when(detection.week >= 26 ~ (detection.week - 26), 
                                                            detection.week < 27 ~ (detection.week + 27), .default = NA), #0 will be july 1 on non-leap years
         detection.month = lubridate::month(timestamp),
         detection.month.since.jul1 = case_when(detection.month >= 7 ~ (detection.month - 7), 
                                                detection.month < 7 ~ (detection.month + 5), .default = NA), #0 will be july 1 on non-leap years
         detection.doy = lubridate::yday(timestamp),
         detection.doy.since.jul1 = case_when(detection.doy >= 182 ~ (detection.doy - 182), 
                                              detection.doy < 182 ~ (detection.doy + 182), .default = NA), #0 will be july 1 on non-leap years
         detection.time = format(timestamp,"%H:%M:%S")) %>%
         group_by(sc.deployment.id) %>%
         arrange(timestamp) %>%
         mutate(first.timestamp = head(timestamp, n = 1)) %>%
         ungroup() %>%
         mutate(days.since.first.detection = round(as.numeric(difftime(timestamp, first.timestamp, units = "days")), 0)) %>%
         select(-first.timestamp) 
rm(mod_dat_sf_lst)


# 4) ADD TAG METADATA #########################################################
# Add metadata to points so they are directly linked with point data and visible when exploring data for different applications
## 4a) LOAD ####
refdat_lst <- readRDS("./Data/Cleaned/12_refdat_DC_DEPS_ENDTYPE.rds")

## 4b) FORMAT DETECT METADATA ####
refdat_df <- refdat_lst %>%
  purrr::map_dfr(~.x %>% select(sc.dataset.id,
                                        sc.deployment.id,
                                        sc.individual.id,
                                        study.id,
                                        deployment.id,
                                        animal.taxon.canonical.name,
                                        animal.local.identifier,
                                        sensor.types.detected,
                                        tag.manufacturer.name,
                                        tag.model,
                                        tag.weight,
                                        estimated.duty.cycle.days,
                                        estimated.duty.cycle.hrs,
                                        estimated.duty.cycle.mins,
                                        estimated.duty.cycle.description, 
                                        contributed.duty.cycle,
                                        deploy.on.timestamp,
                                        deploy.off.timestamp,
                                        deployment.location.provided,
                                        movebank.deployment.end.comments, 
                                        sc.deployment.end.type,
                                        species.code, 
                                        contains("dsa"),
                                        additional.restrictions,
                                        obscuration.100.km.yn,
                                        contains("contact"), 
                                        co.owners.description) %>% 
               mutate(interpolated.timestep = NA,
                      interpolated.yn = "no",
                      deployment.on.time.available = case_when(deploy.on.timestamp != "" ~ "no", 
                                                               is.na(deploy.on.timestamp) ~ "no",
                                                               .default = "yes"),
                      deployment.off.time.available = case_when(deploy.off.timestamp != "" ~ "no", 
                                                              is.na(deploy.off.timestamp) ~ "no",
                                                              .default = "yes")) %>%
               rename(scientific.name = animal.taxon.canonical.name,
                      individual.local.identifier = animal.local.identifier) %>%
               select(-deploy.on.timestamp, -deploy.off.timestamp) %>%
               distinct())

## 4c) MERGE META WITH MODELED ####
mod_single_sf_meta <- left_join(mod_single_sf, refdat_df) #Joining with `by = join_by(sc.deployment.id, sc.dataset.id)`
rm(mod_single_sf, refdat_df)


# 5) ADD ADDITIONAL HELPFUL COLUMNS ##############################################
## 5a) FIRST AND LAST DETECTION IDENTIFIER, PLUS MORE ####
mod_single_sf_meta <- mod_single_sf_meta %>% 
  group_by(sc.deployment.id) %>%
  arrange(fg.modeled.seq.id) %>% 
  mutate(first.last.detection = ifelse(fg.modeled.seq.id == 1, "first_detection",
                                       ifelse(row_number() == n(), "last_detection", NA)),
         deployment.location.yn = case_when(first.last.detection == "first_detection" ~ "yes",
                                         .default = "no"), 
         deployment.location.origin = case_when(fg.modeled.seq.id == 1 &
                                                  deployment.location.provided == "yes" ~ "contributor specified", 
                                                fg.modeled.seq.id == 1 & 
                                                  deployment.location.provided == "no" ~ "sc estimated", 
                                                .default = NA)) %>%
  ungroup() %>% 
  arrange(sc.deployment.id, fg.modeled.seq.id) %>%
  mutate(pipeline.version.code = "2024_06_01_v2") %>% # data download version
  relocate(scientific.name, species.code, sensor.types.detected, study.id, deployment.id, individual.local.identifier, fg.modeled.seq.id) %>% 
  relocate(detection.date, .after = timestamp) %>%
  relocate(detection.year, .after = timestamp) %>%
  relocate(detection.month, .after = timestamp) %>%
  relocate(detection.week, .after = timestamp) %>%
  relocate(sc.deployment.id, .after = species.code) %>%
  relocate(sc.individual.id, .after = species.code) %>%
  relocate(sc.dataset.id, .after = species.code)
rm(am_full_meta_sf)

## 5b) CONVERT TO LAT/LONG ####
mod_single_sf_meta_ll  <- st_transform(mod_single_sf_meta, "EPSG:4326")  

## 5c) ADD LAT/LONG COORDS AS COLUMNS ####
mod_single_sf_meta_ll <- mod_single_sf_meta_ll %>%
  dplyr::mutate(location.lon = sf::st_coordinates(.)[,1],
                location.lat = sf::st_coordinates(.)[,2])

## 5d) REPROJECT ####
mod_single_sf_meta_ea <- st_transform(mod_single_sf_meta_ll, sc_crs)  

## 5e) SAVE ####
saveRDS(mod_single_sf_meta_ea, "./Data/Modeled/15_MODELED_SF_META.rds")


# 6) ADJUST FORMATTING FOR ARCPRO ##################################
## 6a) SUBSET AND CONVERT TO CAMEL CASE ####
ap_sf_ll <- mod_single_sf_meta_ll %>%
         select(-c(scientific.name, 
                   x.se, y.se,
                   estimated.duty.cycle.days, 
                   estimated.duty.cycle.mins,
                   estimated.duty.cycle.hrs,
                   deployment.location.provided,
                   deployment.on.time.available,
                   deployment.off.time.available)) %>%
         rename(movebank.deployment.id = deployment.id,
                movebank.study.id = study.id) %>%
  rename_with(., .fn = ~ snakecase::to_lower_camel_case(.)) %>%
  mutate(detectionWeek = as.character(detectionWeek), 
         detectionMonth = as.character(detectionMonth),
         detectionYear = as.character(detectionYear),
         detectionWeekSinceJul1 = as.character(detectionWeekSinceJul1), 
         detectionMonthSinceJul1 = as.character(detectionMonthSinceJul1),
         movebankDeploymentId = as.character(movebankDeploymentId),
         movebankStudyId = as.character(movebankStudyId))

## 6b) SAVE AS GEOPACKAGE LAYER ####
st_write(ap_sf_ll, "./Data/Modeled/MODELED_POINTS_latlon.gpkg", "modeled_points", append = FALSE)

