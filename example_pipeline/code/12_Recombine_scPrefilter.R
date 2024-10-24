# CODE: 12_Recombine_scPrefilter.R
# CODE PURPOSE: Combine Argos & GPS data again after flagging them for truncation
# Apply SC prefilter
# Save both the pre-filtered obs (keeps)
# And pre-filtered obs dropped (drops)

# 0) SET WORKING DIRECTORY ##############################
setwd()
Sys.setenv(TZ = 'UTC')


# 1) LOAD PACKAGES ###########################################
library(tidyverse)
library(sf)
library(janitor)
source("./Code/Functions/named_group_split.r")
sf_use_s2(FALSE)
# Specify map projection: laea with revised lon (important for mapping birds crossing the international dateline)
sc_crs <- "+proj=laea +lon_0=-101.6 +lat_0=19.22 +datum=WGS84 +units=m +no_defs"


# 2) LOAD DATA ############################################
## 2a) CONTAINS ARGOS ####
dat_argos_lst_updated <- readRDS("./Data/Cleaned/11_event_dat_FLAGGED_DC_DEPS_ARGOS_TRUNC.rds")

## 2b) GPS ONLY ####
dat_gps_lst_updated <- readRDS("./Data/Cleaned/11_event_dat_FLAGGED_DC_DEPS_GPS_TRUNC.rds")

## 2c) REFERENCE DATA ####
refdat_lst <- readRDS("./Data/Cleaned/09_refdat_DC_DEPS.rds")


# 3) RECOMBINE #################################################
## 3a) CONVERT TO DATA FRAMES ####
argos_df <- dat_argos_lst_updated %>% map_dfr(~.x %>% as.data.frame()) #df
gps_df <- dat_gps_lst_updated %>% map_dfr(~.x %>% as.data.frame()) #df

## 3b) COMBINE ROWS ####
event_full_df <- bind_rows(argos_df, gps_df)

## 3c) ARRANGE / FORMAT DATA ####
# Arrange by sc.dataset.id, sc.deployment id, timestamp
event_full_df <- event_full_df %>% arrange(sc.dataset.id, sc.deployment.id, timestamp) %>%
            select(-elapsed.days, -elapsed.days.fact, -n.visible.detections) %>%
  relocate(starts_with("filter"), .after = last_col()) %>%
  relocate(any.failed.sc.filters, .after = last_col()) %>%
  mutate(any.failed.sc.filters = case_when(is.na(any.failed.sc.filters) ~ as.logical("FALSE"), #update so that deploy on/off locations aren't removed unless previously flagged
                                           .default = any.failed.sc.filters))

## 3d) CLEANUP ####
rm(argos_df)
rm(gps_df)
rm(dat_argos_lst_updated, dat_gps_lst_updated)


# 4) SAVE AND FORMAT ##############################################
## 4a) SAVE FLAGGED POINTS PRIOR TO ANIMOTUM PREFILTER ####
# Convert to camel case because fewer problems in ArcPro:
event_full_sf <- event_full_df %>%
  filter(!(filter.incomplete.case == 0| # remove points that cannot be plotted
             filter.bad.lat == 0 | 
             filter.bad.lon == 0)) %>%
  st_as_sf(coords = c("location.long", "location.lat"),
           crs = "+proj=longlat +datum=WGS84") %>% #convert to sf 
  dplyr::mutate(location.long = sf::st_coordinates(.)[,1],
                location.lat = sf::st_coordinates(.)[,2]) %>% # Convert to sf and add coordinates back as columns on here
  select(species.scDatasetID, event.id, visible,
         timestamp, 
         location.long, location.lat, individual.id, deployment.id,
         tag.id, study.id, sensor.type.id, individual.local.identifier,
         tag.local.identifier, individual.taxon.canonical.name,
         argos.lc, sensor.types.detected, 
         species.code, sc.dataset.id, sc.individual.id, sc.deployment.id, 
         data.receipt.type, sc.individual.notes, 
         estimated.duty.cycle.description, contains("filter")) %>%
  rename_with(., .fn = ~ snakecase::to_lower_camel_case(.)) %>%
  mutate(movebankDeploymentId = as.character(deploymentId), #more informative names
         movebankIndId = as.character(individualId),
         movebankStudyId = as.character(studyId))

### EXPORT AS GEOPACKAGE ####
# Necessary for loading into arcpro when exceeding the 2GB file size limit of .shp
# Will need to set projection in arc to laea with revised meridian and lat coords
st_write(event_full_sf, "./Data/Geopackage/POINTS_latlon.gpkg", "original_points", append = FALSE)

### CREATE TRACKLINES AND EXPORT FOR ARCPRO ####
# If in lat/long in geopackage will be wonky around the international date line
tracks_sf_ea <- event_full_sf %>%
  st_transform(crs = sc_crs) %>% # Project to laea with revised meridian 
  group_by(scDeploymentId) %>%
  summarize(do_union = FALSE) %>%
  st_cast("MULTILINESTRING")

### SAVE TRACK LINES AS SHAPEFILE ####
st_write(tracks_sf_ea, "./Data/Cleaned/shp/12_TRACKS_unfiltered.shp", append = FALSE)

## 4b) SPLIT INTO SEPARATE DF BY DATASET ####
# Helps keep things organized
event_full_lst <- named_group_split(event_full_df, sc.dataset.id) 
event_full_lst <- event_full_lst %>% purrr::map(~.x %>%
                                         as.data.frame() %>%
                                         arrange(sc.deployment.id, timestamp))


# 5) UPDATE REFERENCE DATA FOR TAG OFF TYPE ############################################
## 5a) GET IDs of DEPLOYMENTS WHERE WE FLAGGED POINTS FOR REMOVAL AT END ####
truncated_IDs <- event_full_lst %>%
  purrr::map_dfr(~.x %>% filter(filter.sc.manual.truncation.tail == 0 |
                              filter.detection.after.2wk.gap == 0 |
                                filter.detection.after.1wk.gap == 0 |
                                filter.detection.after.4wk.gap == 0) %>%
               select(sc.deployment.id) %>% 
               distinct()) %>%
  pull()

## 5b) GET REF DATA SUPPLIED END POINT INFO ####
movebank_dep_end_comments <- refdat_lst %>% 
  purrr:::map(~.x %>% 
                         select(sc.deployment.id, 
                                deployment.end.type, 
                                deployment.end.comments,
                                tag.failure.comments,
                                animal.death.comments) %>%
                         mutate(across(.cols = everything(), .fns = ~replace(., . ==  "" , NA))) %>%
                         filter(!(is.na(deployment.end.type) &
                                    is.na(deployment.end.comments) &
                                    is.na(tag.failure.comments) &
                                    is.na(animal.death.comments))) %>%
                         distinct() %>%
                         mutate(sc.deployment.end.type = 
                                  case_when(!is.na(animal.death.comments) ~ "dead", 
                                            tag.failure.comments == "Battery failure" ~ "equipment-failure",
                                            sc.deployment.id %in% truncated_IDs ~ "sc-truncated",
                                            .default = deployment.end.type)) %>%
                           mutate(sc.deployment.end.type = case_when(is.na(sc.deployment.end.type) ~ "unknown",
                                                                     .default = sc.deployment.end.type)) %>%
                           unite("movebank.deployment.end.comments", c("animal.death.comments", "tag.failure.comments", 
                                                                             "deployment.end.comments"),
                                                                     sep = ";", remove = TRUE, na.rm = TRUE) %>%
                           select(-deployment.end.type))

## 5c) ADD BACK TO REFERENCE DATA ####
refdat_lst_end <- map2(refdat_lst, movebank_dep_end_comments, ~left_join(.x, .y)) #Joining with `by = join_by(sc.deployment.id)`

## 5d) SAVE UPDATED METADATA FOR TAG END COMMENTS ####
saveRDS(refdat_lst_end, "./Data/Cleaned/12_refdat_DC_DEPS_ENDTYPE.rds")


# 6) APPLY SC PREFILTER #####################################################
# Aka remove flagged obs:
## 6a) GRAB KEEPS: (EVENTS THAT PASSED ALL PREFILTERS) ####
dat_scpf_keeps_lst <- event_full_lst %>% 
  purrr::map(~.x %>% 
    filter(any.failed.sc.filters != "TRUE"))


# 7) ADD SPEED, DISTANCE, ANGLE, MEASUREMENTS TO DATA THAT PASSED SC FREFILTER #####################
# For reference if cross-checking different prefilter options
dat_scpf_keeps_lst <- dat_scpf_keeps_lst %>%
  purrr::map(~.x %>%
               group_by(sc.deployment.id) %>%
               mutate(location.lat2 = lead(location.lat), #add next location in 
                      location.long2 = lead(location.long),
                      dist.to.next.m = round(geosphere::distGeo(
                        p1 = cbind(location.long, location.lat),
                        p2 = cbind(location.long2, location.lat2)), digits = 1),
                      dist.to.next.km = round(dist.to.next.m/1000, digits = 1),
                      time.to.next.sec = round(as.numeric(lead(timestamp)-timestamp)*60, digits = 1),
                      speed.to.next.ms = round(dist.to.next.m/time.to.next.sec, digits = 2),
                      sc.test.speed.filter = case_when(speed.to.next.ms > 42 ~ "FAST SPEED", .default = "REALISTIC SPEED")) %>% #note, this is does not use the macconnell method
               ungroup() %>%
               mutate(date = timestamp) %>% #to calculate angles in bayesmove
               bayesmove::prep_data(dat = ., 
               coord.names = c("location.long","location.lat"), 
               id = "sc.deployment.id") %>%
               mutate(turning.angle = round(angle*180/pi, digits = 1)) %>% 
               mutate(date = lubridate::date(timestamp)) %>%
               select(-step, -angle, -NSD, -dt) %>%
               rename(location.long = x,
                      location.lat = y))


# 8) SAVE KEEPS ###################################################
## 8a) RDS ####
saveRDS(dat_scpf_keeps_lst, "./Data/Cleaned/12_event_dat_SCPREFILTER_KEEPS.rds")

## 8b) AS GEOPACKAGE ####
### CONVERT TO CAMELCASE AND BASIC INFO AND DIST/TURN ANGLE INFO ####
keeps_sf <- dat_scpf_keeps_lst %>%
  map_df(~.x %>% as.data.frame() %>%
  st_as_sf(coords = c("location.long", "location.lat"),
           crs = "+proj=longlat +datum=WGS84") %>% #convert to sf
  dplyr::mutate(location.long = sf::st_coordinates(.)[,1],
                location.lat = sf::st_coordinates(.)[,2]) %>%# Convert to sf and add coordinates back as columns on here
  select(species.scDatasetID, event.id, visible,
         timestamp, 
         location.long, location.lat, individual.id, deployment.id,
         tag.id, study.id, sensor.type.id, individual.local.identifier,
         tag.local.identifier, individual.taxon.canonical.name,
         argos.lc, sensor.types.detected, 
         species.code, sc.dataset.id, sc.individual.id, sc.deployment.id, 
         data.receipt.type, sc.individual.notes, 
         estimated.duty.cycle.description, contains("filter"),
         dist.to.next.km, 
         time.to.next.sec, speed.to.next.ms,
         turning.angle) %>%
  rename_with(., .fn = ~ snakecase::to_lower_camel_case(.)) %>%
  mutate(movebankDeploymentId = as.character(deploymentId),
         movebankIndId = as.character(individualId),
         movebankStudyId = as.character(studyId)) %>%
    select(-deploymentId, -individualId, -studyId))

### EXPORT AS GEOPACKAGE LAYER ####
# Allows us to overlay original points, with points passing the SC prefilter
st_write(keeps_sf, "./Data/Geopackage/POINTS_latlon.gpkg", "scprefiltered_points", append = FALSE)

### CREATE TRACKLINES AND EXPORT FOR ARCPRO ####
keeps_tracks_sf_ea <- keeps_sf %>%
  st_transform(crs = sc_crs) %>%
  group_by(scDeploymentId) %>%
  summarize(do_union = FALSE) %>%
  st_cast("MULTILINESTRING")

### GET DISTINCT METADATA TO ADD TO TRACKS ####
metadata <- keeps_sf %>%
  st_drop_geometry() %>%
  select(individualLocalIdentifier,
         individualTaxonCanonicalName,
         sensorTypesDetected,
         speciesCode, # This was added with sc metadata
         scDatasetId, 
         scIndividualId,
         scDeploymentId,
         movebankDeploymentId,
         movebankIndId,
         movebankStudyId) %>%
  distinct()

# COMBINE WITH TRACKING LINES:
keeps_tracks_sf_ea <- left_join(keeps_tracks_sf_ea, metadata)

### SAVE TRACKS AS SHAPEFILE ####
st_write(keeps_tracks_sf_ea, "./Data/Cleaned/shp/12_TRACKS_scprefiltered.shp", append = FALSE)
