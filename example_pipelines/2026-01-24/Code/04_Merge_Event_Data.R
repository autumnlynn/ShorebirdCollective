# CODE: 04_Merge_Event_Data.R
# CODE PURPOSE: MERGE EVENT DATA FROM MOVEBANK AND DATA DIRECTLY SENT TO SC AFTER FORMATTING AND FLAGGING
#
# INTERNALLY PRIOR TO THIS STEP, THE SHOREBIRD COLLECTIVE FORMATTED AND FLAGGED DATASET SENT DIRECTLY 
# ... to us from repositories other than Movebank.
# ... We formatted these datasets to match those downloaded from Movebank (data dictionary as of 2026-01-24)
# ... and we added columns sc_dataset_id, sc_individual_id, sc_deployment_id, sc_tracking_data_type 
# We then processed these data in the same way as the Movebank data in 02_Movebank_Format_Event_Data.R 

# 00) SETUP ####
data_version <- "2026-01-24"

## SET WORKING DIRECTORY ##############################
setwd()


# 1) LOAD PACKAGES #################################################
library(tidyverse)
library(janitor)
library(sf)
sf_use_s2(FALSE)
sc_crs <- "+proj=laea +lon_0=-101.6 +lat_0=19.22 +datum=WGS84 +units=m +no_defs"


## CUSTOM FUNCTIONS ####
source("./Code/Functions/format_merged_event.r")


# 2) LOAD DATA ####################################################
## 2a) MOVEBANK FLAGGED EVENT DATA ####
dat_merge_scids_df <- readRDS(paste0("./Data/rds/02_movebank_event_to_merge_",data_version,".rds")) 

## 2b) DIRECT SEND FLAGGED EVENT DATA ####
# rds file of merged and flagged data with origins outside of Movebank:
dat_ds_merge_df <- readRDS(paste0("./Data/rds/directsend_event_to_merge_",data_version,".rds"))

## 2c) COMPARE THE COLUMNS ####
compare_df_cols(dat_merge_scids_df, dat_ds_merge_df)


# 3) COMBINE TOGETHER #############################################
event_dat_df <- bind_rows(dat_merge_scids_df, dat_ds_merge_df) 

## 3a) ORGANIZE ####
event_dat_df <- format_merged_event(event_dat_df)

## 3b) SAVE AS RDS ####
saveRDS(event_dat_df, paste0("./Data/rds/04_merged_event_dat_",data_version,".rds"))

# CLEAN UP:
rm(dat_ds_merge_df, dat_merge_scids_df)


# 4) CONVERT ORIGINAL POINTS TO SF & SAVE ########################################################################
## 4a) ORIGINAL POINTS (PRIOR TO PREFILTER) ####
### REMOVE UNPLOTTABLE POINTS ####
event_orig_sf <- event_dat_df %>%
  filter(!(filter_incomplete_case == 0| # remove points that cannot be plotted
             filter_bad_lat == 0 | 
             filter_bad_lon == 0)) %>%
  st_as_sf(coords = c("location_long", "location_lat"),
           crs = "EPSG:4326") %>% #convert to sf
  # Convert to sf and add coordinates back as columns 
  dplyr::mutate(location_long = sf::st_coordinates(.)[,1],
                location_lat = sf::st_coordinates(.)[,2]) %>%
  select(-movebank_individual_taxon_canonical_name) # could lead to unexpected sorting because scientific names not consistent across all species in Movebank
event_orig_sf

### SAVE AS RDS ####
# RDS files load quicker than the geopackage files in r, so exporting for pipeline speed
saveRDS(event_orig_sf, paste0("./Data/rds/04_POINTS_original_sf_",data_version,".rds"))

### EXPORT AS GEOPACKAGE LAYER ####
# But export for exploration
st_write(event_orig_sf, 
         paste0("./Data/Geopackage/POINTS_latlon_",data_version,".gpkg"), 
         "original_points", append = FALSE)

### CREATE TRACKLINES ####
orig_tracks_sf_ea <- event_orig_sf %>%
  st_transform(crs = sc_crs) %>% 
  group_by(sc_deployment_id) %>%
  summarize(do_union = FALSE) %>%
  st_cast("MULTILINESTRING")

### GET DISTINCT METADATA TO ADD ####
metadata_to_add <- event_orig_sf %>%
  st_drop_geometry() %>%
  select(species_code, 
         sc_dataset_id, sc_individual_id, sc_deployment_id, 
         individual_local_identifier, sc_tracking_data_type, 
         movebank_study_id,
         movebank_individual_id,
         movebank_deployment_id) %>%
  distinct()

### ADD META TO TRACKS ####
orig_tracks_sf_ea <- left_join(orig_tracks_sf_ea, metadata_to_add) # Joining with `by = join_by(sc_deployment_id)`

### SAVE AS SHAPEFILE ####
# Export as shapefile because custom projections not supported in gpkg
# ...and projected tracks are helpful for visualizing birds crossing international dateline
# These tracklines will be a complete mess, but helpful for exploring how different filtering steps impact results
st_write(orig_tracks_sf_ea, paste0("./Data/shp/04_TRACKS_unfiltered_",data_version,".shp"), append = FALSE)


# 5) APPLY SHOREBIRD COLLECTIVE PRE-FILTER, CONVERT TO SF & SAVE #######################################
## 5a) FILTER TO POINTS THAT PASSED ALL PREFILTERS ####
event_sc_prefiltered_sf <- event_orig_sf %>% 
                filter(any_failed_sc_filters != "TRUE")

## 5b) SAVE ####
### RDS ####
saveRDS(event_sc_prefiltered_sf, paste0("./Data/rds/04_POINTS_scprefiltered_sf_",data_version,".rds"))

### EXPORT AS GEOPACKAGE LAYER ####
st_write(event_sc_prefiltered_sf, 
         paste0("./Data/Geopackage/POINTS_latlon_",data_version,".gpkg"), 
         "scprefiltered_points", append = FALSE)

## 5c) CREATE SC PRE-FILTERED TRACKLINES ####
tracks_sc_prefiltered_sf_ea <- event_sc_prefiltered_sf %>%
  st_transform(crs = sc_crs) %>%
  group_by(sc_deployment_id) %>%
  summarize(do_union = FALSE) %>%
  st_cast("MULTILINESTRING")

### META TO TRACKS ####
tracks_sc_prefiltered_sf_ea <- left_join(tracks_sc_prefiltered_sf_ea, metadata_to_add) # Joining with `by = join_by(sc_deployment_id)`

### SAVE AS SHAPEFILE ####
st_write(tracks_sc_prefiltered_sf_ea, paste0("./Data/shp/04_TRACKS_scprefiltered_",data_version,".shp"), append = FALSE)


# 6) EXPORT TAG ON/OFF LOCATIONS AS LAYER ###########################################################
## 6a) EXTRACT FIRST AND LAST DEPLOYMENT POINTS ####
deployment_locs_sf <- event_sc_prefiltered_sf %>%
  arrange(sc_deployment_id, timestamp) %>%
  group_by(sc_deployment_id) %>%
  slice(unique(c(1,n()))) %>% # the unique part of it selects ONLY one row if there is only a single point (these will drop later)
  mutate(sc_loc_deploy_on_off_yn = "yes",
         sc_loc_deploy_on_or_off = case_when( row_number() == 1 ~ "deploy_on_location",
                                    row_number() == n() ~ "deploy_off_location"),
         # If first/last deployments were not provided by the data owner, use the first/last location recorded by the tag and call the origin "sc_estimated"
         sc_on_off_origin = case_when(sc_location_origin %in% 
                                        c("directsend_event", "movebank_event") ~ "sc_estimated", 
                                      sc_location_origin %in% 
                                        c("deploy_on_lat_long", "deploy_off_lat_long") ~ "data_owner_provided")) %>% 
  ungroup() 

## 6b) SAVE ####
### RDS ####
saveRDS(deployment_locs_sf, paste0("./Data/rds/04_POINTS_deploy_onoff_sf_",data_version,".rds"))

### EXPORT AS GEOPACKAGE LAYER ####
st_write(deployment_locs_sf, 
         paste0("./Data/Geopackage/POINTS_latlon_",data_version,".gpkg"), 
         "deployment_onoff_points", append = FALSE)

