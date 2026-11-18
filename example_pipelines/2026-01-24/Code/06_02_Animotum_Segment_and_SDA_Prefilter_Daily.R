# CODE: 06_02_Animotum_Segment_and_SDA_Prefilter_Daily.R
# CODE PURPOSE: SEGMENT DATA AFTER LARGE GAPS AND PREFILTER SEGMENTED DATA WITH SDA FILTER 
# This code is an alternative to 06_01_Animotum_SDA_Prefilter.R
# 06_02 prefilters the data in aniMotum after segmenting the data after large data gaps and then runs SDA prefilters in aniMotum
# Segmenting is necessary prior to predicting data to daily locations because the model will continue on and on to predict during large gaps
#
# This code splits tracks into separate segments (following gaps > 15 days) for estimating daily locations
## 15 days was chosen to try to reduce the number of segments per track 
## In this dataset some tags recorded locations weekly, so a two week gap could arise if a location was missed
## In the future, its more suitable that those datasets are run through the SSM with an interval of 7 days
# And prefilters data with speed, distance, angle filters for the different tracking data types (argos only, gps only, or both argosgps)

# 00) SETUP ####
data_version <- "2026-01-24"

## SET WORKING DIRECTORY ##############################
setwd()
Sys.setenv(TZ = 'UTC')


# 1) LOAD PACKAGES ###########################################
library(tidyverse)
library(aniMotum)
library(conflicted)
conflict_prefer("filter", "dplyr")
library(cowplot)
library(sf)

## LOAD CUSTOM FUNCTIONS ####
source("./Code/Functions/split_at_gap.r")
source("./Code/Functions/get_sda_prefiltered_daily_data.r")
source("./Code/Functions/remove_sda_segment_probs.r")


# 2) LOAD DATA ############################################
event_sc_prefiltered_sf <- readRDS(paste0("./Data/rds/04_POINTS_scprefiltered_sf_",data_version,".rds"))


# 3) SEGMENT DATA #####################################################
## 3a) SPLIT INTO SEPARATE SEGMENTS WHEN THERE ARE GAPS LONGER THAN 15 DAYS AND KEEP THOSE WITH AT LEAST 6 DAYS OF TRACKING ####
segment_df <- split_at_gap(event_sc_prefiltered_sf %>%
                           st_drop_geometry(), 
                           deployment_col = "sc_deployment_id",
                           max_gap = 15, # 15 days
                           shortest_track = 6) # 6 days minimum

## 3b) EXTRACT METADATA FOR LATER MERGING ####
metadata_to_add <- segment_df %>%
  st_drop_geometry() %>%
  select(species_code, 
         sc_dataset_id, sc_individual_id, sc_deployment_id, segment_id,
         individual_local_identifier, sc_tracking_data_type, 
         movebank_study_id,
         movebank_individual_id,
         movebank_deployment_id) %>%
  distinct()

### SAVE FOR AFTER ANIMOTUM ####
saveRDS(metadata_to_add, paste0("./Data/rds/06_animotum_daily_metadata_",data_version,".rds"))


# 4) FORMAT DATA FOR aniMotum PREFILTERING ###################################################
## 4a) FORMAT COLUMNS ####
dat_am_df <- segment_df %>% 
  st_drop_geometry() %>%
  arrange(segment_id, timestamp) %>%
  select(segment_id, timestamp, 
         argos_lc, location_long, location_lat, 
         argos_semi_major, argos_semi_minor, argos_orientation, 
         sc_tracking_data_type) %>% #select input variables required for animotum
  rename(id = segment_id, 
         date = timestamp, 
         lc = argos_lc, 
         lon = location_long, 
         lat = location_lat, 
         smaj = argos_semi_major, 
         smin = argos_semi_minor, 
         eor = argos_orientation) %>% # rename to match argos format
  mutate(lc = as.factor(lc), id = as.character(id)) %>% #Id has to be a character to run through ssm
  droplevels() 

## 4b) REMOVE SEGS WITH TOO FEW OBSERVATIONS TO RUN THROUGH ANIMOTUM PREFILTER ####
# At least 3 points are necessary to run through the prefilter
# Note: consider a higher threshold as tracks with 3 points are generally not informative
#
### CHECK NUMBER OF OBSERVATIONS PER SEG #### 
dat_am_df %>%
  group_by(id) %>%
  count() %>%
  filter(n < 3) %>%
  print(n = Inf) 

### GET IDS TO DROP ####
seg_id_less3 <- dat_am_df %>%
  group_by(id) %>%
  count() %>%
  filter(n < 3) %>%
  pull(id)

### DROP THESE DEPS FROM DATA ####
dat_am_df_less3 <- dat_am_df %>%
  filter(!id %in% seg_id_less3) 

## 4b) SPLIT BY DATA TYPE RECORDED BY TAG (SC_TRACKING_DATA_TYPE) ####
# Allows for running different prefilters on tags collecting different types of data
dat_lst_sensors <- split(dat_am_df_less3, dat_am_df_less3$sc_tracking_data_type) %>%
  purrr::map(~.x %>% select(-sc_tracking_data_type))
rm(dat_am_df, dat_am_df_less3)

## 4c) CONVERT TO SF AND SPECIFY LL FORMAT ####
# This basic lat/long is the format of data coming out of Movebank
# If a projection is not specified, had issues for birds crossing the international dateline
dat_lst_sensors_sf <- dat_lst_sensors %>%
  purrr::map(~sf::st_as_sf(.x, coords = c("lon","lat"), 
                           crs="+proj=longlat +datum=WGS84")) # specify this is lat long format

## 4d) SET PROJECTION TO GLOBAL MERCATOR WITH 180 MERIDIAN ####
# This is the projection aniMotum uses...
# Need to specify *specifically* the 180 meridian otherwise international dateline problems will arise
# https://github.com/ianjonsen/aniMotum/blob/main/R/pf_sf_project.R (this is how aniMotum formats data that cross the dateline)
dat_lst_sensors_sf_merc180 <- dat_lst_sensors_sf %>%
  purrr::map(~.x %>%
               st_transform("+proj=merc +lon_0=180 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs"))


# 5) PREFILTER DATA THROUGH ANIMOTUM ###################################
## 5a) GPS ONLY ####
## Use SDA filter on these data (important because there can be errors in GPS data causing large outlier spikes)
# Points beyond 50 - 60 km are removed if they form tight turning angles
# Allows unrealistic flight speeds locally (within 60 km); fast speeds beyond are removed
# In the future, should consider a standard speed filter for these data as well to remove unrealistic local speeds
dat_prefilter_sda_gps <-  aniMotum::fit_ssm(dat_lst_sensors_sf_merc180$gps, 
                                            vmax = 42,  #m/s (speed from Gronroos radar study)
                                            ang = c(15,25), #c(15,25) DEFAULT (applies SDA filter in argos package); NA runs speed filter only
                                            distlim = c(50000, 60000), #default is c(2500, 5000); NA shuts off SDA in favor of speed filter
                                            # expands distance limits to remove spikes beyond 50-60km (spikes patterns were realistic sometimes up to 50km)
                                            spdf = TRUE, # FALSE shuts off speed filter
                                            min.dt = 0, # 0 second minimum allowable time between locs
                                            model = "rw", # random walk
                                            pf = TRUE, # run prefilter (not ssm)
                                            time.step = 24, # returns locations every 24
                                            fit.to.subset =  FALSE, # Run prefilter only; TRUE runs aniMotum right away after prefiltering 
                                            control = ssm_control(verbose = 0))
gc()

## 5b) ARGOS/GPS COMBO ####
## Use SDA filter tags collecting both argos and gps data
# Allows unrealistic flight speeds at shorter distances (up to 10 km)
# Points beyond 5 km to 10 km are removed if they form an angle of < 5 and < 45 respectively
# Larger angles further out removes observed patterns of large outliers
# Unrealistic speeds removed beyond 10km
dat_prefilter_sda_argosgps <-  aniMotum::fit_ssm(dat_lst_sensors_sf_merc180$argosgps, 
                                                 vmax = 42, # m/s (speed from Gronroos radar study)
                                                 ang = c(5,45),  # broadened outer angle to account for key problems identified by visual inspection 
                                                 distlim = c(5000, 10000), # default is c(2500, 5000); broadened outer filter for spikes beyond 10 km
                                                 spdf = TRUE, # FALSE shuts off speed filter
                                                 min.dt = 0, #0 second minimum allowable time between locs (allows for same timestamp to run through)
                                                 model = "rw", # random walk
                                                 pf = TRUE, # prefilter (not ssm)
                                                 time.step = 24, # returns locations every 24 hrs
                                                 fit.to.subset =  FALSE,  # Run prefilter only; TRUE runs aniMotum right away after prefiltering 
                                                 control = ssm_control(verbose = 0))
gc()

## 5c) ARGOS ONLY ####
# This uses the default DSA Argos filter settings for the first angles but is stricter beyond 10 km 
# Visual inspection showed patterns up to 45 degrees causing problems beyond 10 km
dat_prefilter_sda_argos <-  aniMotum::fit_ssm(dat_lst_sensors_sf_merc180$argos, 
                                              vmax = 42, # m/s (speed from Gronroos radar study)
                                              ang = c(15,45),  # broadened outer angle angle further to account for key problems identified by visual inspection
                                              distlim = c(2500, 10000), #default is c(2500, 5000); broadened outer filter for spikes beyond 10 km
                                              spdf = TRUE, # FALSE shuts off speed filter
                                              min.dt = 0, # 0 second minimum allowable time between locs (allows for same timestamp to run through)
                                              model = "rw", # random walk 
                                              pf = TRUE, # run prefilter (not ssm)
                                              time.step = 24, #locations every 24 hours
                                              fit.to.subset =  FALSE, # Run prefilter only; TRUE runs aniMotum right away after prefiltering 
                                              control = ssm_control(verbose = 0))
gc()

## 5d) RE-LIST DATA ####
dat_sdaprefilter_lst <- lst(dat_prefilter_sda_argos, dat_prefilter_sda_argosgps, dat_prefilter_sda_gps)

## 5e) CONVERT BACK TO LAT AND LONG ####
dat_sdaprefilter_lst_ll <- dat_sdaprefilter_lst %>%
  purrr::map(~ .x %>% st_transform("EPSG:4326")) %>% # to match input ll
  purrr::map( ~.x %>% mutate(lon = unlist(purrr::map(.x$geometry,1)), #Add lat/lon as columns
                             lat = unlist(purrr::map(.x$geometry,2))))
# NOTE: 
# The output format of columns changes (eg eor seems to convert to radians, etc.)
# This can be a problem in CTMM later as it requires the input data columns in the original movebank format 


# 6) GET SDA PREFILTERED DATA ####################################################
## 6a) ADD SDA PREFILTER COLUMN TO ORIGINAL DATA AND ADD METADATA & FILTER ####
dat_sda_filtered_sf_meta <- get_sda_prefiltered_daily_data(output_df_lst = dat_sdaprefilter_lst, 
                                                     input_df_lst = dat_lst_sensors, 
                                                     metadata_df = metadata_to_add)
## 6b) SAVE PREFILTERED DATA ####
### RDS ####
saveRDS(dat_sda_filtered_sf_meta, paste0("./Data/rds/06_POINTS_sdaprefiltered_daily_sf_",data_version,".rds"))
gc()


# 7) FIND & REMOVE SHORT DURATION SEGMENTS & SEGMENTS WITH FEW LOCATIONS AFTER SDA (AGAIN) ####################################
# Remove segments after sda prefiltering if they have:
## a) <= 7 days tracking 
## b) <= 4 locations
# Note: this function is a slightly different approach than the original timestep script because it filters down the input dataframe for running the SSM in aniMotum

## 7a) CALCULATE AND REMOVE ####
segchecks_lst <- remove_sda_segment_probs(prefiltered_sda_daily_sf = dat_sda_filtered_sf_meta,
                         input_merc180_dat_lst = dat_lst_sensors_sf_merc180)

## 7b) GET INPUT DATA WITH PROB SEGS REMOVED ####
dat_lst_sensors_sf_merc180_checked <- segchecks_lst$cleaned_data

## 7c) SAVE SEGMENTS READY FOR ANIMOTUM SSM ####
saveRDS(dat_lst_sensors_sf_merc180_checked, paste0("./Data/rds/06_animotum_daily_input_segdat_",data_version,".rds"))

