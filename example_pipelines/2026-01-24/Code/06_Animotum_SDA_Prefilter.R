# CODE: 06_Animotum_SDA_Prefilter.R
# CODE PURPOSE: RUN SC PRE-FILTERED TRACKING DATA THROUGH aniMotum FOR ADDITIONAL PREFILTERING
# EXPORT LOCATIONS THAT REMAIN AFTER ANIMOTUM PREFILTERING FOR VISUAL COMPARISON
# REMOVE A FEW INDIVIDUALS WITH SHORT TRACKS AFTER ANIMOTUM PREFILTERING
#
# The script runs separate prefilters for tags collecting argos data only, gps data only, and both argos and gps data
#
# Note: it is possible to run the prefilters directly in the script with the SSM (eg script 07_Animotum_SSM_Origts.r)...
# ...but it's nice to export the points that pass the prefilter so you know what points are directly informing the SSM
# This script can be used to identify points that the SC has marked as manually marked outliers in the script 03_Movebank_Format_Reference_Data.r
# ...essentially it's important that major spikes and outliers do not move through the SSM
#
# aniMotum isn't necessary for prefiltering (could use trip package instead), but...
# ...currently have to run animotum with a prefilter if inputting data in an sf format
# ...inputting data as sf was necessary for our dataset to specify a map projection suitable for birds crossing the international date line
# 
# Example vignettes for aniMotum can be found here: animotum: https://github.com/ianjonsen/aniMotum


# 00) SETUP ####
data_version <- "2026-01-24"

## SET WORKING DIRECTORY ##############################
setwd()
Sys.setenv(TZ = 'UTC')


# 1) LOAD PACKAGES #################################################
library(tidyverse)
# install aniMotum # 1.2-14
# install.packages("aniMotum",
#                  repos = c("https://cloud.r-project.org",
#                            "https://ianjonsen.r-universe.dev"),
#                  dependencies = TRUE)
# Needed to update Rtools
#install.packages('TMB', type = 'source')
library(aniMotum)
library(conflicted)
conflict_prefer("filter", "dplyr")
library(cowplot)
library(sf)
sf_use_s2(FALSE)
sc_crs <- "+proj=laea +lon_0=-101.6 +lat_0=19.22 +datum=WGS84 +units=m +no_defs"

### LOAD CUSTOM FUNCTIONS ####
source("./Code/Functions/flag_failed_sc_filters.r")
source("./Code/Functions/get_sda_prefiltered_data.r")
source("./Code/Functions/flag_sda_track_issues.r")


# 2) LOAD DATA ############################################
event_sc_prefiltered_sf <- readRDS(paste0("./Data/rds/04_POINTS_scprefiltered_sf_",data_version,".rds"))

# EXTRACT METADATA FOR LATER MERGING:
metadata_to_add <- event_sc_prefiltered_sf %>%
         st_drop_geometry() %>%
  select(species_code, 
         sc_dataset_id, sc_individual_id, sc_deployment_id, 
         individual_local_identifier, sc_tracking_data_type, 
         movebank_study_id,
         movebank_individual_id,
         movebank_deployment_id) %>%
  distinct()


# 3) FORMAT DATA FOR aniMotum PREFILTERING ###################################################
## 3a) FORMAT COLUMNS ####
dat_am_df <- event_sc_prefiltered_sf %>% 
  st_drop_geometry() %>%
  arrange(sc_deployment_id, timestamp) %>%
  select(sc_deployment_id, timestamp, 
         argos_lc, location_long, location_lat, 
         argos_semi_major, argos_semi_minor, argos_orientation, 
         sc_tracking_data_type) %>% #select input variables required for animotum
  rename(id = sc_deployment_id, 
                      date = timestamp, 
                      lc = argos_lc, 
                      lon = location_long, 
                      lat = location_lat, 
                      smaj = argos_semi_major, 
                      smin = argos_semi_minor, 
                      eor = argos_orientation) %>% # rename to match argos format
  mutate(lc = as.factor(lc), id = as.character(id)) %>% #Id has to be a character to run through ssm
  droplevels() 

## 3b) REMOVE DEPS WITH TOO FEW OBSERVATIONS TO RUN THROUGH ANIMOTUM PREFILTER ####
# At least 3 points are necessary to run through the prefilter
# Note: consider a higher threshold as tracks with 3 points are generally not informative
#
### CHECK NUMBER OF OBSERVATIONS PER DEPLOYMENT #### 
dat_am_df %>%
  group_by(id) %>%
  count() %>%
  filter(n < 3) %>%
  print(n = Inf) 

### GET IDS TO DROP ####
dep_id_less3 <- dat_am_df %>%
  group_by(id) %>%
  count() %>%
  filter(n < 3) %>%
  pull(id)

### DROP THESE DEPS FROM DATA ####
dat_am_df_less3 <- dat_am_df %>%
  filter(!id %in% dep_id_less3) 

## 3b) SPLIT BY DATA TYPE RECORDED BY TAG (SC_TRACKING_DATA_TYPE) ####
# Allows for running different prefilters on tags collecting different types of data
dat_lst_sensors <- split(dat_am_df_less3, dat_am_df_less3$sc_tracking_data_type) %>%
  purrr::map(~.x %>% select(-sc_tracking_data_type))
rm(dat_am_df, dat_am_df_less3)

## 3c) CONVERT TO SF AND SPECIFY LL FORMAT ####
# This basic lat/long is the format of data coming out of Movebank
# If a projection is not specified, I had issues for birds crossing the international dateline
dat_lst_sensors_sf <- dat_lst_sensors %>%
  purrr::map(~sf::st_as_sf(.x, coords = c("lon","lat"), 
                           crs="+proj=longlat +datum=WGS84")) # specify this is lat long format
# dat_lst_sensors_sf %>% names()

## 3d) SET PROJECTION TO GLOBAL MERCATOR WITH 180 MERIDIAN ####
# This is the projection aniMotum uses...
# Need to specify *specifically* the 180 meridian otherwise international dateline problems will arise
# https://github.com/ianjonsen/aniMotum/blob/main/R/pf_sf_project.R (this is how aniMotum formats data that cross the dateline)
dat_lst_sensors_sf_merc180 <- dat_lst_sensors_sf %>%
  purrr::map(~.x %>%
               st_transform("+proj=merc +lon_0=180 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs"))


# 4) PREFILTER DATA THROUGH ANIMOTUM ###################################
## 4a) GPS ONLY ####
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
                                            time.step = NA, # returns locations at observation times
                                            fit.to.subset =  FALSE, # Run prefilter only; TRUE runs aniMotum right away after prefiltering 
                                            control = ssm_control(verbose = 0))
gc()

## 4b) ARGOS/GPS COMBO ####
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
                                                 min.dt = 0, #0 second minimum allowable time between locs 
                                                 model = "rw", # random walk
                                                 pf = TRUE, # prefilter (not ssm)
                                                 time.step = NA, # returns locations at observation times
                                                 fit.to.subset =  FALSE,  # Run prefilter only; TRUE runs aniMotum right away after prefiltering 
                                                 control = ssm_control(verbose = 0))
gc()

## 4c) ARGOS ONLY ####
# This uses the default DSA Argos filter settings for the first angles but is stricter beyond 10 km 
# Visual inspection showed patterns up to 45 degrees causing problems beyond 10 km
dat_prefilter_sda_argos <-  aniMotum::fit_ssm(dat_lst_sensors_sf_merc180$argos, 
                                              vmax = 42, # m/s (speed from Gronroos radar study)
                                              ang = c(15,45),  # broadened outer angle angle further to account for key problems identified by visual inspection
                                              distlim = c(2500, 10000), #default is c(2500, 5000); broadened outer filter for spikes beyond 10 km
                                              spdf = TRUE, # FALSE shuts off speed filter
                                              min.dt = 0, # 0 second minimum allowable time between locs
                                              model = "rw", # random walk 
                                              pf = TRUE, # run prefilter (not ssm)
                                              time.step = NA, #locations at observation times
                                              fit.to.subset =  FALSE, # Run prefilter only; TRUE runs aniMotum right away after prefiltering 
                                              control = ssm_control(verbose = 0))
gc()

## 4d) RE-LIST DATA ####
dat_sdaprefilter_lst <- lst(dat_prefilter_sda_argos, dat_prefilter_sda_argosgps, dat_prefilter_sda_gps)

## 4e) CONVERT BACK TO LAT AND LONG ####
dat_sdaprefilter_lst_ll <- dat_sdaprefilter_lst %>%
  purrr::map(~ .x %>% st_transform("EPSG:4326")) %>% # to match input ll
  purrr::map( ~.x %>% mutate(lon = unlist(purrr::map(.x$geometry,1)), #Add lat/lon as columns
                             lat = unlist(purrr::map(.x$geometry,2))))
# NOTE: 
# The output format of columns changes (eg eor converts to radians, etc.)
# This can be a problem in CTMM later as it requires the input data columns in the original Movebank format, not in radians


# 5) GET SDA PREFILTERED DATA ####################################################
## 5a) ADD SDA PREFILTER COLUMN TO ORIGINAL DATA & ADD METADATA & FILTER ####
dat_sda_filtered_sf_meta <- get_sda_prefiltered_data(output_df_lst = dat_sdaprefilter_lst, 
                         input_df_lst = dat_lst_sensors, 
                         metadata_df = metadata_to_add)

## 5b) SAVE PREFILTERED DATA ####
### RDS ####
saveRDS(dat_sda_filtered_sf_meta, paste0("./Data/rds/06_POINTS_sdaprefiltered_sf_",data_version,".rds"))

### GEOPACKAGE ####
st_write(dat_sda_filtered_sf_meta, 
         paste0("./Data/Geopackage/POINTS_latlon_",data_version,".gpkg"), 
         "sdaprefiltered_points", append = FALSE)

## 5c) CREATE TRACKLINES ####
tracks_sdaprefiltered_sf_ea <- dat_sda_filtered_sf_meta %>%
  st_transform(crs = sc_crs) %>%
  group_by(sc_deployment_id) %>%
  summarize(do_union = FALSE) %>%
  st_cast("MULTILINESTRING")

### APPEND METADATA TO TRACKS ####
tracks_sdaprefiltered_sf_ea <- left_join(tracks_sdaprefiltered_sf_ea, metadata_to_add) # Joining with `by = join_by(sc_deployment_id)`

### SAVE AS SHAPEFILE ####
st_write(tracks_sdaprefiltered_sf_ea, paste0("./Data/shp/06_TRACKS_sdaprefiltered_",data_version,".shp"), append = FALSE)


# 6) FLAG SHORT DURATION TRACKS & TRACKS WITH FEW LOCATIONS AFTER SDA (AGAIN) ####################################
# Flag tag deployments in the SC PREFILTERED data for removal prior to officially running the SSM in aniMotum
# Remember that above we just pre-filtered the data to check it is appropriately removing major outliers
# In the next step, we need to officially run aniMotum (and right now we need to run aniMotum with the prefilter at the same time)
#
# This flags tag deployments with ...
# ...1) 1 week or less duration
# ...2) and less than 5 locations 
# ... AFTER pre-filtering
# These tend to cause problems in SSMs
# In the future, could consider even tighter thresholds (especially if resampling data, this is a very small number of locations)
# Note: prior to ssm we filtered out deployments with less than 3 locations ...
# ... we could have made that value larger and potentially avoided some of this step
dat_sda_filtered_sf_meta %>% glimpse()

## 6a) APPLY FLAGS ####
event_sc_prefiltered_sf_sdaflags <- flag_sda_track_issues(sda_filtered_sf = dat_sda_filtered_sf_meta, 
                      sc_prefiltered_sf = event_sc_prefiltered_sf)

## 6b) SAVE SDA PREFILTERED DATA WITH NEW FLAGS ####
saveRDS(event_sc_prefiltered_sf_sdaflags, paste0("./Data/rds/06_POINTS_sdaflags_sf_",data_version,".rds"))
