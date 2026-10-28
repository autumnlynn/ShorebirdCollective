# CODE: 07_AniMotum_SSM_Origts.R
# CODE PURPOSE: RUN SC PRE-FILTERED TRACKING DATA THROUGH ANIMOTUM TO ESTIMATE SPATIAL ERROR and ...
# PREDICT LOCATIONS AT ORIGINAL TIME INTERVAL RECORDED BY THE TAG WHILE ACCOUNTING FOR SPATIAL ERROR
# Note: this runs tag deployments through aniMotum with > 7 days tracking and at least 5 locations after SDA prefiltering
# The current pipeline requires the sc_prefiltered data to run through the pipeline, NOT the SDA prefiltered data
#... this is because we need to input data in sf format with a projection accounting for the international date line
#... and it's currently not possible to shut of the prefiltering step in aniMotum if inputting data in sf format


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


# 2) LOAD DATA ############################################
## 2a) DATA PASSING SC PREFILTERS WITH TAG DEPLOYMENTS WITH SHORT DURATION/FEW DETECTIONS AFTER SDA FLAGGED FOR REMOVAL ####
event_sc_prefiltered_sf_sdaflags <- readRDS(paste0("./Data/rds/06_POINTS_sdaflags_sf_",data_version,".rds"))

## 2b) FILTER TO TAG DEPLOYMENTS TO RUN THROUGH SSM ####
event_for_ssm_sf <- event_sc_prefiltered_sf_sdaflags %>%
  filter(filter_track_duration_less8days != 0 & # remove deployments with a week or less tracking duration after sda prefiltering
           filter_visible_obs_less5 != 0 & # remove deployments with fewer than 5 locations after sda prefiltering
           filter_scprefilter_less3 != 0) # remove deployments we didn't even try to prefilter in aniMotum

rm(event_sc_prefiltered_sf_sdaflags)

## 2c) EXTRACT METADATA FOR LATER MERGING ####
metadata_to_add <- event_for_ssm_sf %>%
  st_drop_geometry() %>%
  select(species_code, 
         sc_dataset_id, sc_individual_id, sc_deployment_id, 
         individual_local_identifier, sc_tracking_data_type, 
         movebank_study_id,
         movebank_individual_id,
         movebank_deployment_id) %>%
  distinct()


# 3) FORMAT FOR ANIMOTUM ######################################
## 3a) FORMAT COLUMNS ####
dat_am_df <- event_for_ssm_sf %>% 
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

## 3b) SPLIT BY DATA TYPE RECORDED BY TAG (SC_TRACKING_DATA_TYPE) ####
dat_lst_sensors <- split(dat_am_df, dat_am_df$sc_tracking_data_type) %>%
  purrr::map(~.x %>% select(-sc_tracking_data_type))
rm(dat_am_df)

## 3c) CONVERT TO SF AND SPECIFY LL FORMAT ####
# This basic lat/long is the format of data coming out of Movebank
# If a projection is not specified, had issues for birds crossing the international dateline
dat_lst_sensors_sf <- dat_lst_sensors %>%
  purrr::map(~sf::st_as_sf(.x, coords = c("lon","lat"), 
                           crs="+proj=longlat +datum=WGS84")) # specify this is lat long format

## 3d) SET PROJECTION TO GLOBAL MERCATOR WITH 180 MERIDIAN ####
# This is the projection aniMotum uses...
# Need to specify *specifically* the 180 meridian otherwise international dateline problems will arise
# https://github.com/ianjonsen/aniMotum/blob/main/R/pf_sf_project.R (this is how aniMotum formats data that cross the dateline)
dat_lst_sensors_sf_merc180 <- dat_lst_sensors_sf %>%
  purrr::map(~.x %>%
               st_transform("+proj=merc +lon_0=180 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs"))
rm(dat_lst_sensors, dat_lst_sensors_sf, event_for_ssm_sf)


# 4) FIT SSM (RUN aniMotum): GPS ONLY DATA #################################
# Modeled data to account for spatial error is not needed for GPS data really
# However, we run these birds through for consistency of output across data types 

## 4a) GET UNIQUE TIMESTAMPS ####
# Get the unique input timestamps for the tag prior to the SDA prefilter
# These are the timesteps we will predict locations at 
# Note: with this approach if the SDA in aniMotum removes a location, the model will interpolate a new one...
# ... at that timestamp
gps_timesteps <- dat_lst_sensors_sf_merc180$gps %>%
  distinct(id, date)

## 4b) PREFILTER AND FIT SSM ####
dat_ssm_gps <-  aniMotum::fit_ssm(dat_lst_sensors_sf_merc180$gps, 
                                  vmax = 42,  #m/s (speed from Gronroos radar study)
                                  ang = c(15,25), #c(15,25) DEFAULT (applies SDA filter in argos package); NA runs speed filter only
                                  distlim = c(50000, 60000), #default is c(2500, 5000); NA shuts off SDA in favor of speed filter
                                  # expands distance limits to remove spikes beyond 50-60km (spikes patterns were realistic in many cases up to 50km)
                                  spdf = TRUE, # FALSE shuts off speed filter
                                  min.dt = 0, # 0 second minimum allowable time between locs
                                  model = "rw", # random walk
                                  time.step = gps_timesteps, # returns locations at observation times
                                  fit.to.subset =  TRUE, # TRUE runs aniMotum right away after prefiltering (this is the main difference to the previous prefiltering only step)
                                  control = ssm_control(verbose = 0)) 
rm(gps_timesteps)

# IF RECEIVE THIS WARNING: stats::filter(x.init1, rep(1, 5)/5)  'filter' is longer than time series...
# There is likely a tag with a really short run length and needs to be removed
# If receive warnings that SE's could not be calculated
# Could try changing interpolation interval (see example here: https://cran.r-project.org/web/packages/foieGras/vignettes/basics.html)

## 4b) EXPLORE SSM RESULTS ####
glimpse(dat_ssm_gps) %>% print(n = Inf) # look at all results
glimpse(dat_ssm_gps) %>% filter(converged == "FALSE") # check if any failed to converge
glimpse(dat_ssm_gps) %>% filter(pdHess == "FALSE") # check if TMB solved the Hessian Matrix & Obtained SEs

## 4c) SAVE SSM RESULTS ####
saveRDS(dat_ssm_gps, paste0("./Data/rds/Modeled/07_SSM_Origts_gps_",data_version,".rds"))

## 4d) EXTRACT SSM predicted VALUES ####
# GRAB MODELED DAT AS SPATIAL DATA FRAME:
plocs_sf_gps <- grab(dat_ssm_gps, what = "predicted", as_sf = TRUE)

## 4e) CREATE SF DATAFRAME OF AM ESTIMATED LOCS ####
plocs_sf_gps_format <- plocs_sf_gps %>%
  rename(sc_deployment_id = id,
         timestamp = date,
         x_se_km = x.se,
         y_se_km = y.se) %>%
  arrange(sc_deployment_id, timestamp) %>%
  group_by(sc_deployment_id) %>%
  mutate(am_modeled_seq_id = row_number()) %>% # create sequential ID of predicted location out of aniMotum for the tag deployment
  ungroup()

## 4f) JOIN WITH METDATA ####
plocs_sf_gps_format <- left_join(plocs_sf_gps_format, metadata_to_add) #Joining with `by = join_by(sc_deployment_id)`

## 4g) SAVE MODELED DATA ####
saveRDS(plocs_sf_gps_format, paste0("./Data/rds/Modeled/07_PREDICTED_Origts_SF_gps_",data_version,".rds"))
rm(plocs_sf_gps_format, dat_ssm_gps, plocs_sf_gps, se_issues_gps)
gc()


# 5) FIT SSM (RUN aniMotum): ARGOS/GPS DATA #################################
## 5a) GET UNIQUE TIMESTAMPS ####
# Get the unique input timestamps for the tag prior to the SDA prefilter
# These are the timesteps we will predict locations at 
# Note: with this approach if the SDA in aniMotum removes a location, the model will interpolate a new one...
# ... at that timestamp
argosgps_timesteps <- dat_lst_sensors_sf_merc180$argosgps %>%
  distinct(id, date)

## 5b) PREFILTER AND FIT SSM ####
dat_ssm_argosgps <-  aniMotum::fit_ssm(dat_lst_sensors_sf_merc180$argosgps, 
                                       vmax = 42,  #m/s (speed from Gronroos radar study)
                                       ang = c(5,45), # broadened outer angle to account for key problems identified by visual inspection 
                                       distlim = c(5000, 10000), # default is c(2500, 5000); broadened outer filter for spikes beyond 10 km
                                       spdf = TRUE, # FALSE shuts off speed filter
                                       min.dt = 0, # 0 second minimum allowable time between locs 
                                       model = "rw", # random walk
                                       time.step = argosgps_timesteps, # returns locations at observation times
                                       fit.to.subset =  TRUE, # TRUE runs aniMotum right away after prefiltering (this is the main difference to script 06)
                                       control = ssm_control(verbose = 0)) 

## 5c) EXPLORE SSM RESULTS ####
glimpse(dat_ssm_argosgps) %>% print(n = Inf) # look at all results
glimpse(dat_ssm_argosgps) %>% filter(converged == "FALSE") # check if any failed to converge:
glimpse(dat_ssm_argosgps) %>% filter(pdHess == "FALSE") # check if TMB solved the Hessian Matrix & Obtained SEs

## 5d) SAVE SSM RESULTS ####
saveRDS(dat_ssm_argosgps, paste0("./Data/rds/Modeled/07_SSM_Origts_argosgps_",data_version,".rds"))

## 5e) EXTRACT SSM predicted VALUES ####
# GRAB MODELED DAT AS SPATIAL DATA FRAME:
plocs_sf_argosgps <- grab(dat_ssm_argosgps, what = "predicted", as_sf = TRUE)

## 5f) CREATE SF DATAFRAME OF PREDICTED LOCS ####
plocs_sf_argosgps_format <- plocs_sf_argosgps %>%
  rename(sc_deployment_id = id,
         timestamp = date,
         longitude_se_km = x.se,
         latitude_se_km = y.se) %>%
  arrange(sc_deployment_id, timestamp) %>%
  group_by(sc_deployment_id) %>%
  mutate(am_modeled_seq_id = row_number()) %>%
  ungroup()

# JOIN WITH METDATA:
plocs_sf_argosgps_format <- left_join(plocs_sf_argosgps_format, metadata_to_add) #Joining with `by = join_by(sc.deployment.id)`

## 5g) SAVE MODELED DATA ####
saveRDS(plocs_sf_argosgps_format, paste0("./Data/rds/Modeled/07_PREDICTED_Origts_SF_argosgps_",data_version,".rds"))
rm(plocs_sf_argosgps_format, dat_ssm_argosgps, plocs_sf_argosgps, argosgps_timesteps, se_issues_argosgps)
gc()


# 6) FIT SSM (RUN aniMotum): ARGOS DATA #################################
## 6a) GET UNIQUE TIMESTAMPS ####
# Get the unique input timestamps for the tag prior to the SDA prefilter
# These are the timesteps we will predict locations at 
# Note: with this approach if the SDA in aniMotum removes a location, the model will interpolate a new one...
# ... at that timestamp
argos_timesteps <- dat_lst_sensors_sf_merc180$argos %>%
  distinct(id, date)

## 6b) PREFILTER AND FIT SSM ####
dat_ssm_argos <-  aniMotum::fit_ssm(dat_lst_sensors_sf_merc180$argos, 
                                    vmax = 42,  #m/s (speed from Gronroos radar study)
                                    ang = c(15,45),  # broadened outer angle angle further to account for key problems identified by visual inspection
                                    distlim = c(2500, 10000), #default is c(2500, 5000); broadened outer filter for spikes beyond 10 km
                                    spdf = TRUE, # FALSE shuts off speed filter
                                    min.dt = 0, # 0 second minimum allowable time between locs 
                                    model = "rw", # random walk
                                    time.step = argos_timesteps, # returns locaitons at observation times
                                    fit.to.subset =  TRUE, # TRUE runs aniMotum right away after prefiltering (this is the main difference from script 06)
                                    control = ssm_control(verbose = 0)) 

## 6c) EXPLORE SSM RESULTS ####
glimpse(dat_ssm_argos) %>% print(n = Inf) # look at all results
glimpse(dat_ssm_argos) %>% filter(converged == "FALSE") # check if any failed to converge
glimpse(dat_ssm_argos) %>% filter(pdHess == "FALSE") # check if TMB solved the Hessian Matrix & Obtained SEs

## 6d) SAVE SSM RESULTS ####
saveRDS(dat_ssm_argos, paste0("./Data/rds/Modeled/07_SSM_Origts_argos_",data_version,".rds"))

## 6e) EXTRACT SSM predicted VALUES ####
# GRAB MODELED DAT AS SPATIAL DATA FRAME:
plocs_sf_argos <- grab(dat_ssm_argos, what = "predicted", as_sf = TRUE)

## 6f) CREATE SF DATAFRAME OF PREDICTED LOCS ####
plocs_sf_argos_format <- plocs_sf_argos %>%
  rename(sc_deployment_id = id,
         timestamp = date,
         longitude_se_km = x.se,
         latitude_se_km = y.se) %>%
  arrange(sc_deployment_id, timestamp) %>%
  group_by(sc_deployment_id) %>%
  mutate(am_modeled_seq_id = row_number()) %>%
  ungroup()

# JOIN WITH METDATA:
plocs_sf_argos_format <- left_join(plocs_sf_argos_format, metadata_to_add) #Joining with `by = join_by(sc.deployment.id)`

## 6h) SAVE MODELED DATA ##################################################
saveRDS(plocs_sf_argos_format, paste0("./Data/rds/Modeled/07_PREDICTED_Origts_SF_argos_",data_version,".rds"))
rm(plocs_sf_argos_format, dat_ssm_argos, plocs_sf_argos, argos_timesteps, se_issues_argos)
gc()

