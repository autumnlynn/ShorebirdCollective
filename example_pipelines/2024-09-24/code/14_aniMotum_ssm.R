# CODE: 14_aniMotum_ssm.R
# CODE PURPOSE: RUN SC PRE-FILTERED TRACKING DATA THROUGH ANIMOTUM TO ESTIMATE MOST PROBABLE LOCATION AND SPATIAL ERROR
# This script only runs tag deployments with sufficient durations and detection sample sizes 
# after considering the aniMotum speed-distance-angle prefilter step
# Note: birds tracked with GPS only, Argos only, and both Argos and GPS are run through aniMotum separately 
# because they require different prefiltering approaches
# Example can be found here: https://cran.r-project.org/web/packages/foieGras/vignettes/basics.html


# 0) SET WORKING DIRECTORY ###############################
setwd()
Sys.setenv(TZ = 'UTC')


# 1) LOAD PACKAGES ###########################################
library(tidyverse)
library(aniMotum)
library(conflicted)
conflict_prefer("filter", "dplyr")
library(cowplot)
library(sf)
source("./Code/Functions/named_group_split.r")


# 2) LOAD DATA ############################################
## 2a) DATA PASSING SC PREFILTERS WITH SHORT TRACK DURATION/FEW DETECTIONS AFTER SDA REMOVED ####
dat_scpf_keeps_lst_updated <- readRDS("./Data/Cleaned/13_event_dat_SDAFILTER_INDS.rds")
# In previous step, might be possible to export as sf, so wouldn't need to reformat below

### EXTRACT METADATA FOR MERGING WITH SSM OUTPUT ####
meta_df <- dat_scpf_keeps_lst_updated %>%
  map_df(~.x %>% as.data.frame()) %>%
  filter(sc.location.origin == "tag") %>%
  select(sc.deployment.id, sensor.types.detected, species.code, common.name) %>%
  distinct()

## 2b) FORMAT FOR ANIMOTUM ####
dat_lst_am <- dat_scpf_keeps_lst_updated %>% 
  purrr::map(~.x %>%
               select(sc.deployment.id, timestamp, argos.lc, location.long, location.lat, 
                      argos.semi.major, argos.semi.minor, argos.orientation, sensor.types.detected) %>% 
               rename(id = sc.deployment.id, 
                      date = timestamp, 
                      lc = argos.lc, 
                      lon = location.long, 
                      lat = location.lat, 
                      smaj = argos.semi.major, 
                      smin = argos.semi.minor, 
                      eor = argos.orientation) %>% # rename to match argos format
               mutate(lc = as.factor(lc), id = as.character(id)) %>% #Id has to be a character to run through aniMotum
               droplevels() %>% 
               as.data.frame())

## 2c) SPLIT BY DATA TYPE ####
dat_lst_sensors <- dat_lst_am %>%
  purrr::map_dfr(~.x %>% as.data.frame()) %>%
  named_group_split(., sensor.types.detected) %>%
  purrr::map(~.x %>% select(-sensor.types.detected))
rm(dat_lst_am, dat_scpf_keeps_lst_updated)

## 2d) CONVERT TO SF AND SPECIFY LAT/LONG ####
# Otherwise AM will guess the CRS for each project
dat_lst_sensors_ll_sf <- dat_lst_sensors %>%
  purrr::map(~sf::st_as_sf(.x, coords=c("lon","lat"), crs="+proj=longlat +datum=WGS84"))

## 2d) SPECIFY AM MERCATOR PROJECTION ####
# And convert to global mercator with the meridian at 180 again (to match animotum example)
# Meridian 180 helpful to ensure dateline is correct
dat_lst_sensors_sf_merc180 <- dat_lst_sensors_ll_sf %>%
  purrr::map(~.x %>%
        st_transform("+proj=merc +lon_0=180 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs"))
# Meridian 180 needed to ensure dateline is correct (may not be required in this version of aniMotum but keeping for consistency)
rm(dat_lst_am, dat_lst_sensors_ll_sf)


# 3)  FIT SSM (RUN aniMotum): GPS ONLY DATA #################################
# SSM to account for spatial error is not needed for GPS data if outputting at the original timestep of the tag
# Running for consistency of output across data types and can adjust if needed for timestep
## 3a) SDA PREFILTER AND FIT SSM ####
dat_ssm_gps <- aniMotum::fit_ssm(dat_lst_sensors_sf_merc180$GPS, 
                                    vmax = 42,  #m/s (SDA max speed from Gronroos radar study)
                                    ang = c(15,25), # default angles to consider
                                    distlim = c(50000, 60000), #default is c(2500, 5000); expands distance limits to remove spikes beyond 50-60km (spike patterns were realistic in many cases up to 50km)
                                    spdf = TRUE, # FALSE shuts off speed filter
                                    min.dt = 0, # 0 second minimum allowable time between locs (allows for same timestamp to run through)
                                    model = "rw", # random walk
                                    time.step = NA, # returns locaitons at observation times
                                    fit.to.subset =  TRUE, # TRUE runs aniMotum SSM right away after prefiltering 
                                    control = ssm_control(verbose = 0))

glimpse(dat_ssm_gps) %>% print(n = Inf) # look at all results
glimpse(dat_ssm_gps) %>% filter(converged == "FALSE") # check if any failed to converge
glimpse(dat_ssm_gps) %>% filter(pdHess == "FALSE") # check if TMB solved the Hessian Matrix & Obtained SEs

## 3c) SAVE SSM RESULTS ####
saveRDS(dat_ssm_gps, "./Data/Modeled/15_SSM_gps_.rds")

## 3d) EXTRACT SSM FITTED VALUES ####
# GRAB MODELED DATA AS SPATIAL DATA FRAME:
flocs_sf_gps <- grab(dat_ssm_gps, what = "fitted", as_sf = TRUE)

## 3e) CREATE SF DATAFRAME OF AM ESTIMATED LOCS ####
flocs_sf_gps_format <- flocs_sf_gps %>%
  rename(sc.deployment.id = id,
         timestamp = date,
         longitude.se.km = x.se,
         latitude.se.km = y.se) %>%
  arrange(sc.deployment.id, timestamp) %>%
  group_by(sc.deployment.id) %>%
  mutate(fg.modeled.seq.id = row_number()) %>%
  ungroup()

## 3f) JOIN WITH METDATA ####
flocs_sf_gps_format <- left_join(flocs_sf_gps_format, meta_df) #Joining with `by = join_by(sc.deployment.id)`

## 3g) SAVE MODELED DATA ####
saveRDS(flocs_sf_gps_format, "./Data/Modeled/14_FITTED_SF_gps.rds")


# 4) FIT SSM (RUN aniMotum): ARGOS/GPS DATA #################################
## 4a) SDA PREFILTER AND FIT SSM ####
dat_ssm_argosgps <-  aniMotum::fit_ssm(dat_lst_sensors_sf_merc180$`GPS,Argos Doppler Shift`, 
                                  vmax = 42,  #m/s (SDA max speed from Gronroos radar study)
                                  ang = c(5,45), # broadened outer angle to account for key problems identified by visual inspection 
                                  distlim = c(5000, 10000), # default is c(2500, 5000); broadened outer filter for spikes beyond 10 km
                                  spdf = TRUE, # FALSE shuts off speed filter
                                  min.dt = 0, # 0 second minimum allowable time between locs (allows for same timestamp to run through)
                                  model = "rw", # random walk
                                  time.step = NA, # returns locaitons at observation times
                                  fit.to.subset =  TRUE, # TRUE runs aniMotum right away after prefiltering 
                                  control = ssm_control(verbose = 0),
                                  map = list(psi = factor(NA)))

glimpse(dat_ssm_argosgps) %>% print(n = Inf) # look at all results
glimpse(dat_ssm_argosgps) %>% filter(converged == "FALSE") # check if any failed to converge
glimpse(dat_ssm_argosgps) %>% filter(pdHess == "FALSE") # check if TMB solved the Hessian Matrix & Obtained SEs

## 4b) SAVE SSM RESULTS ####
saveRDS(dat_ssm_argosgps, "./Data/Modeled/14_SSM_argosgps.rds")

## 4c) EXTRACT SSM FITTED VALUES ####
# GRAB MODELED DAT AS SPATIAL DATA FRAME:
flocs_sf_argosgps <- grab(dat_ssm_argosgps, what = "fitted", as_sf = TRUE)

## 4d) CREATE SF DATAFRAME OF AM ESTIMATED LOCS ####
flocs_sf_argosgps_format <- flocs_sf_argosgps %>%
  rename(sc.deployment.id = id,
         timestamp = date,
         longitude.se.km = x.se,
         latitude.se.km = y.se) %>%
  arrange(sc.deployment.id, timestamp) %>%
  group_by(sc.deployment.id) %>%
  mutate(fg.modeled.seq.id = row_number()) %>%
  ungroup()

## 4e) JOIN WITH METADATA ####
flocs_sf_argosgps_format <- left_join(flocs_sf_argosgps_format, meta_df) #Joining with `by = join_by(sc.deployment.id)`

## 4f) SAVE MODELED DATA ####
saveRDS(flocs_sf_argosgps_format, "./Data/Modeled/14_FITTED_SF_argosgps.rds")


# 5) FIT SSM (RUN aniMotum): ARGOS DATA #################################
## 5a) PREFILTER AND FIT SSM ####
dat_ssm_argos <-  aniMotum::fit_ssm(dat_lst_sensors_sf_merc180$`Argos Doppler Shift`, 
                                       vmax = 42,  #m/s (SDA max speed from Gronroos radar study)
                                       ang = c(15,45),  # broadened outer angle angle further to account for key problems identified by visual inspection
                                       distlim = c(2500, 10000), #default is c(2500, 5000); broadened outer filter for spikes beyond 10 km
                                       spdf = TRUE, # FALSE shuts off speed filter
                                       min.dt = 0, # 0 second minimum allowable time between locs (allows for same timestamp to run through)
                                       model = "rw", # random walk
                                       time.step = NA, # returns locaitons at observation times
                                       fit.to.subset =  TRUE, # TRUE runs aniMotum right away after prefiltering 
                                       control = ssm_control(verbose = 0),
                                       map = list(psi = factor(NA)))
glimpse(dat_ssm_argos) %>% print(n = Inf) # look at all results
glimpse(dat_ssm_argos) %>% filter(converged == "FALSE") # check if any failed to converge
glimpse(dat_ssm_argos) %>% filter(pdHess == "FALSE") # check if TMB solved the Hessian Matrix & Obtained SEs

## 5b) SAVE SSM RESULTS ####
saveRDS(dat_ssm_argos, "./Data/Modeled/14_SSM_argos_.rds")

## 5c) EXTRACT SSM FITTED VALUES ####
# GRAB MODELED DAT AS SPATIAL DATA FRAME:
flocs_sf_argos <- grab(dat_ssm_argos, what = "fitted", as_sf = TRUE)

## 5d) CREATE SF DATAFRAME OF AM ESTIMATED LOCS ####
flocs_sf_argos_format <- flocs_sf_argos %>%
  rename(sc.deployment.id = id,
         timestamp = date,
         longitude.se.km = x.se,
         latitude.se.km = y.se) %>%
  arrange(sc.deployment.id, timestamp) %>%
  group_by(sc.deployment.id) %>%
  mutate(fg.modeled.seq.id = row_number()) %>%
  ungroup()

## 5e) JOIN WITH METADATA ####
flocs_sf_argos_format <- left_join(flocs_sf_argos_format, meta_df) #Joining with `by = join_by(sc.deployment.id)`

## 5f) SAVE MODELED DATA ####
saveRDS(flocs_sf_argos_format, "./Data/Modeled/14_FITTED_SF_argos.rds")
