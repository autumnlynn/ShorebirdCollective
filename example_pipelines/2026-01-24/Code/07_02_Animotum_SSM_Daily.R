# CODE: 07_02_aniMotum_SSM_Daily.R
# This code is to be used after 06_02_AniMotum_Segment_and_SDA_Prefilter_Daily.r
#
# CODE PURPOSE: RUN SC SEGMENTED AND PRE-FILTERED TRACKING DATA THROUGH ANIMOTUM TO ESTIMATE 1 LOCATION PER DAY
# ESTIMATES DAILY LOCATIONS but after segmenting after gaps of 15 days and retaining only segments with 5 pts after passing prefilter
#

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
## 2a) SEGMENTED DATA FORMATTED FOR ANIMOTUM AFTER DAILY PREFILTER ####
dat_lst_sensors_sf_merc180_checked <- paste0("./Data/rds/06_animotum_daily_input_segdat_",data_version,".rds")

## 2b) METADATA TO ADD ####
metadata_to_add <- readRDS(paste0("./Data/rds/06_animotum_daily_metadata_",data_version,".rds"))


# 3) FIT SSM (RUN aniMotum): GPS ONLY DATA #################################
## 3a) PREFILTER AND FIT SSM ####
dat_ssm_gps <-  aniMotum::fit_ssm(dat_lst_sensors_sf_merc180_checked$gps, 
                                  vmax = 42,  #m/s (speed from Gronroos radar study)
                                  ang = c(15,25), #c(15,25) DEFAULT (applies SDA filter in argos package); NA runs speed filter only
                                  distlim = c(50000, 60000), #default is c(2500, 5000); NA shuts off SDA in favor of speed filter
                                  # expands distance limits to remove spikes beyond 50-60km (spikes patterns were realistic in some cases up to 50km)
                                  spdf = TRUE, # FALSE shuts off speed filter
                                  min.dt = 0, # 0 second minimum allowable time between locs (allows for same timestamp to run through)
                                  model = "rw", # random walk
                                  time.step = 24, # 1 location per day
                                  fit.to.subset =  TRUE, # TRUE runs aniMotum right away after prefiltering 
                                  control = ssm_control(verbose = 0)) 
gc()

# IF RECEIVE THIS WARNING: stats::filter(x.init1, rep(1, 5)/5)  'filter' is longer than time series...
# There is likely a tag with a really short run length and needs to be removed
# If receive warnings that SE's could not be calculated
# Could try changing interpolation interval (see example here: https://cran.r-project.org/web/packages/foieGras/vignettes/basics.html)

## 3b) EXPLORE SSM RESULTS ####
glimpse(dat_ssm_gps) %>% print(n = Inf) # look at all results
glimpse(dat_ssm_gps) %>% filter(converged == "FALSE") # check if any failed to converge 
glimpse(dat_ssm_gps) %>% filter(pdHess == "FALSE") # check if TMB solved the Hessian Matrix & Obtained SEs

## 3c) SAVE SSM RESULTS ####
saveRDS(dat_ssm_gps, paste0("./Data/rds/Modeled/07_SSM_Daily_gps_",data_version,".rds"))

## 3d) EXTRACT SSM predicted VALUES ####
# GRAB MODELED DAT AS SPATIAL DATA FRAME:
plocs_sf_gps <- grab(dat_ssm_gps, what = "predicted", as_sf = TRUE)

## 3e) CREATE SF DATAFRAME OF AM ESTIMATED LOCS ####
plocs_sf_gps_format <- plocs_sf_gps %>%
  rename(segment_id = id,
         timestamp = date,
         x_se_km = x.se,
         y_se_km = y.se) %>%
  arrange(segment_id, timestamp) %>%
  group_by(segment_id) %>%
  mutate(am_modeled_seq_id = row_number()) %>%
  ungroup()

## 3f) JOIN WITH METDATA ####
plocs_sf_gps_format <- left_join(plocs_sf_gps_format, metadata_to_add) #Joining with `by = join_by(segment_id)`

## 3g) SAVE MODELED DATA ####
saveRDS(plocs_sf_gps_format, paste0("./Data/rds/Modeled/07_PREDICTED_Daily_SF_gps_",data_version,".rds"))
rm(plocs_sf_gps_format, dat_ssm_gps, plocs_sf_gps)
gc()


# 4) FIT SSM (RUN aniMotum): ARGOS/GPS DATA #################################
## 4a) PREFILTER AND FIT SSM ####
dat_ssm_argosgps <-  aniMotum::fit_ssm(dat_lst_sensors_sf_merc180_checked$argosgps, 
                                       vmax = 42,  #m/s (speed from Gronroos radar study)
                                       ang = c(5,45), # broadened outer angle to account for key problems identified by visual inspection 
                                       distlim = c(5000, 10000), # default is c(2500, 5000); broadened outer filter for spikes beyond 10 km
                                       spdf = TRUE, # FALSE shuts off speed filter
                                       min.dt = 0, # 0 second minimum allowable time between locs (allows for same timestamp to run through)
                                       model = "rw", # random walk
                                       time.step = 24, # 1 location per day
                                       fit.to.subset =  TRUE, # TRUE runs aniMotum right away after prefiltering 
                                       control = ssm_control(verbose = 0))
gc()


## 4b) EXPLORE SSM RESULTS ####
glimpse(dat_ssm_argosgps) %>% print(n = Inf) # look at all results
glimpse(dat_ssm_argosgps) %>% filter(converged == "FALSE") # check if any failed to converge 
glimpse(dat_ssm_argosgps) %>% filter(pdHess == "FALSE") # check if TMB solved the Hessian Matrix & Obtained SEs

## 4c) SAVE SSM RESULTS ####
saveRDS(dat_ssm_argosgps, paste0("./Data/rds/Modeled/07_SSM_Daily_argosgps_",data_version,".rds"))

## 4d) EXTRACT SSM predicted VALUES ####
# GRAB MODELED DAT AS SPATIAL DATA FRAME:
plocs_sf_argosgps <- grab(dat_ssm_argosgps, what = "predicted", as_sf = TRUE)

## 4e) CREATE SF DATAFRAME OF AM ESTIMATED LOCS ####
plocs_sf_argosgps_format <- plocs_sf_argosgps %>%
  rename(segment_id = id,
         timestamp = date,
         x_se_km = x.se,
         y_se_km = y.se) %>%
  arrange(segment_id, timestamp) %>%
  group_by(segment_id) %>%
  mutate(am_modeled_seq_id = row_number()) %>%
  ungroup()

# JOIN WITH METDATA:
plocs_sf_argosgps_format <- left_join(plocs_sf_argosgps_format, metadata_to_add) #Joining with `by = join_by(segment_id)`

## 4f) SAVE MODELED DATA ####
saveRDS(plocs_sf_argosgps_format, paste0("./Data/rds/Modeled/07_PREDICTED_Daily_SF_argosgps_",data_version,".rds"))
rm(plocs_sf_argosgps_format, dat_ssm_argosgps, plocs_sf_argosgps)
gc()


# 5) FIT SSM (RUN aniMotum): ARGOS DATA #################################
## 5a) PREFILTER AND FIT SSM ####
dat_ssm_argos <-  aniMotum::fit_ssm(dat_lst_sensors_sf_merc180_checked$argos, 
                                    vmax = 42,  #m/s (speed from Gronroos radar study)
                                    ang = c(15,45),  # broadened outer angle angle further to account for key problems identified by visual inspection
                                    distlim = c(2500, 10000), #default is c(2500, 5000); broadened outer filter for spikes beyond 10 km
                                    spdf = TRUE, # FALSE shuts off speed filter
                                    min.dt = 0, # 0 second minimum allowable time between locs (allows for same timestamp to run through)
                                    model = "rw", # random walk
                                    time.step = 24, # returns locaitons at observation times
                                    fit.to.subset =  TRUE, # TRUE runs aniMotum right away after prefiltering 
                                    control = ssm_control(verbose = 0)) 
gc()

## 5b) EXPLORE SSM RESULTS ####
glimpse(dat_ssm_argos) %>% print(n = Inf) # look at all results
glimpse(dat_ssm_argos) %>% filter(converged == "FALSE") # check if any failed to converge
glimpse(dat_ssm_argos) %>% filter(pdHess == "FALSE") # check if TMB solved the Hessian Matrix & Obtained SEs

## 5c) SAVE SSM RESULTS ####
saveRDS(dat_ssm_argos, paste0("./Data/rds/Modeled/07_SSM_Daily_argos_",data_version,".rds"))

## 5d) EXTRACT SSM predicted VALUES ####
# GRAB MODELED DAT AS SPATIAL DATA FRAME:
plocs_sf_argos <- grab(dat_ssm_argos, what = "predicted", as_sf = TRUE)

## 5e) CREATE SF DATAFRAME OF AM ESTIMATED LOCS ####
plocs_sf_argos_format <- plocs_sf_argos %>%
  rename(segment_id = id,
         timestamp = date,
         x_se_km = x.se,
         y_se_km = y.se) %>%
  arrange(segment_id, timestamp) %>%
  group_by(segment_id) %>%
  mutate(am_modeled_seq_id = row_number()) %>%
  ungroup()

# JOIN WITH METDATA:
plocs_sf_argos_format <- left_join(plocs_sf_argos_format, metadata_to_add) #Joining with `by = join_by(segment_id)`

## 5f) SAVE MODELED DATA ####
saveRDS(plocs_sf_argos_format, paste0("./Data/rds/Modeled/07_PREDICTED_Daily_SF_argos_",data_version,".rds"))
rm(plocs_sf_argos_format, dat_ssm_argos, plocs_sf_argos)
gc()

