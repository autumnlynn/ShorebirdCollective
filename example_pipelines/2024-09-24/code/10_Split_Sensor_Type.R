# CODE: 10_Split_Sensor_Type.R
# CODE PURPOSE: SPLIT DATA INTO ARGOS& ARGOS/GPS & GPS 
# This code simply split into two different sensor type groups
# So that can more cleanly truncate argos data for large gaps (not relevant for many gps tags)

# Movebank data library for more info: http://vocab.nerc.ac.uk/collection/MVB/current/


# 0) SET WORKING DIRECTORY ##############################
setwd()
Sys.setenv(TZ = 'UTC')


# 1) LOAD PACKAGES #################################################
library(tidyverse)


# 2) LOAD EVENT DATA ###############################################
event_dat_lst <- readRDS("./Data/Cleaned/09_event_dat_FLAGGED_DC_DEPS.rds")


# 3) SUBSET BY SENSOR TYPE #####################################
event_dat_df <- event_dat_lst %>% 
  map_dfr(~.x %>% as.data.frame())

## 3a) CONTAINS ARGOS DATA ####
argos_dat_df <- event_dat_lst %>% 
  map_dfr(~.x %>% filter(sensor.types.detected %in% c("Argos Doppler Shift", "GPS,Argos Doppler Shift")))

## 3b) GPS DATA ONLY ####
gps_dat_df <- event_dat_lst %>% 
  map_dfr(~.x %>% filter(sensor.types.detected %in% c("GPS", "GPS,Cellular Location")))

# CHECK: SHOULD BE THE SAME:
event_dat_df %>% nrow()
argos_dat_df %>% nrow() + gps_dat_df %>% nrow()


# 4) SAVE FOR NEXT STEPS #########################################
## 4a) CONTAINS ARGOS ####
saveRDS(argos_dat_df, "./Data/Cleaned/10_event_dat_FLAGGED_DC_DEPS_ARGOS.rds")

## 4b) GPS ONLY ####
saveRDS(gps_dat_df, "./Data/Cleaned/10_event_dat_FLAGGED_DC_DEPS_GPS.rds")
