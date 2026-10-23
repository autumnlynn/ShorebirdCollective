# CODE: 02_Movebank_Reassign_SensorType.R
# CODE PURPOSE: REASSIGN REFERENCE DATA SENSOR TYPES
# Movebank data library for more info: http://vocab.nerc.ac.uk/collection/MVB/current/

# THIS CODE DOES THE FOLLOWING: 
# Filters reference data to sensor types with lat and long location data
# This is important because sometimes data managers specified sensor data as a different type than what is available
# For example, a few times, GPS data are listed as a data type, but no GPS data exist for that bird or project
# **Data types are important because GPS, Argos, and GPS/Argos data have different analysis constraints**
# Ultimately, this adds a column called sensor.types.detected to event and reference data


# 0) SET WORKING DIRECTORY ##############################
setwd()
Sys.setenv(TZ = 'UTC')


# 1) LOAD PACKAGES #################################################
library(tidyverse)
library(conflicted)
library(janitor)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")


# 2) LOAD IN MOVEBANK EVENT & REFERENCE DATA ###########################################
# Data were downloaded in Script 01
## 2a) DETECTION EVENTS ####
movebank_event_dat_df <- readRDS("./Data/MovebankDownload/01_argosgps_download_dat.rds")
movebank_event_dat_df %>% select(deployment.id) %>% distinct() %>% count()

## 2b) REFERENCE DATA ####
movebank_ref_dat_df <- readRDS("./Data/MovebankDownload/01_argosgps_download_refdat.rds")
# Note that there is more than one row per deployment if more than one data type reported by data manager OR...
# more than one sensor detected


# 3) DETERMINE EVENT SENSOR TYPES ##################################################
# Use event detections to assess which primary sensor types (argos or gps) are associated with each deployment

## 3a) GET DEPLOY IDs WHERE BOTH GPS & ARGOS DAT EXIST: ####
movebank_event_dat_df %>% select(sensor.type) %>% distinct()

# BOTH ARGOS & GPS DATA:
argosgps_depIDs <- movebank_event_dat_df %>% 
  select(deployment.id, sensor.type) %>% 
  distinct() %>%
  group_by(deployment.id) %>% count() %>% 
  filter(n > 1) %>% 
  select(deployment.id) 

## 3b) GET DEPLOY IDs WHERE *ONLY* ARGOS DAT EXIST: ####
# ARGOS only: 
# Grab all Argos dep IDs:
allargos_depIDs <- movebank_event_dat_df %>% 
  filter(sensor.type == "Argos Doppler Shift") %>%
  select(deployment.id) %>% 
  distinct()  
# Remove dep ids for deployments with both argos & gps detects:
argosOnly_depIDs <- anti_join(allargos_depIDs, argosgps_depIDs) %>% #Joining, by = "deployment.id"
  pull() 

## 3c) GET DEPLOY IDs WHERE *ONLY* GPS dat EXIST: ####
# GPS only:
# Grab all gps dep IDs:
allgps_depIDs <- movebank_event_dat_df %>% 
  filter(sensor.type == "GPS") %>%
  select(deployment.id) %>%
  distinct() 
# Remove dep ids for deployments with both argos & gps detects:
gpsOnly_depIDs <- anti_join(allgps_depIDs, argosgps_depIDs) %>%  #Joining, by = "deployment.id"
  pull() 

# Convert argos/gps ids to vector:
argosgps_depIDs <- argosgps_depIDs %>% pull(deployment.id)


# 4) SUBSET & REASSIGN REFERENCE DATA : #######################################################
# Filter to unique deployments and adds sensor types we calculated from event data

## 4a) SUBSET TO UNIQUE ROW PER DEPLOYMENT ####
# CREATE SINGLE ROW PER TAG DEPLOYMENT:
# Remove columns unique to different sensor types
movebank_ref_dat_df_sub <- movebank_ref_dat_df %>%
  select(-sensor.type, -sensor.type.id, -animal.sensor.type.ids, -tag.sensor.type.ids, -sensor.type.ids,
         -number.of.location.events, -number.of.events) %>%
  distinct() %>% 
  filter(!is.na(deployment.id)) # remove those without deployments

# CHECK FOR DUPLICATE DEPLOYMENTS:
movebank_ref_dat_df_sub %>% 
  group_by(deployment.id) %>%
  count() %>%
  filter(n > 1) 

## 4b) ARGOS AND GPS: ADD SENSOR TYPE DETECTED #### 
# Could use case_when statements to make 4b-e a single step below
# Subset to individuals with both argos and gps detections and add identifier:
ref_ag <- movebank_ref_dat_df_sub %>%
  filter(deployment.id %in% argosgps_depIDs) %>%
  mutate(sensor.types.detected = "GPS,Argos Doppler Shift")

## 4c) ARGOS ONLY: ADD SENSOR TYPE DETECTED ####
# Subset to individuals with only argos detections and add identifier:
ref_a <- movebank_ref_dat_df_sub %>%
  filter(deployment.id %in% argosOnly_depIDs) %>%
  mutate(sensor.types.detected = "Argos Doppler Shift")

## 4d) GPS ONLY: ADD SENSOR TYPE DETECTED ####
# Subset to individuals with only gps detections and add identifier:
ref_g <- movebank_ref_dat_df_sub %>% 
  filter(deployment.id %in% gpsOnly_depIDs) %>%
  mutate(sensor.types.detected = "GPS")

## 4e) COMBINE ASSIGNED REF DATA BACK INTO ONE DF ####
ref_dat_reassigned <- rbind(ref_ag, ref_a, ref_g)

## CHECK REF DAT DO WE NOT HAVE EVENT DATA FOR:
test <- anti_join(movebank_ref_dat_df, ref_dat_reassigned) %>%
  select(study.id, deployment.id, sensor.type, number.of.location.events,
         number.of.events, animal.local.identifier, 
         tag.local.identifier, deploy.on.timestamp)
# These should be tags without deployment times, deployments with no data, or possibly sensor data only
rm(test)

## 4f) SAVE AS "REASSIGNED" REF DATA ####
saveRDS(ref_dat_reassigned, "./Data/Cleaned/02_movebank_ref_dat_reassigned.rds")


# 5) ADD SENSOR.TYPES.DETECTED COLUMN INTO EVENT DATA #########################
# Could use case_when statements to make 5a-e a single step below

## 5a) GRAB & ASSIGN DATA: ####
event_dat_reassigned <- movebank_event_dat_df %>% 
  mutate(sensor.types.detected = case_when(deployment.id %in% argosgps_depIDs ~ "GPS,Argos Doppler Shift",
                                           deployment.id %in% argosOnly_depIDs ~ "Argos Doppler Shift",
                                           deployment.id %in% gpsOnly_depIDs ~ "GPS", .default = NA))

## 5b) SAVE AS "REASSIGNED" EVENT DATA ####
saveRDS(event_dat_reassigned, "./Data/Cleaned/02_movebank_event_dat_reassigned.rds")

