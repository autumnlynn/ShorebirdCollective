# CODE: 03_Movebank_Create_scIds.R
# CODE PURPOSE: ADD sc.dataset.id, and CREATE sc.individual.id and sc.deployment.id 
# And add these IDs to event and ref dat as unique identifiers
# Movebank data library for more info: http://vocab.nerc.ac.uk/collection/MVB/current/
# If only using Movebank data, this step wouldn't be necessary because there are unique IDs in movebank, but...
# We need unique IDs for both movebank and data sent internally

# 0) SET WORKING DIRECTORY ##############################
setwd()
Sys.setenv(TZ = 'UTC')


# 1) LOAD PACKAGES #################################################
library(tidyverse)
library(conflicted)
library(lubridate)
library(viridis)
library(janitor)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")


# 2) LOAD IN DATA ##################################################
## 2a) Event data ####
event_dat_reassigned <- readRDS("./Data/Cleaned/02_movebank_event_dat_reassigned.rds")

## 2b) Reference data ####
# (filtered to sensor types present in event data & reassigned to groups under "sensor.type.detected")
ref_dat_reassigned <- readRDS("./Data/Cleaned/02_movebank_ref_dat_reassigned.rds")

## 2c) SC DSA metadata ####
# This csv has internal dataset ids linked to data terms of use
# They are linked by a column with movebank study.id as well as species name
all_argosgps_scmeta <- read.csv("./Data/MetadataExtract/argosgps_scMETA.csv")


# 3) ADD DATASET IDs ##################################
## 3a) MERGE WITH EVENT DATA ####
event_dat_reassigned_meta <- left_join(event_dat_reassigned, all_argosgps_scmeta) 
rm(ref_dat_reassigned_merge)

## 3b) MERGE WITH REF DATA ####
ref_dat_reassigned_meta <- left_join(ref_dat_reassigned, all_argosgps_scmeta) 


# 4) CREATE SC.INDIVIDUAL.IDS ############################
# Create these by combining dataset ids with individual IDs for birds from data owners
## 4a) EVENT DATA ####
movebank_event_dat_scID <- event_dat_reassigned_meta %>% 
  mutate(IND = "IND") %>%
  unite("sc.individual.id", c("sc.dataset.id", "IND", "individual.local.identifier"), sep = "_", remove = FALSE) %>%
  select(-IND) %>% 
  relocate(sc.individual.id, .after = sc.dataset.id)

# 4b) REF DATA ####
ref_dat_dat_scID <- ref_dat_reassigned_meta %>% 
  mutate(IND = "IND") %>%
  unite("sc.individual.id", c("sc.dataset.id", "IND", "animal.local.identifier"), sep = "_", remove = FALSE) %>%
  select(-IND) %>% 
  relocate(sc.individual.id, .after = sc.dataset.id)


# 5) CREATE SC.DEPLOYMENT.IDS #############################
## 5a) EVENT DATA ####
movebank_event_dat_scID <- movebank_event_dat_scID %>% 
  mutate(DEP = "DEP") %>%
  unite("sc.deployment.id", c("sc.individual.id", "DEP", "deployment.id"), sep = "_", remove = FALSE) %>%
  select(-DEP) %>% 
  relocate(sc.deployment.id, .after = sc.individual.id)

## 5b) REF DATA ####
ref_dat_dat_scID <- ref_dat_dat_scID %>% 
  mutate(DEP = "DEP") %>%
  unite("sc.deployment.id", c("sc.individual.id", "DEP", "deployment.id"), sep = "_", remove = FALSE) %>%
  select(-DEP) %>% 
  relocate(sc.deployment.id, .after = sc.individual.id)


# 6) SAVE DATA #######################################
# 6a) EVENT DATA:
saveRDS(movebank_event_dat_scID, "./Data/Cleaned/03_movebank_event_dat_argosgps_scID.rds")
# 6b) REF DATA:
saveRDS(ref_dat_dat_scID, "./Data/Cleaned/03_movebank_ref_dat_scID.rds")

