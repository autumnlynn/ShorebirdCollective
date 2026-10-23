# CODE: 04_Merge_DirectSend_Data.R
# CODE PURPOSE: READ IN PRE-PREPPED DATA SENT DIRECTLY TO US...
# ... & COMBINE WITH MOVEBANK DATA (EVENTS & REFERENCE DAT) 
# Movebank data library for more info: http://vocab.nerc.ac.uk/collection/MVB/current/


# 0) SET WORKING DIRECTORY ##########################################
setwd()


# 1) LOAD PACKAGES #################################################
library(tidyverse)
library(conflicted)
library(janitor)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")


# 2) LOAD IN PREPPED EVENT DATA ##########################################
# These are .rds files for event data for each dataset already...
# Formatted to match the movebank data dictionary format... and 
# With sc.dataset.id, sc.individual.id, sc.deployment.id already assigned
# And sensor.types.detected columns
direct_send1 <- readRDS("./Data/DirectSend/rds/direct_send1_event.rds")
direct_send2 <- readRDS("./Data/DirectSend/rds/direct_send2_event.rds")
direct_send3 <- readRDS("./Data/DirectSend/rds/direct_send3_event.rds")


# 3) COMBINE EVENT DATA TOGETHER ####
## 3a) LIST DFS ####
event_dat_list <- list(direct_send1, direct_send2, direct_send3)

## 3b) CLEAN UP ####
rm(direct_send1)
rm(direct_send2)
rm(direct_send3)

## 3c) COMBINE DIRECT SEND DATA: ROWS, MATCH COLUMNS, FILL NA ####
# Binds and converts list to df:
ds_event_dat_df <- plyr::rbind.fill(event_dat_list) %>% # will fill/assign all of the same columns into the data, creates dataframe
  remove_empty("cols")
rm(event_dat_list)


# 4) CROSSCHECK EVENT DATA FOR UPDATED MOVEBANK COLUMNS ##############################################
# Occassionally movebank updates their column names/descriptions
# These may need to be updated in the direct send datasets since since they were first prepped
# Check columns agree below

## 4a) EVENT DATA COLUMN MAPPING ####
#  READ IN MOVEBANK DATA TO CHECK FORMAT:
movebank_event_dat_scID <- readRDS("./Data/Cleaned/03_movebank_event_dat_argosgps_scID.rds") %>%
  mutate(data.receipt.type = "movebank") #add identifier of how we received the data

# COMPARE COLUMNS TO SEE IF THEY MATCH BEFORE MERGING:
column_check_dat <- compare_df_cols(ds_event_dat_df, movebank_event_dat_scID)
rm(column_check_dat)

# Make any updates as necessary before merging


# 5) REF DATA #####################################################################################
## 5a) LOAD IN PREPPED REFERENCE DATA ####
ref_direct_send1 <- readRDS("./Data/DirectSend/rds/ref_direct_send1_event.rds")
ref_direct_send2 <- readRDS("./Data/DirectSend/rds/ref_direct_send2_event.rds")
ref_direct_send3 <- readRDS("./Data/DirectSend/rds/ref_direct_send3_event.rds")

## 5b) LIST DFs ####
ref_dat_list <- list(ref_direct_send1, ref_direct_send2, ref_direct_send3) 

## 5c) CLEAN UP ####
rm(ref_direct_send1) 
rm(ref_direct_send2)
rm(ref_direct_send3)

## 5d) COMBINE DIRECT SEND DATA: ROWS, MATCH COLUMNS, FILL NA ####
# COMBINE AND CONVERT TO DF:
ds_ref_dat_df <- plyr::rbind.fill(ref_dat_list) %>% # will fill/assign all of the same columns into the data, creates dataframe
  remove_empty("cols")
rm(ref_dat_list)


# 6) CROSSCHECK REF DATA FOR UPDATED MOVEBANK COLUMNS ####################################################
# Since data files were prepped, movebank revised some of their metadata names
# So may need to adjust column names

## 6a) REF DATA COLUMN MAPPING ####
# GET MOVEBANK REF DATA:
movebank_ref_dat_scID <- readRDS("./Data/Cleaned/03_movebank_ref_dat_scID.rds") 

# COMPARE COLUMNS TO SEE IF THEY MATCH BEFORE MERGING:
column_check_dat <- compare_df_cols(ds_ref_dat_df, movebank_ref_dat_scID)

# Make any updates as necessary before merging


# 7) COMBINE DIRECT SEND AND MOVEBANK DATASETS #####################################################
## 7a) EVENT DATA ####
event_dat_all <- bind_rows(movebank_event_dat_scID, ds_event_dat_df)
rm(movebank_event_dat_scID, ds_event_dat_df)

## 7b) REF DATA ####
ref_dat_all <- bind_rows(movebank_ref_dat_scID, ds_ref_dat_df)

## 7c) ADD SHOREBIRD COLLECTIVE DSA METADATA ####
# This adds Data Sharing Agreement and privacy info to all reference data
# LOAD:
all_argosgps_scmeta <- read.csv("./Data/MetadataExtract/argosgps_scMETA.csv")
# COMBINE:
ref_dat_all <- left_join(ref_dat_all, argosgps_scmeta_sn) #Joining by sc.dataset.id, species.code


############################################
# This leaves us with:
# 1) DETECTION DATA: both movebank and direct send 
event_dat_all <- event_dat_all %>%
  remove_empty("cols") 

# 2) REFERENCE DATA: both movebank and direct send
ref_dat_all <- ref_dat_all %>%
  remove_empty("cols")


# 8) SAVE DIRECT-SEND COMBINED DATA ######################################################################
saveRDS(event_dat_all, "./Data/Merged/04_all_event_dat.rds")
saveRDS(ref_dat_all, "./Data/Merged/04_all_refdat.rds")
