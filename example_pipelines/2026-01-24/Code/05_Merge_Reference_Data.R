# CODE: 05_Merge_Reference_Data.R
# CODE PURPOSE: MERGE REFERENCE DATA FROM MOVEBANK AND REFERENCE DATA DIRECTLY SENT TO SC AFTER FORMATTING 
#
# INTERNALLY PRIOR TO THIS STEP, THE SHOREBIRD COLLECTIVE FORMATTED DATASET REFERENCE DATA SENT DIRECTLY 
# ... to us from repositories other than Movebank.
# ... We formatted these reference data to match those downloaded from Movebank (data dictionary as of 2026-01-24)
# ... and we added columns sc_dataset_id, sc_individual_id, sc_deployment_id, sc_tracking_data_type 
# We then processed these data in the same way as the Movebank data in 03_Movebank_Format_Reference_Data.R 


# 00) SETUP ####
data_version <- "2026-01-24"

## SET WORKING DIRECTORY ##############################
setwd()


# 1) LOAD PACKAGES #################################################
library(tidyverse)
library(janitor)
library(sf)


## CUSTOM FUNCTIONS ####
# In functions folder
source("./Code/Functions/format_merged_refdat.r")


# 2) LOAD DATA ####################################################
## 2a) MOVEBANK DATA ####
refdat_merge_scids_df <- readRDS(paste0("./Data/rds/03_movebank_ref_to_merge_",data_version,".rds"))

## 2b) DIRECT SEND DATA ####
ds_refdat_merge_df <- readRDS(paste0("./Data/rds/directsend_ref_to_merge_",data_version,".rds"))

## 2c) COMPARE COLUMNS ####
compare_df_cols(refdat_merge_scids_df, ds_refdat_merge_df)

## 2d) DATASET DSA METADATA ####
# File internal to Shorebird Collective with dataset metadata 
# To add as table in geopackage
datasets_argosgps_meta_df <- read.csv(paste0("./Data/metadata_exports/datasets_argosgps_meta_df_", data_version,".csv"),
                                      fileEncoding = "latin1") 


# 3) COMBINE TOGETHER #############################################
ref_dat_df <- bind_rows(refdat_merge_scids_df, ds_refdat_merge_df) 

## 3a) ORGANIZE ####
ref_dat_df <- format_merged_refdat(ref_dat_df)

## 3b) SAVE ####
### RDS ####
saveRDS(ref_dat_df, paste0("./Data/rds/05_merged_deploy_ref_dat_",data_version,".rds"))

### CSV ####
write.csv(ref_dat_df, paste0("./Data/metadata_exports/05_merged_deploy_ref_dat_",data_version,".csv"))

# CLEAN UP:
rm(refdat_merge_scids_df, ds_refdat_merge_df)


# 4) ADD DATASET METADATA AS TABLE TO GEOPACKAGE ##########################################################
## 4o) LOAD DSA META AS NON-SPATIAL TABLE TO GEOPACKAGE ####
st_write(obj = datasets_argosgps_meta_df, 
         dsn = paste0("./Data/Geopackage/POINTS_latlon_",data_version,".gpkg"), 
         layer = paste0("dataset_metadata_df_", data_version),   
         append = TRUE) # or FALSE to overwrite

# CHECK GEOPACKAGE LAYERS:
st_layers(paste0("./Data/Geopackage/POINTS_latlon_",data_version,".gpkg"))
