# CODE: 03_Movebank_Format_Reference_Data.R
# CODE PURPOSE: FORMAT REFERENCE DATA IN CONSISTENT WAY ACROSS DATASETS


# 00) SETUP ####
data_version <- "2026-01-24"

## SET WORKING DIRECTORY ##############################
setwd()

## SPECIFY BASE FILEPATH FOR SAVING DATA ####
data_filepath <- "./Data/Movebank/"


# 1) LOAD PACKAGES #################################################
library(tidyverse)

## CUSTOM FUNCTIONS ####
# In the functions folder
source("./Code/Functions/subset_movebank_reference_columns.r")
source("./Code/Functions/format_movebank_reference_data.r")
source("./Code/Functions/add_sc_dataset_ids.r")


# 2) LOAD DATA #####################################################
## 2a) REFERENCE DATA ####
ref_dat_list <- readRDS(paste0("./Data/rds/01_movebank_download_refdat_lst_",data_version,".rds"))

## 2b) EVENT DATA SUMMARY ####
# Has sc_tracking_data_type column added to it
event_sum_list <- readRDS(paste0("./Data/rds/01_movebank_download_event_summary_lst_",data_version,".rds"))

## 2c) SC DEPLOYMENT START/END TIMES ####
sc_deps_df <- read.csv(paste0(data_filepath, "./movebank_sc_deployment_ts_",data_version,".csv"),
                       na.strings = c("", " "))

## 2d) ESTIMATED DUTY CYCLE INFO ####
duty_cycle_df_list <- readRDS(paste0("./Data/rds/02_est_dutycycle_lst_",data_version,".rds")) %>%
  purrr::map(~.x %>%
  select(deployment_id, sc_estimated_duty_cycle_description))

## 2e) LOAD SC IDS TO MAP ####
sc_ids_df <- read.csv("./Data/metadata_exports/02_movebank_scids_to_map.csv")


# 3) SUBSET ROWS AND COLUMNS ############################################
## 3a) SUBSET TO MOVEBANK COLUMNS ####
ref_dat_list_sub <- ref_dat_list %>%
  purrr::map(~subset_movebank_reference_columns(.x))

## 3b) FORMAT MOVEBANK REFERENCE DATA ####
# Match column names in ref data to the event data col names
# Standardize some columns 
# Create an overall notes column 
ref_dat_list_sub <- ref_dat_list_sub %>%
  purrr::map(~format_movebank_reference_data(.x))


# 4) ADD OTHER HELPFUL SC DERIVED COLUMNS ####################################################################
## 4a) ADD TRACKING DATA TYPE TO dat_list ####
ref_dat_list_sub <- purrr::map2(ref_dat_list_sub, event_sum_list %>%
                          purrr::map(~.x %>% select(individual_id, deployment_id, sc_tracking_data_type)),
                        ~left_join(.x,.y)) # Joining with `by = join_by(individual_id, deployment_id)`

## 4b) ADD SC MANUALLY IDENTIFIED START AND END TIMES ####
ref_dat_list_sub <- purrr::map(ref_dat_list_sub,  
  ~left_join(.x,sc_deps_df %>%  # Joining with `by = join_by(deployment_id)`
               select(any_of(c("deployment_id", "sc_deploy_on_timestamp", "sc_deploy_off_timestamp", "sc_deployment_on_notes", "sc_deployment_off_notes"))) %>%
               distinct() %>%
               mutate(sc_deploy_on_timestamp = lubridate::ymd_hms(sc_deploy_on_timestamp), # format the timestamps
                      sc_deploy_off_timestamp = lubridate::ymd_hms(sc_deploy_off_timestamp))))

## 4c) ADD ESTIMATED DUTY CYLE INFO ####
ref_dat_list_sub <- purrr::map2(ref_dat_list_sub, duty_cycle_df_list,  
                               ~left_join(.x,.y)) # Joins by deployment_id

## 4d) CONVERT TO DF ####
refdat_merge_df <- purrr::map_dfr(ref_dat_list_sub,
  ~ .x %>% as.data.frame())

## 4e) ADD SC IDS ####
refdat_merge_scids_df <- left_join(refdat_merge_df, sc_ids_df %>%
                                     select(-individual_local_identifier)) %>% 
   filter(!is.na(sc_deployment_id)) # remove deployment rows that we don't have event data for!

## 4f) SAVE ####
### RDS to add to refdata later:
saveRDS(refdat_merge_scids_df, paste0("./Data/rds/03_movebank_ref_to_merge_",data_version,".rds"))

