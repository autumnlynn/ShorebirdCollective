# CODE: 02_Movebank_ArgosGPS_Download.R
# CODE PURPOSE: DOWNLOAD ARGOS, GPS, & ARGOS/GPS DATA (EVENTS & REFERENCE DAT) ####
# Movebank data library for more info: http://vocab.nerc.ac.uk/collection/MVB/current/ (as of 2026-01-24)
# Note: if a column in the reference data isn't listed, this may be a custom column added by data owner

# THIS CODE DOES THE FOLLOWING: 
# i) Downloads Argos/GPS/ARGOS&GPS datasets from a list of MovebankIDs
# ii) Downloads reference data for associated tag deployments
# iii) Downloads study data
# v) Summarizes number of events per deployment

# 00) SETUP ####
data_version <- "2026-01-24"
Sys.setenv(TZ = 'UTC')

## SET WORKING DIRECTORY ##############################
setwd()

## SPECIFY BASE FILEPATH FOR SAVING DATA ####
data_filepath <- "./Data/Movebank/"


# 1) LOAD PACKAGES #################################################
library(move) # 
library(tidyverse)
library(conflicted)
library(janitor)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")


# 2) LOG IN TO MOVEBANK ############################################
# To connect to data on the server
# Create object that stores login info
loginStored <- movebankLogin(username = "YourUserName", 
                             password = "YourPassword") 


# 3) LOAD MOVEBANK STUDIES OF INTEREST ######################################
## 3a) GET IDS BY DATA TYPE ####
# CREATE VECTORS OF "ARGOS" AND "GPS" DATA MOVEBANK STUDY IDS:
# To pull data types separately... 
# move::getMovebankLocationData function run if both Argos and GPS are listed at the same time

# ALL MOVEBANK STUDY IDS:
IDs_all <- c("studyid1", "studyid2", "studyid3")

# MOVEBANK STUDY IDS WITH ARGOS DATA: either only argos or both Argos & GPS:
IDs_argos <- c("studyid1", "studyid2") #replace with project IDs

# MOVEBANK STUDY IDS WITH GPS: either only GPS or both Argos & GPS
IDs_gps <- c("studyid1", "studyid3") #replace with project IDs


# 4) DOWNLOAD EVENT/LOCATION DATAFRAMES ########################################
# Have to download the Argos and GPS data separately because specifying both sensors will 
# ... only download data from individuals with both sensor types

## 4a) CREATE TIMESTAMP FOR DOWNLOAD FOR CHECKING FOR EVENTS WITH TIMESTAMPS IN THE FUTURE ####
now_utc <- Sys.time()           # system local time
now_utc <- as.POSIXct(now_utc, tz = "UTC")  # convert to UTC
now_utc


# LOOP TO GRAB ARGOS/GPS LOCATION DATA FOR ALL OF THE STUDY IDs: 
## 4b) DOWNLOAD ARGOS DATA ####
argos_dat_list <- list()

for (study_id in IDs_argos) {
  
    argos_dat <- getMovebankLocationData(
      study = study_id,
      sensorID = "Argos Doppler Shift",
      login = loginStored,
      includeOutliers = TRUE,
      underscoreToDots = FALSE
    )
    
    argos_dat_list[[as.character(study_id)]] <- argos_dat
}
# Will report warnings because GPS tag data will not be downloaded in this call (download separately below)
rm(argos_dat)

## 4c) DOWNLOAD GPS DATA ####
gps_dat_list <- list()

for (study_id in IDs_gps) {
  
  gps_dat <- getMovebankLocationData(
    study = study_id,
    sensorID = "GPS",
    login = loginStored,
    includeOutliers = TRUE,
    underscoreToDots = FALSE
  )
  
  gps_dat_list[[as.character(study_id)]] <- gps_dat
}
rm(gps_dat)


# 5) MERGE & FORMAT EVENT DATA ########################################################
## 5a) CREATE CONSISTENT COLUMNS ACROSS ALL DFs IN THE LIST ####
# and convert to data frame
argos_dat_df <- plyr::rbind.fill(argos_dat_list) # will fill/assign all of the same columns into the data, creates dataframe
gps_dat_df <- plyr::rbind.fill(gps_dat_list)

# Check columns are in same format for merging:
column_check_dat <- compare_df_cols(argos_dat_df, gps_dat_df) 
rm(column_check_dat)

# Calc total number of rows we should have in the end:
nrow(argos_dat_df) + nrow(gps_dat_df) 

## 5b) MERGE ARGOS AND GPS  & ARRANGE ####
movebank_event_dat_df <- plyr::rbind.fill(argos_dat_df, gps_dat_df) %>%
  arrange(study_id, individual_taxon_canonical_name, individual_id, deployment_id, timestamp) 

# crosscheck counts
nrow(movebank_event_dat_df)

## 5c) LIST BY STUDY_ID ####
# For organization
### SPLIT INTO LISTS BY STUDY ####
dat_list <- split(movebank_event_dat_df, movebank_event_dat_df$study_id)
rm(movebank_event_dat_df, argos_dat_df, gps_dat_df, gps_dat_list, argos_dat_list)

## 5d) SAVE EVENT DATA ####
### ADD IDENTIFIERS ####
dat_list <- dat_list %>%
  purrr::map(~.x %>%
              mutate(sc_download_timestamp = now_utc,
                      sc_data_receipt_origin = "Movebank"))

### CREATE WORKING DIRECTORIES IF NEEDED ####
iwalk(
  dat_list,
  ~ dir.create(
    file.path(data_filepath, .y), #.y is the name of the list
    recursive = TRUE,
    showWarnings = FALSE
  )
)

### SAVE EVENT DATA TO BASE FILE PATH ####
# Note: If data were already in folder, this will overwrite it unless detaching/appending
purrr::map2(dat_list, names(dat_list), 
            ~write.csv(.x, paste0(data_filepath,"/", .y , "/", .y, "_event_dat.csv")))

### SAVE RDS ####
saveRDS(dat_list, paste0("./Data/rds/02_movebank_download_eventdat_lst_",data_version,".rds"))


# 6) DOWNLOAD REFERENCE/DEPLOYMENT DATA #######################################################
## 6a) DOWNLOAD ####
### CREATE EMPTY DATAFRAME:
ref_dat_list <- list() 

### LOOP TO GRAB REFERENCE DATA FOR ALL OF THE STUDY IDs ####
for (study_id in IDs_all){
  ref_dat <- getMovebankReferenceTable(study = study_id, 
                                       login = loginStored) 
  ref_dat_list[[as.character(study_id)]] <- ref_dat
}

## 6b) SAVE WITH EXTENSION OF DOWNLOAD DATE ####
### ARRANGE DATA ####
ref_dat_list <- ref_dat_list %>%
  purrr::map(~.x %>%
               arrange(study_id, animal_taxon_canonical_name, animal_id, deployment_id) %>%
               mutate(sc_download_timestamp = now_utc,
                      sc_data_receipt_origin = "Movebank"))

### SAVE REFERENCE DATA TO BASE FILEPATHS ####
# Note: Will not over-write the data if a difference data_version specified
purrr::map2(ref_dat_list, names(ref_dat_list), 
            ~write.csv(.x, paste0(data_filepath,"/", .y , "/", .y, "_ref_dat_", data_version,".csv")))

### FORMAT CONSISTENT COLUMN CLASSES ACROSS DATASETS ####
ref_dat_df <- plyr::rbind.fill(ref_dat_list) %>% # will fill/assign all of the same columns into the data, creates dataframe
  arrange(study_id, animal_taxon_canonical_name, animal_id, deployment_id)

### SPLIT BACK INTO LISTS BY STUDY ID:
ref_dat_list <- split(ref_dat_df, ref_dat_df$study_id)

### SAVE RDS ####
saveRDS(ref_dat_list, paste0("./Data/rds/02_movebank_download_refdat_lst_",data_version,".rds"))


# 7) DOWNLOAD STUDY DATA #################################################
## 7a) DOWNLOAD ####
study_dat_list <- list()

for (study_id in IDs_all){
  study_dat <- getMovebankStudy(study = study_id,
                                login = loginStored)
  study_dat_list[[as.character(study_id)]] <- study_dat
}
study_dat_list

### CHECK COLUMNS DOWNLOADED:
study_dat_list %>%
  purrr::map(~.x %>% str())

## 7b) ADD IDENTIFIERS ####
study_dat_list <- study_dat_list %>%
  purrr::map(~.x %>%
               mutate(sc_download_timestamp = now_utc,
                      sc_data_receipt_origin = "Movebank"))

## 7c) SAVE STUDY DATA TO BASE FILEPATHS ####
purrr::map2(study_dat_list, names(study_dat_list), 
            ~write.csv(.x, paste0(data_filepath,"/", .y , "/", .y, "_study_dat_", data_version,".csv")))

### SAVE RDS ####
saveRDS(study_dat_list, paste0("./Data/rds/02_movebank_download_studydat_lst_",data_version,".rds"))


# 8) SUMMARIZE EVENT DATA ###############################################################
# Summarise observations from event data because these are only for confirmed deployments
# Helpful for assessing if new deployments and events have been added in the future

## 8a) EVENT DEP SUMMARY ####
event_count_list <- dat_list  %>%
  purrr::map(~.x %>%
               group_by(study_id, individual_taxon_canonical_name, individual_local_identifier, individual_id, deployment_id, sensor_type) %>%
               count() %>%
               ungroup() %>%
               pivot_wider(names_from = sensor_type, 
                           values_from = n, 
                           values_fill = 0) %>%
               mutate(
                 `Argos Doppler Shift` = if ("Argos Doppler Shift" %in% names(.)) `Argos Doppler Shift` else 0, # if no argos column add it and fill with 0
                 GPS = if ("GPS" %in% names(.)) GPS else 0 # if no gps column add it and fill with 0
               ) %>% 
               rename(n_argos_events = `Argos Doppler Shift`, n_gps_events = `GPS`) %>%
               mutate(n_total_events = n_argos_events + n_gps_events,
                      sc_tracking_data_type = case_when(n_argos_events > 0 & n_gps_events == 0 ~ "argos",
                                                        n_argos_events > 0 & n_gps_events > 0 ~ "argosgps",
                                                        n_argos_events == 0 & n_gps_events > 0 ~ "gps", 
                                                        .default = NA_character_)) %>%
               arrange(study_id, individual_taxon_canonical_name, individual_id, deployment_id)) 

## 8b) EVENT TS SUMMARY ####
event_startend_list <- dat_list  %>%
  purrr::map(~.x %>%
               select(study_id, individual_taxon_canonical_name, individual_id, deployment_id, timestamp) %>%
               group_by(study_id, individual_taxon_canonical_name, individual_id, deployment_id) %>%
               summarise(first_timestamp = min(timestamp),
                         last_timestamp = max(timestamp),
                         .groups = "drop"))

## 8c) COMBINE TOGETHER ####
event_sum_list <- purrr::map2(event_count_list, event_startend_list, 
                              ~left_join(.x, .y)) # Joining with `by = join_by(study_id, individual_taxon_canonical_name, individual_id, deployment_id)`

rm(event_startend_list, event_count_list)

## 8d) ADD IDENTIFIERS ####
event_sum_list <- event_sum_list %>%
  purrr::map(~.x %>%
               mutate(sc_download_timestamp = now_utc,
                      sc_data_receipt_origin = "Movebank"))

## 8e) SAVE EVENT SUMMARY DATA TO BASE FILEPATHS ####
purrr::map2(event_sum_list, names(event_sum_list), 
            ~write.csv(.x, paste0(data_filepath,"/", .y , "/", .y, "_event_summary_dat_", data_version,".csv")))

### SAVE RDS ####
saveRDS(event_sum_list, paste0("./Data/rds/02_movebank_download_event_summary_lst_",data_version,".rds"))
