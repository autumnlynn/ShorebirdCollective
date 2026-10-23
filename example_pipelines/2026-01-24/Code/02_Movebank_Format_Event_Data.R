# CODE: 02_Movebank_Format_Event_Data.R
# CODE PURPOSE: FORMAT ARGOS, GPS, & ARGOS/GPS EVENT DATA IN CONSISTENT WAY ACROSS DATASETS
# FLAG COMMMON DATA PROBLEMS FOR REMOVAL
# CREATE DIAGNOSTIC FIGURES OF SENSOR DATA TO INVESTIGATE DEAD/SHED TAGS
# ADD HELPFUL COLUMNS FOR PIPELINE
# Loops through multiple datasets at once
# More filters may be required for your dataset

# 00) SETUP ####
data_version <- "2026-01-24"

## SET WORKING DIRECTORY ##############################
setwd()
Sys.setenv(TZ = 'UTC')


## SPECIFY BASE FILEPATH FOR SAVING DATA ####
data_filepath <- "./Data/Movebank/"


# 1) LOAD PACKAGES #################################################
library(tidyverse)

## CUSTOM FUNCTIONS: IN FUNCTIONS FOLDER ####
# Note: these functions contain descriptions that describe constraints
# Please read the function details for more information on needed inputs and constraints
source("./Code/Functions/subset_movebank_columns.r")
source("./Code/Functions/format_timestamps.r")
source("./Code/Functions/add_argos_lc_for_gps.r")
source("./Code/Functions/add_sequential_ids.r")
source("./Code/Functions/add_quality_filter_flags.r")
source("./Code/Functions/flag_complete_duplicates.r")
source("./Code/Functions/flag_failed_sc_filters.r")
source("./Code/Functions/flag_worse_ts_duplicates.r")
source("./Code/Functions/split_at_gap.r") # needed for flag_manufacturer_locations
source("./Code/Functions/flag_manufacturer_locations.r")
source("./Code/Functions/flag_test_sites.r")
source("./Code/Functions/flag_movebank_sc_mmo.r")
source("./Code/Functions/decode_argos_sensors.r")
source("./Code/Functions/create_argos_diagnostic_plots.r")
source("./Code/Functions/create_nonargossensor_figs.r")
source("./Code/Functions/flag_sc_deploy_times.r")
source("./Code/Functions/make_sampling_interval_figs.r")
source("./Code/Functions/estimate_duty_cycle_df.r")
source("./Code/Functions/extract_deploy_on_locations.r")
source("./Code/Functions/extract_deploy_off_locations.r")
source("./Code/Functions/add_sc_dataset_ids.r")
source("./Code/Functions/merge_format_event_as_df.r")
source("./Code/Functions/add_sc_ids.r")


# 2) LOAD DATA #####################################################
## 2a) EVENT DATA ####
dat_list <- readRDS(paste0("./Data/rds/02_movebank_download_eventdat_lst_",data_version,".rds"))

## 2b) EVENT DATA SUMMARY ####
# Has sc_tracking_data_type added to it
event_sum_list <- readRDS(paste0("./Data/rds/02_movebank_download_event_summary_lst_",data_version,".rds"))

## 2c) LOAD SC MANUALLY MARKED OUTLIER LIST ####
# A list of event_id from Movebank identified as outliers by Shorebird Collective that were problems 
# But not removed in filtering process (typically multi-event spikes)
sc_mmo_df <- read.csv(paste0(data_filepath, "./movebank_sc_manually_flagged_outliers_",data_version,".csv"))

## 2d) LOAD SC DEPLOYMENT START/END TIMES ####
# Deployment start and end times identified by the SC that were not identified in original Movebank studies
sc_deps_df <- read.csv(paste0(data_filepath, "./movebank_sc_deployment_ts_",data_version,".csv"),
                       na.strings = c("", " "))

## 2e) LOAD REFERENCE DATA ####
ref_dat_list <- readRDS(paste0("./Data/rds/02_movebank_download_refdat_lst_",data_version,".rds"))

## 2f) LOAD SHOREBIRD COLLECTIVE DATASET METADATA ####
# To merge sc_dataset_id and species_code identifiers
datasets_argosgps_meta_df <- read.csv(paste0("./Data/metadata_exports/01_datasets_argosgps_meta_df_", data_version,".csv"),
fileEncoding = "latin1")


# 3) FORMAT DATASETS FOR PIPELINES #########################################################

## 3a) SUBSET MOVEBANK COLUMNS TO THOSE TO CARRY THROUGH PIPELINE ####
# Note, pulls "individual_taxon_canonical_name", "individual_local_identifier" which could change if data owner changes these names
dat_list_sub <- dat_list %>%
  purrr::map(~subset_movebank_columns(.x))

## 3b) FORMAT TIMESTAMPS ####
dat_list_sub <- dat_list_sub %>%
  purrr::map(~format_timestamps(.x, deployment_col = deployment_id))

## 3c) UPDATE ARGOS LC CODE ####
# Add G for "GPS" as it is an accepted value in Movebank, helpful in pipeline, and used for aniMotum
dat_list_sub <- dat_list_sub %>%
  purrr::map(~add_argos_lc_for_gps(.x))

## 3d) ADD DEPLOYMENT LOCATIONS AS EVENTS ####
# Helpful for identifying spikes at the beginning/end of tag deployments
# And reconstructing paths if tag does not record a location near the tagging site

### ADD LOCATION ORIGIN COLUMN ####
dat_list_sub <- dat_list_sub %>%
  purrr::map(~.x %>%
               mutate(sc_location_origin = "movebank_event"))

### GET DEP ON LOCATIONS TO ADD ####
dep_on_locs_lst <- ref_dat_list %>%
  purrr::map(~extract_deploy_on_locations(.x)) # get both deployment on and off locations

### GET DEP OFF LOCATIONS TO ADD ####
dep_off_locs_lst <- ref_dat_list %>%
  purrr::map(~extract_deploy_off_locations(.x))

### ADD ON LOCATIONS TO EVENT DATA ####
dat_list_sub <- purrr::map2(dat_list_sub, dep_on_locs_lst,
                    ~bind_rows(.x,.y) %>% 
                      arrange(deployment_id, timestamp))

### ADD OFF LOCATIONS TO EVENT DATA ####
dat_list_sub <- purrr::map2(dat_list_sub, dep_off_locs_lst,
                            ~bind_rows(.x,.y) %>% 
                              arrange(deployment_id, timestamp))

rm(dep_on_locs_lst, dep_off_locs_lst)

## 3e) ADD COLUMN IDENTIFIERS: SEQUENTIAL ID ####
# Helps with organization and querying later
dat_list_sub <- dat_list_sub %>%
  purrr::map(~add_sequential_ids(.x, deployment_col = deployment_id))

## 3f) ADD TRACKING DATA TYPE TO EVENT DATA ####
dat_list_sub <- purrr::map2(dat_list_sub, event_sum_list %>%
                          purrr::map(~.x %>% select(individual_id, deployment_id, sc_tracking_data_type)),
                        ~left_join(.x,.y)) # Joining with `by = join_by(individual_id, deployment_id)`


# 4) FLAG DATA FOR PREFILTERING ############################################

## 4a) FLAG QUALITY FILTERS ####
# CREATE FLAG FILTERS FOR THE FOLLOWING COMMON DATA PROBLEMS: 
# See add_quality_filter_flags.r function for more information"
#
# i) INCOMPLETE CASES
# ii) NO LC CLASS
# iii) FAILED LOTEK CYCLIC REDUNDANCY CHECK
# iv) EVENT IN THE FUTURE 
# v) MANUALLY MARKED OUTLIER (contributor identified data error)
# vi) FLAG BAD LATITUDES
# vii) FLAG BAD LONGITUDES 
# viii) ARGOS Z'S
# ix) CTT ERROR CODES
# x) LAT LONG AT 0,0

dat_list_sub <- dat_list_sub %>%
  purrr::map(~add_quality_filter_flags(.x))

# HERE YOU CAN CHECK THE PROPORTION OF LOCATIONS IN EACH DATASET FLAGGED FOR REMOVAL:
# 0 = FAIL
# 1 = PASS
dat_list_sub %>% map(~.x %>% group_by(filter_incomplete_case) %>% count()) 
dat_list_sub %>% map(~.x %>% group_by(filter_no_lc_class, argos_lc, sc_tracking_data_type, sensor_type) %>% count()) 
dat_list_sub %>% map(~.x %>% group_by(filter_GPS_crc_fail, sensor_type) %>% count()) 
dat_list_sub %>% map(~.x %>% group_by(filter_future_detection) %>% count()) 
dat_list_sub %>% map(~.x %>% group_by(filter_manually_marked_outlier) %>% count()) 
dat_list_sub %>% map(~.x %>% group_by(filter_bad_lat) %>% count()) 
dat_list_sub %>% map(~.x %>% group_by(filter_bad_lon) %>% count()) 
dat_list_sub %>% map(~.x %>% group_by(filter_argos_z) %>% count()) 
dat_list_sub %>% map(~.x %>% group_by(filter_ctt_error) %>% count()) #This is not a Movebank column
dat_list_sub %>% map(~.x %>% group_by(filter_ll_00) %>% count()) 

## 4b) FLAG COMPLETE DUPLICATES ####
# Flags rows with the same timestamp, LC codes, argos semimajor and minor ellipses and orientations, 
# ... sensor info, CTT errors, manual marked outlier status, etc

dat_list_sub <- dat_list_sub %>%
  purrr::map(~flag_complete_duplicates(.x, event_col = event_id))

# CHECK:
dat_list_sub %>% map(~.x %>% group_by(filter_complete_duplicate) %>% count()) 

## 4c) FLAG THOSE THAT FAILED ANY FILTER SO FAR ####
dat_list_sub <- dat_list_sub %>%
  purrr::map(~flag_failed_sc_filters(.x))

# CHECK:
dat_list_sub %>% map(~.x %>% group_by(any_failed_sc_filters) %>% count()) 

## 4d) FLAG WORSE QUALITY LOCATIONS FOR TIMESTAMP DUPLICATES ####
# Animotum will drop one of two timestamp duplicates but will not pick the highest quality
# We prefer to retain the highest quality Argos location class or smallest major/minor axes
# Currently don't have a way to select higher quality GPS locations
dat_list_sub <- dat_list_sub %>%
  purrr::map(~flag_worse_ts_duplicates(.x, 
                                       deployment_col = deployment_id))
# CHECK:
dat_list_sub %>% map(~.x %>% group_by(filter_worse_ts_duplicate) %>% count()) 

## 4e) FLAG OBS OCCURING AT MANUFACTURING FACILITIES ####
### FIRST UPDATE ANY FAILED FILTER COLUMN ####
dat_list_sub <- dat_list_sub %>%
  purrr::map(~flag_failed_sc_filters(.x))

### FLAG LOCATIONS AT MANUFACTURER LOCATIONS ####
# See function for more details 
# Default is for Microwave Telemetry facility -- could adjust for other sites 
dat_list_sub <- dat_list_sub %>%
  purrr::map(~flag_manufacturer_locations(.x, 
                                          deployment_col = deployment_id))

# CHECK:
dat_list_sub %>% map(~.x %>% group_by(filter_manufacturer_loc) %>% count()) 

## 4f) FLAG KNOWN TEST SITES ####
## CREATE DF OF TEST SITES ####
# See function for more details
# Requires an input of known tag test sites that should be removed formatted like this:
test_sites_df <- data.frame(site = c("site1", "site2", "site3", "site4"),
                         species.code = c("scientific name 1","scientific name 1",
                                          "scientific name 2", "scientific name 3"),
                         lon = c(longitude_decimal_deg1, longitude_decimal_deg2, longitude_decimal_deg3, longitude_decimal_deg4),
                         lat = c(latitude_decimal_deg1, latitude_decimal_deg2, latitude_decimal_deg3, latitude_decimal_deg4))

dat_list_sub <- dat_list_sub %>%
  purrr::map(~flag_test_sites(.x, site_df = test_sites_df,
                              species_col = individual_taxon_canonical_name,
                                          deployment_col = deployment_id))

# CHECK:
dat_list_sub %>% map(~.x %>% group_by(filter_test_sites) %>% count()) 

rm(test_sites_df)

## 4g) MAKE SURE ALL ARE FORMATTED AS DF ####
dat_list_sub <- dat_list_sub %>%
  purrr::map(~.x %>% as.data.frame())


# 5) FLAG SC MANUALLY MARKED OUTLIERS ###############################################
# Flag events manually identified as problems by the SC
# Will merge by Movebank event_id 
# event_id should stay stable unless owner removes track and completely reuploads it
dat_list_sub <- dat_list_sub %>%
  purrr::map(~flag_movebank_sc_mmo(.x, sc_mmo_df = sc_mmo_df))

# CHECK:
dat_list_sub %>% map_dfr(~.x %>% as.data.frame()) %>%
                       filter(filter_sc_manually_marked_outlier == 0) %>%
  nrow() 
nrow(sc_mmo_df) 


# 6) DECODE ARGOS SENSOR DATA (IF IT EXISTS) ################################
### FIRST UPDATE ANY FAILED FILTER COLUMN ####
dat_list_sub <- dat_list_sub %>%
  purrr::map(~flag_failed_sc_filters(.x))

## 6a) DECODE ARGOS SENSOR COLUMNS IF THEY EXIST ####
# ASSUMING PTT-100 decoding formulas (Note -- could be incorrect for sunbirds etc)
# See function notes for more details
argos_sensor_lst <- dat_list_sub %>%
  purrr::map(~decode_argos_sensors(.x,
                                   deployment_col = deployment_id,
                                   study_col = study_id, 
                                   individual_col = individual_id))

## 6b) FILTER TO STUDIES WITH ARGOS SENSOR DATA ####
argos_sensor_lst <- argos_sensor_lst %>%
  purrr::keep(~ inherits(.x, "data.frame") && nrow(.x) > 0) 

### ADD IDENTIFIERS ####
now_utc <- as.POSIXct("2026-01-24 16:15:02 UTC", tz = "UTC") # timestamp we downloaded from Movebank

argos_sensor_lst <- argos_sensor_lst %>%
  purrr::map(~.x %>%
               mutate(sc_download_timestamp = now_utc,
                      sc_data_receipt_origin = "Movebank"))

## 6c) SAVE DECODED SENSOR DATA WITH IDENTIFIERS ####
#### CREATE WORKING DIRECTORY IF NOT DONE YET*
iwalk(
  argos_sensor_lst,
  ~ dir.create(
    file.path(paste0(data_filepath, .y,"/Decoded_Sensor/")), #.y is the name of the list (the Movebank study_id)
    recursive = TRUE,
    showWarnings = FALSE
  )
)

# Note: If data were already in the folder, this will overwrite it unless detaching/appending
purrr::map2(argos_sensor_lst, names(argos_sensor_lst), 
            ~write.csv(.x, paste0(data_filepath, .y ,"/Decoded_Sensor/", .y, "_decoded_argos_sensor_dat.csv")))

## 6d) CREATE FIGURES DECODED SENSOR FIGURES PER BIRD ####
argos_sensor_figs_lst <- argos_sensor_lst %>%
  purrr::map(~create_argos_diagnostic_plots(.x,
                                            deployment_col = deployment_id))

## 6e) SAVE FIGS ####
iwalk(argos_sensor_figs_lst, function(study_plots_lst, study_id) {
  
  # SPECIFY STUDY FILE PATH:
  figures_dir <- file.path(paste0(data_filepath, study_id ,"/Decoded_Sensor/Figures/"))
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
  
  # LOOP THROUGH EACH DEPLOYMENT PLOT INSIDE THIS STUDY
  iwalk(study_plots_lst, function(deployment_plot, deployment_id) {
    
      # FILE PATH PER DEPLOYMENT
      file_path <- file.path(figures_dir, paste0(deployment_id, "_ptt_diag_plot.png"))
      
      # SAVE THE PLOT
      cowplot::save_plot(plot = deployment_plot,
                filename = file_path, 
                device = "png",
                base_height = 8,
                base_width = 11,
                dpi = 150,
                bg = "white")
    })
})
rm(argos_sensor_figs_lst)


# 7) CREATE FIGS FOR BIRDS WITHOUT ARGOS SENSORS ################################
# These show argos_lc codes over time by lat and long

## 7a) CREATE THE FIGS ####
nonargos_figs_lst <- dat_list_sub %>%
  purrr::map(~create_nonargossensor_figs(.x,
                                            deployment_col = deployment_id,
                                            study_col = study_id))

### DROP LISTS WITH NO FIGS ####
nonargos_figs_lst <- purrr::keep(nonargos_figs_lst, ~length(.x) > 0)

## 7b) SAVE THE FIGS ####
#### CREATE WORKING DIRECTORY IF NEEDED:
iwalk(
  nonargos_figs_lst,
  ~ dir.create(
    file.path(paste0(data_filepath, .y,"/OtherLatLonFigs/")), #.y is the name of the list
    recursive = TRUE,
    showWarnings = FALSE
  )
)

### SAVE FIGS ####
iwalk(nonargos_figs_lst, function(study_plots_lst, study_id) {
  
  # SPECIFY STUDY FILE PATH:
  figures_dir <- file.path(paste0(data_filepath, study_id ,"/OtherLatLonFigs/"))
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
  
  # LOOP THROUGH EACH DEPLOYMENT PLOT INSIDE THIS STUDY
  iwalk(study_plots_lst, function(deployment_plot, deployment_id) {
    
     # FILE PATH PER DEPLOYMENT
      file_path <- file.path(figures_dir, paste0(deployment_id, "_latlon_plot.png"))
      
      # SAVE THE PLOT
      cowplot::save_plot(plot = deployment_plot,
                filename = file_path, 
                device = "png",
                base_height = 8,
                base_width = 11,
                dpi = 150,
                bg = "white")
  })
})
rm(nonargos_figs_lst)


# 8) FLAG SC IDENTIFIED DEPLOYMENT ISSUES ###########################################
## 8a) FORMAT TIMESTAMPS FOR DEPLOYMENT TIMES ####
sc_deps_df_formatted <- sc_deps_df %>%
  mutate(sc_deploy_on_timestamp = lubridate::ymd_hms(sc_deploy_on_timestamp, tz = "UTC"),
         sc_deploy_off_timestamp = lubridate::ymd_hms(sc_deploy_off_timestamp, tz = "UTC"))
rm(sc_deps_df)

## 8b) FLAG OBS OUTSIDE OF DEPLOYMENT TIMES ####
dat_list_sub <- dat_list_sub %>% 
  purrr::map(~flag_sc_deploy_times(.x, sc_deps_df_formatted,
                                   dat_deploy_col = deployment_id,
                                   sc_deploy_col = deployment_id))

# CHECK:
dat_list_sub %>% map(~.x %>% group_by(filter_sc_deploy_off_time) %>% count())
dat_list_sub %>% map(~.x %>% group_by(filter_sc_deploy_on_time) %>% count())

## 8c) UPDATE ANY FLAGGED FILTERS COLUMN ####
dat_list_sub <- dat_list_sub %>%
  purrr::map(~flag_failed_sc_filters(.x))

# CHECK:
dat_list_sub %>% map(~.x %>% group_by(any_failed_sc_filters) %>% count()) 

## 8d) SAVE ####
saveRDS(dat_list_sub, paste0("./Data/rds/03_dat_list_flagged_", data_version))


# 9) ESTIMATE TAG SAMPLE INTERVALS ###########################################################
# Create figs showing frequency of different time gap intervals recorded by each tag
# Helpful for assessing if the tag was duty cycled or not

## 9a) CREATE PLOTS OF SAMPLING INTERVALS ####
sampling_figs_lst <- dat_list_sub %>%
  purrr::map(~make_sampling_interval_figs(.x, deployment_col = deployment_id))

## 9b) SAVE FIGS ####
iwalk(sampling_figs_lst, function(study_plots_lst, study_id) {
  
  # SPECIFY STUDY FILE PATH:
  figures_dir <- file.path(paste0(data_filepath, study_id ,"/Sampling_Interval/png/"))
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
  
  # LOOP THROUGH EACH DEPLOYMENT PLOT INSIDE THIS STUDY
  iwalk(study_plots_lst, function(deployment_plot, deployment_id) {
    
    # FILE PATH PER DEPLOYMENT
    file_path <- file.path(figures_dir, paste0(deployment_id, "_samplinginterval_plot.png"))
    
    # SAVE THE PLOT
    save_plot(plot = deployment_plot,
              filename = file_path, 
              device = "png",
              base_height = 8,
              base_width = 10,
              dpi = 150,
              bg = "white")
  })
})
rm(sampling_figs_lst)

## 9d) ESTIMATE DUTY CYCLE ####
# Estimate duty cycles of tags in a coarse way (see constraints in function code)
duty_cycle_df_list <- dat_list_sub %>%
  purrr::map(~estimate_duty_cycle_df(.x, deployment_col = deployment_id))

## 9e) SAVE ESTIMATED DUTY CYCLE DFs ####
### CSV for later reference:
purrr::map2(duty_cycle_df_list, names(duty_cycle_df_list), 
            ~write.csv(.x, paste0(data_filepath, .y ,"/Sampling_Interval/", .y, "_est_duty_cycle_dat.csv")))

### RDS to add to refdata later:
saveRDS(duty_cycle_df_list, paste0("./Data/rds/03_est_dutycycle_lst_",data_version,".rds"))


# 10) PREPARE DATASETS FOR MERGE ############################################################
## 10a) DROP A FEW COLUMNS NO LONGER NEEDED & MERGE INTO DF ####
### DROP THE COLS AND MERGE INTO ONE ####
dat_merge_df <- merge_format_event_as_df(dat_list_sub) 

## 10b) ADD sc_dataset_id ####
# This is a unqiue dataset identifier of Shorebird Collective 
# Will be used later to append Data Sharing Aggreement Terms 

dat_merge_datasets_df <- add_sc_dataset_ids(datasets_argosgps_meta_df, dat_merge_df) # should have two more columns but no more rows
dat_merge_datasets_df %>% glimpse()

## 10c) CREATE/ADD SC DEP AND INDIVIDUAL IDS ####
# Create unique identifiers sc_individual_id and sc_deployment_id that are unique across all data 
# ... including datasets sent directly (not from Movebank)
dat_merge_scids_df <- add_sc_ids(dat_merge_datasets_df)

## 10d) SAVE EVENT DATA FOR MERGING ####
saveRDS(dat_merge_scids_df, paste0("./Data/rds/03_movebank_event_to_merge_",data_version,".rds"))

rm(dat_merge_datasets_df, dat_merge_df)

## 10e) SAVE SC_IDS TO ADD TO REF DATA ####
sc_ids_df <- dat_merge_scids_df %>%
  distinct(deployment_id, individual_local_identifier,
           species_code, sc_dataset_id, sc_individual_id, sc_deployment_id)

### AND AS CSV FOR RECORDS:
write.csv(sc_ids_df, "./Data/metadata_exports/03_movebank_scids_to_map.csv")

