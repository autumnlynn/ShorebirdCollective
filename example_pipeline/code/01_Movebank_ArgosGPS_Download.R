# CODE: 01_Movebank_ArgosGPS_Download.R
# CODE PURPOSE: DOWNLOAD ARGOS, GPS, & ARGOS/GPS DATA (EVENTS & REFERENCE DAT) ####
# And do some formatting for consistent output across datasets
# Movebank data library for more info: http://vocab.nerc.ac.uk/collection/MVB/current/


# THIS CODE DOES THE FOLLOWING: 
# i) Downloads Argos/GPS/ARGOS&GPS datasets from a list of MovebankIDs
# ii) Downloads reference data for associated tag deployments


# 0) SET WORKING DIRECTORY ##############################
setwd()
Sys.setenv(TZ = 'UTC')


# 1) LOAD PACKAGES #################################################
library(move)
library(tidyverse)
library(conflicted)
library(janitor)
conflict_prefer("filter", "dplyr")
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


# 4) DOWNLOAD EVENT DATAFRAMES (FULL DATA) ########################################
# LOOP TO GRAB DETECTION DATA FOR ALL OF THE STUDY IDs 
# Have to download the Argos and GPS data separately (selecting multiple data types does not work)

## 4a) DOWNLOAD ARGOS DATA ####
# CREATE EMPTY LIST FOR DFs:
argos_dat_list <- list()

for (i in 1:length(IDs_argos)){
  argos_dat <- getMovebankLocationData(study = IDs_argos[i],
                          sensorID = "Argos Doppler Shift",
                          login = loginStored,
                          includeOutliers = TRUE)
  argos_dat_list[[i]] <- argos_dat
}
# Will report warnings because GPS tag data will not be downloaded in this call (download separately below)
rm(argos_dat)


## 4b) DOWNLOAD GPS DATA ####
# CREATE EMPTY LIST FOR DFs:
gps_dat_list <- list()

for (i in 1:length(IDs_gps)){
  gps_dat <- getMovebankLocationData(study = IDs_gps[i],
                                 sensorID = "GPS",
                                 login = loginStored,
                                 includeOutliers = TRUE)
  gps_dat_list[[i]] <- gps_dat
}
rm(gps_dat)

## 4c) FORMAT LIST NAMES ####
# ADD MOVEBANK STUDY ID AS LIST NAME:
names(argos_dat_list) <- IDs_argos #helps with organization
names(gps_dat_list) <- IDs_gps 

## 4d) CREATE CONSISTENT COLUMNS ACROSS ALL DFs IN THE LIST ####
# and convert to data frame
argos_dat_df <- plyr::rbind.fill(argos_dat_list) # will fill/assign all of the same columns into the data, creates dataframe
gps_dat_df <- plyr::rbind.fill(gps_dat_list)

## 4e) COMBINE ARGOS & GPS DETECTIONS INTO SINGLE DATAFRAME ####
# Check columns are in same format for merging:
column_check_dat <- compare_df_cols(argos_dat_df, gps_dat_df) 
rm(column_check_dat)
# Calc total number of rows we should have in the end:
nrow(argos_dat_df) + nrow(gps_dat_df) 
movebank_event_dat_df <- plyr::rbind.fill(argos_dat_df, gps_dat_df) #crosschecked counts


## 4f) REASSIGN OBS MARKED AS SCOLOPACIDAE ####
# CHECK SPECIES NAMES:
movebank_event_dat_df %>%
  distinct(individual.taxon.canonical.name)
# A small subset of obs somehow did not receive species names and are listed as Scolopacidae

# CHECK THESE PROJECTS: 
movebank_event_dat_df %>%
  filter(individual.taxon.canonical.name == "Scolopacidae") # These happened to be AMWO

# REASSIGN TO AMWO:
movebank_event_dat_df <- movebank_event_dat_df %>%
  mutate(individual.taxon.canonical.name = case_when(individual.taxon.canonical.name == "Scolopacidae" ~ "Scolopax minor",
                                                     .default = individual.taxon.canonical.name))


# 5) DOWNLOAD REFERENCE/DEPLOYMENT DATA #################################################
## 5a) DOWNLOAD REFERENCE DATA ####

# CREATE EMPTY DATAFRAME:
ref_dat_list <- list() 

# LOOP TO GRAB REFERENCE DATA FOR ALL OF THE STUDY IDs ####
for (i in 1:length(IDs_all)){
  ref_dat <- getMovebankReferenceTable(study = IDs_all[i], 
                                 login = loginStored,
                                 allAttributes = TRUE) 
  ref_dat_list[[i]] <- ref_dat
}


# 6) FORMAT REFERENCE DATA #################################################
## 6a) CONVERT COLUMN NAMES FROM _ to .  ####
# Dots used in many move functions
# In the future will go back to snake_case with "_" when updating to move2 package
ref_dat_list <- ref_dat_list %>%
  map(~ .x %>% rename_all(function(x) gsub("_", ".", x)))

## 6b) RESASSIGN COLUMN CLASSES #### 
# Need to be the same class for merging
# CROSS CHECK COLUMN CLASS TYPES ACROSS DATASETS
column_check_ref <- compare_df_cols(ref_dat_list) 

# FIND COLUMNS THAT ARE NUMERIC:
numeric_col_names <- column_check_ref %>% 
  pivot_longer(cols = !column_name, names_to = "datlist", values_to = "dattype") %>% 
  filter(dattype %in% c("numeric", "integer")) %>%
  distinct(column_name) %>% 
  print(n = Inf) 
numeric_col_names %>% print(n = Inf)# Check which columns these are

# LIST COLUMNS THAT SHOULD BE KEPT AS CHARACTERS:
numeric_col_except <- c("animal.local.identifier","animal.ring.id", 
                        "deployment.local.identifier","tag.local.identifier",
 "tag.production.date","tag.serial.no")

# REMOVE CHARACTER COLUMNS:
numeric_col_names <- numeric_col_names %>% 
  filter(!column_name %in% numeric_col_except) %>%
  pull()

## 6c) REASSSIGN DATA TYPES ACROSS DATASETS ####
ref_dat_list <- ref_dat_list %>%
  map(~ .x %>% 
        mutate_at(numeric_col_names, as.numeric) %>% #updated from previous script
        mutate_at(numeric_col_except, as.character) %>% # a few exceptions that come up as numeric but should be read as character
        mutate_if(is.logical, as.character))
column_check_ref <- compare_df_cols(ref_dat_list) #check again
rm(column_check_ref_long, column_check_ref)

## 6e) TURN REF DATA INTO DATAFRAME ####
ref_dat_df <- map_dfr(ref_dat_list, ~as.data.frame(.x))

## 6f) GET SENSOR CODE NAMES ####
# getMovebankReferenceTable downloads refdata for all sensor types...
# But does not list sensor name, so download the sensor codes and names:
sensor_codes <- getMovebankSensors(login = loginStored)

# For merging with ref_dat_list need same name: sensor.type.id
sensor_codes <- sensor_codes %>% select(id , name) %>% 
  rename(sensor.type.id = id, sensor.type = name)  

## 6g) ADD IN SENSOR CODES: (for easier filtering) ####
ref_dat_df <- left_join(ref_dat_df, sensor_codes) #Joining, by = "sensor.type.id"

# QUICK CHECK OF SENSOR CODES:
# Which sensor codes do we have?:
ref_dat_df %>% select(sensor.type) %>% unique() 

## 6h) REASSIGN OBS MARKED AS SCOLOPACIDAE ####
# A small subset of obs somehow did not receive species names
# CHECK WHICH PROJECT THESE COME FROM:
ref_dat_df %>%
  filter(animal.taxon.canonical.name == "Scolopacidae") #AMWO

# REASSIGN TO AMWO:
ref_dat_df <- ref_dat_df %>%
  mutate(animal.taxon.canonical.name = case_when(animal.taxon.canonical.name == "Scolopacidae" ~ "Scolopax minor",
                                                     .default = animal.taxon.canonical.name))

## 6i) REMOVE REF DAT FOR TAGS WITHOUT DEPLOYMENTS ####
ref_dat_df <- ref_dat_df %>%
  filter(!is.na(deployment.id))


# 7) GET MOVEBANK STUDY INFORMATION ##############################################
# What proportion of studies/species/individuals have data set as open access in movebank?
## 7a) GET SUMMARY INFO FOR EACH MOVEBANK STUDY ####
study_dat_list <- list()

for (i in 1:length(IDs_all)){
  study_dat <- getMovebankStudy(study = IDs_all[i],
                                login = loginStored)
  study_dat_list[[i]] <- study_dat
}

## 7b) COLLAPSE LISTS TO DF ####
movebank_study_info_df <- study_dat_list %>%
  map_df(~.x %>% as.data.frame()) %>%
  distinct()

# License information:
## CC_0 : Public Domain
## CC_BY : Creative Commons Attribution (BY) : attribution required
## CC_BY_NC : non-commercial purposes with attribution
## ALL OTHER ARE "Custom" and aren't currently downloadable directly

## 7c) SAVE MOVEBANK STUDY INFO ####
write.csv(movebank_study_info_df, "./Data/MovebankDownload/01_movebank_study_info.csv")

## 7d) GET LICENSE INFO AND APPEND TO REF DATA ####
license_merge <- movebank_study_info_df %>%
  select(study_id, license_type) %>%
  rename(study.id = study_id, 
         license.type = license_type)

ref_dat_df <- left_join(ref_dat_df, license_merge) #Joining with `by = join_by(study.id)`

#################################################################################
# This leaves us with:
# 1) argos, gps, and argos&gps DETECTION DATA: movebank_event_dat (dataframe)
# 2) all reference data for deployments in DETECTION DATA: ref_dat_df (dataframe)
# NOTE: There is more than one line per deployment in the reference data... 
# This is a different line for each sensor type...
# Will clean in the next code

# 8) SAVE DOWNLOADED DATA ##########################################
saveRDS(movebank_event_dat_df, "./Data/MovebankDownload/01_argosgps_download_dat.rds")
saveRDS(ref_dat_df, "./Data/MovebankDownload/01_argosgps_download_refdat.rds")
