# CODE: 07_Flag_Prefilters.R
# CODE PURPOSE: CLEAN AND FLAG DATA PRIOR TO ANIMOTUM FILTER 
# Movebank data library for more info: http://vocab.nerc.ac.uk/collection/MVB/current/

# We downloaded all data reguardless of if data owners applied filters in movebank
# This allows us to apply consistent filtering to the data across datasets

# THE BELOW CODE DOES THE FOLLOWING: 
# Formats timestamps
# Fills in missing species names
# Does some general data formatting
# Flags data for prefiltering:
## Flags incomplete cases
## Flags no LC class code #Note: this is currently removing Cellular Locations for one gps dataset. Need to investigate cellular location constraints
## Flags obs failing lotek cyclic redundancy check (GPS data)
## Flags obs outside of tag active period (outside deployment on/off timestamps)
## Flags obs in the future (seems to happen with some GPS data)
## Flags contributor manually marked outliers
## Flags unrealistic lats and longs
## Flags "Z" locations
## Flags ctt specific errors
## Flags lat/lon at 0,0
## Flags complete duplicates
## Manually flag bad first locations
## Manually flag bad last location problems
## Manually flag other problems -- eg, species flying to midway atoll that doesn't go there
## Flag events requiring 100km obscuration (legal requirement in some locations)
## Flag tags at manufacturing facility
## Add column "any failed filters"


# 0) SET WORKING DIRECTORY ##############################
setwd()
Sys.setenv(TZ = 'UTC')


# 1) LOAD PACKAGES #################################################
library(move)
library(tidyverse)
library(conflicted)
library(janitor)
library(lubridate)
library(viridis)
library(sf)
library(rnaturalearth)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")

# CUSTOM FUNCTIONS:
source("./Code/Functions/named_group_split.r") 
source("./Code/Functions/manufacturer_dates_to_filter.R")


# 2) LOAD IN DATA ##################################################
## 2a) EVENT DATA ####
event_dat_all <- readRDS("./Data/Merged/04_all_event_dat.rds")

## 2b) REFERENCE DATA ####
ref_dat_all <- readRDS("./Data/Merged/04_all_refdat.rds")

## 2c) SPECIFY PROJECTION ####
# LAEA with custom meridian locations 
# Especially helpful for plotting birds that cross the international dateline
sc_crs <- "+proj=laea +lon_0=-101.6 +lat_0=19.22 +datum=WGS84 +units=m +no_defs" 

## 2d) LOAD LOCATIONS FOR TAG MANUFACTURERS ####
tag_man_loc_df <- read.csv("./Data/Spatial_Layers/Tag_Manufacturer_Locations/tag_manufacturer_locations.csv")

### CONVERT TAG MANUFACTURER LOCATIONS TO SF ####
tag_man_loc_sf <- tag_man_loc_df %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
  st_transform(crs = st_crs(sc_crs))
rm(tag_man_loc_df)


# 3) DATA CLEAN ####################################################
## FORMAT TIMESTAMPS ####
# GET THOSE WHERE EVENTS PARSE AFTER FORMATING TO YMDHMS:
event_dat_ts <- event_dat_all %>% 
  mutate(timestamp = lubridate::ymd_hms(timestamp, tz = "UTC")) %>%
  filter(!is.na(timestamp))

# FIX TIME STAMPS FOR THOSE WITHOUT HMS:
# to mid-date as least amount of error (12 hrs)
event_ts_fix <- event_dat_all %>%
  mutate(ts.hr = hour(timestamp),
         ts.min = minute(timestamp),
         ts.sec = second(timestamp)) %>%
  filter(ts.hr == 0, ts.min == 0, ts.sec == 0) %>%
  mutate(hms = "12:00:00") %>%
  unite(timestamp, c("timestamp", "hms"), sep = " ") %>%
  mutate(timestamp = lubridate::ymd_hms(timestamp, tz = "UTC")) %>%
  select(-ts.hr, -ts.min, -ts.sec)

# JOIN BACK TOGETHER:
event_dat_ts <- rbind(event_dat_ts, event_ts_fix) %>%
  arrange(sc.deployment.id, timestamp)
rm(event_ts_fix, event_dat_all)


## GENERAL DATA MANAGEMENT #############################################
### SPLIT BY SPECIES/STUDY ####
# Allows for easier looping through each species & dataset at a time
# Helps with organization
event_dat_lst <- event_dat_ts %>% 
  named_group_split(individual.taxon.canonical.name, sc.dataset.id) 

### ADD LC CODE TO "G" FOR GPS DATA ####
# Needed for aniMotum
event_dat_lst <- event_dat_lst %>% map(~.x %>% 
                     mutate(argos.lc = case_when(sensor.type == "GPS" ~ "G", .default = argos.lc)))

### ADD MONTH AND YEAR CHARACTERS TO THE DATA ####
event_dat_lst <- event_dat_lst %>% map(~.x %>%
  mutate(detection.year = as.character(lubridate::year(timestamp)),
         detection.month = as.character(lubridate::month(timestamp)),
         date = date(timestamp)))

### ADD NEW COLUMN orig.dep.seq.id ####
# An identifier of the sequential observation from download per individual
event_dat_lst <- event_dat_lst %>% 
                    map(~.x %>%
                     group_by(sc.deployment.id) %>%
                     arrange(sc.deployment.id, timestamp) %>%
                     mutate(orig.dep.seq.id = row_number()) %>%
                              ungroup())


# 4) FLAG DATA FOR PREFILTERING ############################################
# 0 = failed the filter; 1 = passed the filter

## FLAG INCOMPLETE CASE FILTER ####
# Flags obs without lat, long, and/or timestamp
event_dat_lst <- event_dat_lst %>% 
  map(~ .x %>%
        mutate(filter.incomplete.case = case_when(is.na(location.lat)|
                                                is.na(location.long)|
                                                is.na(timestamp)|
                                                is.na(location.lat) ~ 0, .default = 1))) 

## FLAG NO LC CLASS FILTER #### 
event_dat_lst <- event_dat_lst %>% 
  map(~ .x %>% 
        mutate(filter.no.lc.class = case_when(argos.lc == "" ~ 0,
                                              is.na(argos.lc) ~ 0, .default = 1)))

## FLAG OBS FAILING LOTEK CYCLIC REDUNDANCY CHECK (GPS DAT) ####
# For more info on CRC check Lotek CRC status: http://vocab.nerc.ac.uk/collection/MVB/current/
event_dat_lst <- event_dat_lst %>% 
  map(~ .x %>% 
        mutate(filter.GPS.crc.fail = case_when(lotek.crc.status.text == "Fail" ~ 0,
                                             lotek.crc.status.text == "" ~ 1,
                                             is.na(lotek.crc.status.text) ~ 1, 
                                             .default = 1)))

## FLAGS OBS OUTSIDE DEPLOYMENT START AND END DATES FILTER ####
# SPLIT INTO STUDY AND SPECIES LISTS:
refdat_lst <- ref_dat_all %>% 
  named_group_split(animal.taxon.canonical.name, sc.dataset.id) 

# CONVERT DEPLOY ON AND OFF TIMES TO POSIXCT: &
deploy_times_lst <- refdat_lst %>% 
  map(~.x %>%
        select(sensor.types.detected, sc.deployment.id, deploy.on.timestamp, deploy.off.timestamp) %>%
        mutate(deploy.on.timestamp = as.POSIXct(deploy.on.timestamp, format = "%Y-%m-%d %T", tz = "UTC"),
               deploy.off.timestamp = as.POSIXct(deploy.off.timestamp, format = "%Y-%m-%d %T", tz = "UTC")))

# COMBINE DEPLOY TIMES WITH LOCATION DATA:
event_dat_lst <- map2(event_dat_lst, deploy_times_lst, left_join) #should join by sc.deployment.id, sensor.types.detected 

# FIX DEPLOYMENT.IDs ON TIMESTAMPS PREVIOUSLY FOUND TO BE A PROBLEM:
# These were listed as having a year as "0017"
# Check ID's
fix_ids <- event_dat_lst %>% map_dfr(~.x %>% 
                                       filter(year(deploy.on.timestamp) == "17") %>%
                                       select(sc.deployment.id) %>% 
                                      distinct()) %>% 
                                       pull() 

# Subset and fix the deploy.on.times for select ids:
dat_fix <- event_dat_lst %>%
  map(~.x %>% filter(sc.deployment.id %in% fix_ids) %>%
    mutate(deploy.on.timestamp = 
                       gsub("0017", "2017", deploy.on.timestamp)))
# Subset and don't fix:
dat_good <-  event_dat_lst %>%
  map(~.x %>% filter(!(sc.deployment.id %in% fix_ids)))

# Map2 to add them back together:
event_dat_lst <- map2(dat_good, dat_fix, rbind)

# Cleanup:
rm(dat_fix)
rm(dat_good)
rm(fix_ids)

# FLAG DETECTIONS OUTSIDE OF DEPLOYMENT TIMES:
event_dat_lst <- event_dat_lst %>% map( ~.x %>%
                                          mutate(filter.detect.before.deployment = case_when(timestamp < deploy.on.timestamp ~ 0, .default = 1),
                                                 deployment.on.time.available = case_when(is.na(deploy.on.timestamp) ~ "no", .default = "yes"),
                                                 filter.detect.after.shutoff = case_when(timestamp > deploy.off.timestamp ~ 0 , .default = 1),
                                                 deployment.off.time.available = case_when(is.na(deploy.off.timestamp) ~ "no", .default = "yes"))) #for those where there was no deploy off time
rm(deploy_times_lst)

## FLAG DETECTIONS IN THE FUTURE ####
# A problem for some GPS data, so added it in as a check
# Some detections were showing up in the future and some way before true deployment
event_dat_lst <- event_dat_lst %>% 
  map(~ .x %>%
        mutate(filter.future.detection = case_when(timestamp > lubridate::now() ~ 0, .default = 1)))

## FLAG CONTRIBUTOR MANUALLY MARKED OUTLIERS ####
# EXPLORE MANUALLY MARKED OUTLIERS:
# Data owners have gone out of their way to identify these as incorrect
event_dat_lst %>% map(~.x %>% group_by(study.id, manually.marked.outlier) %>%
                        count()) 
# FLAG THEM:
event_dat_lst <- event_dat_lst %>% 
  map(~ .x %>% 
        mutate(filter.manually.marked.outlier = case_when(manually.marked.outlier == "true" ~ 0, .default = 1)))

## FLAG UNREALISTIC LAT AND LONGS ####
# LATS:
event_dat_lst <- event_dat_lst %>% 
  map(~ .x %>% mutate(filter.bad.lat = case_when((location.lat > 90|location.lat < -90) ~ 0, .default = 1)))

# LONGS:
event_dat_lst <- event_dat_lst %>% 
  map(~ .x %>% mutate(filter.bad.lon = case_when((location.long > 180|location.long < -180) ~ 0 , .default = 1)))

## FLAG Z DETECTIONS ####
event_dat_lst <- event_dat_lst %>% 
  map(~ .x %>% 
        mutate(filter.argos.z = case_when(argos.lc == "Z" ~ 0 , .default = 1))) 

## FLAG CTT ERRORS ####
event_dat_lst <- event_dat_lst %>% 
  map(~ .x %>% 
        mutate(filter.ctt.error = case_when(ctt.error.code != 0 ~ 0 , .default = 1))) 

## FLAG LAT/LONG at 0,0 ####
event_dat_lst <- event_dat_lst %>% 
  map(~ .x %>% 
        mutate(filter.ll.00 = case_when(round(location.lat, digits = 0) == 0 &
                                          round(location.long, digits = 0) == 0 ~ 0 , .default = 1))) 

## FLAG COMPLETE DUPLICATES ####
# GET COMPLETE DUPLICATES:
complete_dup_ts_check <- event_dat_lst %>%
  map_df(~.x %>% 
           janitor::get_dupes(., -c(event.id, orig.dep.seq.id, visible, algorithm.marked.outlier, 
                                           argos.lat2, argos.lon2, argos.valid.location.algorithm,
                                           lotek.crc.status), -(contains("filter"))))

# ASSIGN IDS TO COMPLETE DUPLICATES:
complete_dup_ts_check_ids <- complete_dup_ts_check %>%
  distinct() %>%
  mutate(dup.id = row_number())

# JOIN TOGETHER:
complete_dup_ts_check <- left_join(complete_dup_ts_check, complete_dup_ts_check_ids)
rm(complete_dup_ts_check_ids)

# DROP FIRST OBSERVATION TO CREATE FILTER:
complete_dup_ts_check <- complete_dup_ts_check %>%
  select(-dupe_count) %>%
  group_by(dup.id) %>%
  slice(2:n()) %>%
  ungroup() %>%
  mutate(filter.complete.duplicate = 0)

# JOIN DATA BACK WITH ORIGINAL:
event_dat_lst <- event_dat_lst %>%
  map(~left_join(.x, complete_dup_ts_check) %>%
  mutate(filter.complete.duplicate = 
                       case_when(is.na(filter.complete.duplicate) ~ 1, 
                                       .default = filter.complete.duplicate)))
rm(complete_dup_ts_check)

## MANUALLY FLAG FIRST LOCATION DETECTION PROBLEMS ####
# First and last locations that are errors are difficult to remove because they ...
# Are not easily filtered out with a speed, angle, distance filter, etc.
# An alternative to flagging here would be to manually adjust deployment start and end times and filter above

# IDENTIFY WHICH OBS HAVE ALREADY FAILED FILTERS:
event_dat_lst <- event_dat_lst %>%
  map(~.x %>% mutate(any.failed.sc.filters = if_any(starts_with('filter.'), ~ . == 0)))

# CREATE FILTER FOR MANUALLY ID'D OUTLIERS:
# EVENT IDs or dates determined by inspecting maps
event_dat_lst <- event_dat_lst %>% 
  map(~ .x %>% mutate(filter.sc.first.detect.outlier = case_when(event.id %in% 
          c("movebankeventid1", 
            "movebankeventid2") ~ 0, 
          sc.deployment.id == "sddepid1" &
            timestamp == lubridate::ymd_hms("2016-05-30 23:20:29") ~ 0,
          sc.deployment.id == "scdepid2" &
            timestamp == lubridate::ymd_hms("2019-07-14 18:38:08") ~ 0, .default = 1)))

## SC MANUALLY FLAG LAST LOCATION DETECTION PROBLEMS ####
### FLAG THE OBS:
event_dat_lst <- event_dat_lst %>% 
  map(~ .x %>% mutate(filter.sc.last.detect.outlier = 
                        case_when(event.id == "movebankeventid600" ~ 
                                                                  0, .default = 1)))

## SC MANUALLY FLAG OTHER OBSERVATIONS ####
# Some errors persist and must be manually flagged for removal:
# e.g., tag removed from one bird and placed on another in an area outside the species distribution
# ... in otherwords a deployment on/off was missed and data assigned to wrong species
# Some stray points persist despite filtering options (e.g. spikes with two point locations)

event_dat_lst <- event_dat_lst %>% 
  map(~ .x %>% mutate(filter.sc.outlier = 
                        case_when(deployment.id == "movebankdepid1" & 
                                    timestamp > lubridate::ymd_hms("2022-06-02 15:02:24") ~  0,
                                  deployment.id == "movebankdepid2" & event.id == "movebankeventid1" ~ 0, 
                                  sc.deployment.id == "scdepid1" & 
                                    timestamp <= lubridate::ymd_hms("2021-06-30 13:00:48") ~ 0,
                                  .default = 1)))

## FLAG EVENTS REQUIRING 100km OBSCURATION ####
# Currently for several projects, a portion of the data are required to have locations obscured to 100 km
# This is a legal requirement in some states
# Assigned based on data contributor name
event_dat_lst <- event_dat_lst %>% map(~.x %>%
                     mutate(obscuration.100km.yn = case_when(grepl("contributorname1", .x$sc.dataset.id) ~ "yes", 
                                                             grepl("contributorname2", .x$sc.dataset.id) ~ "yes", .default = "no")))

## FLAG OBS NEAR MANUFACTURING FACILITY ####
# FIRST: HIDE POINTS THAT HAVE ALREADY FAILED FILTERS:
event_dat_lst <- event_dat_lst %>%
  map(~.x %>% mutate(any.failed.sc.filters = 
                       if_any(starts_with('filter.'), ~ . == 0)))

# GET LOCATION FOR MICROWAVE TELEMETRY:
mt_sf <- tag_man_loc_sf %>% 
  filter(tag.manufacturer.name.location == "Microwave Telemetry Columbia")

### GET DATES TO FILTER FOR THAT LOCATION #### 
mt_dates_filter <- manuf_dates_to_filter(point_to_buffer = mt_sf, 
                                         event_data_list = event_dat_lst, 
                                         project_crs = sc_crs)

### ADD FLAGGED FILTER TO EACH LIST ####
event_dat_lst <- event_dat_lst %>% 
  map(~left_join(.x, mt_dates_filter)) #Joining with `by = join_by(sc.deployment.id, date)`

### FILL ANY NA's WITH 1s ####
# These are points that didn't fail the filter
event_dat_lst <- event_dat_lst %>% 
  map(~.x %>% mutate(filter.manufacturer.dates = ifelse(is.na(filter.manufacturer.dates), 1, 
                                                        filter.manufacturer.dates)))

## FLAG OBS AT TEST SITES ####
# Locations where known tag testing occurred
# Note, this currently could flag other real points that fly through these areas or stop there

### CREATE DF OF SITES ####
test_sites <- data.frame(site = c("site1", "site2", "site3"),
                         species.code = c("speciescode1", "speciescode2", "speciescode3"),
                          lon = c(-118.1234, -117.1234, -72.5678),
                          lat = c(43.12345, 43.12345, 42.5678))

### CONVERT TO SF AND BUFFER ####
test_sites_sf <- test_sites %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
  st_transform(crs = st_crs(sc_crs)) %>%
  st_buffer(5000) 

### GET POINTS INTERSECTING TEST SITES ####
event_sf <- event_dat_lst %>%
  map_df(~.x %>% as.data.frame() %>%
        filter(filter.incomplete.case == 1) %>% #points with lat and long   
        st_as_sf(coords = c("location.long","location.lat"), crs = 4326) %>%
        st_transform(crs = st_crs(sc_crs)) %>%
        select(orig.dep.seq.id, sc.deployment.id, timestamp, species.code))

in_test_sites_sf <- event_sf %>% 
  st_filter(test_sites_sf, .predicates = st_intersects) %>%
  mutate(filter.test.sites = 0) 

in_test_sites <- in_test_sites_sf %>% 
  st_drop_geometry() %>%
  filter(species.code %in% c("speciescode1", "speciescode2")) #keep only species known to be tested there
rm(event_sf)

### JOIN WITH EVENT DATA ####
event_dat_lst <- event_dat_lst %>% 
  map(~left_join(.x, in_test_sites) %>%
        mutate(filter.test.sites = case_when(is.na(filter.test.sites) ~ 1, .default = filter.test.sites)))
rm(in_test_sites, in_test_sites_sf)


# 5) UPDATE COLUMN TO ANY FAILED FILTERS #############################
event_dat_lst <- event_dat_lst %>%
  map(~.x %>% mutate(any.failed.sc.filters = if_any(starts_with('filter.'), ~ . == 0)))


# 6) SAVE DATA #######################################################################
## 6a) SAVE FLAGGED DETECTION DATA ####
# Data are not filtered but problems are flagged for removal
saveRDS(event_dat_lst, "./Data/Cleaned/07_event_dat_FLAGGED.rds")

## 6b) SAVE CLEAN REF DATA ####
saveRDS(refdat_lst, "./Data/Cleaned/07_refdat_CLEAN.rds")
