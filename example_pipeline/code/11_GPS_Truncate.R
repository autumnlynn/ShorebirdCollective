# CODE: 12_GPS_Truncate.R
# CODE PURPOSE: TRUNCATE OFF GPS TAGS FOR LIKELY DROPS
# These tags likely fell off and are still transmitting
# These truncation steps could be improved by examining sensor data (especially activity counter and mortality)
# Alternatively could determine revised end dates and alter deployment end times then filter
# For now, it's a broad approach and fairly clunky code


# 0) SET WORKING DIRECTORY ##############################
setwd()
Sys.setenv(TZ = 'UTC')


# 1) LOAD PACKAGES #################################################
library(tidyverse)
library(sf)
sf_use_s2(FALSE)

source("./Code/Functions/named_group_split.r")


# 2) LOAD GPS DATA ###################################
gps_dat_df <- readRDS("./Data/Cleaned/10_event_dat_FLAGGED_DC_DEPS_GPS.rds")


# 3) CLEANING ##############################
## 3a) SPLIT BY SP & PROJECT FOR QUICK PLOTTING ####
dat_gps_list <- gps_dat_df %>% 
  named_group_split(sc.dataset.id) 
dat_gps_list <- dat_gps_list %>%
  map(~.x %>% as.data.frame()) 

## 3b) CALCULATE LARGE GAPS ####
dat_gps_lst_gaps <- dat_gps_list %>%
  map(~.x %>% filter(any.failed.sc.filters != "TRUE" | #for dat that's visible (note this also removes Z detections)
                       sc.location.origin %in% c("deploy_on_lat_long", "deploy_off_lat_long")) %>% # for lat and long deployment locations
        arrange(sc.deployment.id, timestamp) %>%
        group_by(sc.deployment.id) %>%
        mutate(elapsed.days = round(as.numeric(difftime(timestamp, lag(timestamp, default = first(timestamp)), units = "days")), 0),
               elapsed.days.fact = as.factor(elapsed.days),
               gap.marker = ifelse(elapsed.days > 7 & elapsed.days < 15, "short_gap", # assign to gap size
                                   ifelse(elapsed.days >= 15 & elapsed.days < 30, "medium_gap", 
                                          ifelse(elapsed.days >= 30, "long_gap", NA)))) %>%
        ungroup())


# PLOT THESE WITH A SEPARATE SCRIPT ####################################
# 11_GPS_PlotDetectionGaps.R #Plot by lat over time or distance traveled over time
# No projects with consistent gap problems
# Instead, focus on single, specific problems


# 4) AGGREGATE IDS FOR TRUNCATION #############################################
## DROPPED TAGS ####
# Remove all points
depIDs_1 <-  c(movebankdepid38, movebankdepid90, movebankdepid67)

## REMOVE AFTER AUGUST FIRST 2019 ####
depIDs_2 <-  c(movebankdepid17, movebankdepid38, movebankdepid40)

## REMOVE AFTER AUGUST FIRST 2018 ####
depIDs_3 <-  movebankdepid71


# 5) FLAG THESE OBS ##############################################################
dat_gps_lst_gaps  <- dat_gps_lst_gaps  %>%
  map(~.x %>% mutate(filter.sc.manual.truncation.tail = 
                        case_when(deployment.id %in% depIDs_1 ~ 0, # 1) remove all for dropped tag
                                  deployment.id %in% depIDs_2 & 
                                    date(timestamp) >= lubridate::ymd("2019-08-01") & 
                                    timestamp < lubridate::now() ~ 0, # 2)
                                  deployment.id %in% depIDs_3 & 
                                    date(timestamp) >= lubridate::ymd("2018-08-01") & 
                                    timestamp < lubridate::now() ~ 0, # 3)
                                  .default = NA)))


# 6) ADD OBS THAT PREVIOUSLY FAILED A FILTER BACK IN TO DATASET #############################
## 6a) GET OBS THAT PREVIOUSLY FAILED A FILTER ####
# To keep all downloaded points aggregated with flags
dat_gps_failed_lst <- dat_gps_list %>%
  map(~.x %>% filter(any.failed.sc.filters == "TRUE") %>% 
        group_by(sc.deployment.id) %>%
        arrange(timestamp) %>%
        mutate(elapsed.days = NA, 
               elapsed.days.fact = NA,
               gap.marker = NA,
               twoweek.trunc.gaps = NA,
               filter.detection.after.2wk.gap = NA,
               filter.sc.manual.truncation.tail = NA,
               filter.sc.manual.truncation.head = NA) %>%
        ungroup())

## 6b) ADD MISSING COLUMNS TO FILTERED GPS DATA ####
# To match Argos data
dat_gps_lst_gaps <- dat_gps_lst_gaps %>%
  map(~.x %>% mutate(twoweek.trunc.gaps = NA,
                     filter.detection.after.2wk.gap = NA,
                     filter.sc.manual.truncation.head = NA))

## 6c) COMBINE COMPLETE FLAGGED DATA AND THOSE FROM 6A ####
dat_gps_lst_updated <- map2(dat_gps_lst_gaps, dat_gps_failed_lst, rbind) %>%
  map(~.x %>% arrange(sc.deployment.id, timestamp)) # arrange in proper order

## 6d) UPDATE FILTER COLUMNS TO BE 1... ####
# If they aren't zero (0's are failed filters, 1's pass)
dat_gps_lst_updated <- dat_gps_lst_updated %>%
  map(~.x %>% mutate(filter.detection.after.2wk.gap = case_when(is.na(filter.detection.after.2wk.gap) ~ 1, 
                                                                .default = filter.detection.after.2wk.gap),
                     filter.sc.manual.truncation.tail = case_when(is.na(filter.sc.manual.truncation.tail) ~ 1, 
                                                                  .default = filter.sc.manual.truncation.tail),
                     filter.sc.manual.truncation.head = case_when(is.na(filter.sc.manual.truncation.head) ~ 1, 
                                                                  .default = filter.sc.manual.truncation.head)))
## 6e) UPDATE THE ANY FAILED FILTERS COLUMN ####
dat_gps_lst_updated <- dat_gps_lst_updated %>%
  map(~.x %>% mutate(any.failed.sc.filters = if_any(starts_with('filter.'), ~ . == 0)))


# 7) FLAG SHORT TRACKS #####################################################
# Short tracks not suitable for animotum caused by super short tracks ...
# E.G., Low time elapsed between first and last detection 
# Flag any less than or equal to 7 days between

## 7a) CALCULATE TRACK DURATION (MINUTES) ####
track_length_df <- dat_gps_lst_updated %>% map_dfr(~.x %>% 
                                                       filter(any.failed.sc.filters != "TRUE" | 
                                                                sc.location.origin %in% c("deploy_on_lat_long", "deploy_off_lat_long")) %>%
                                                       group_by(sc.deployment.id) %>%
                                                       arrange(timestamp) %>% 
                                                       filter(row_number() == 1 | row_number() == n()) %>% # get first and last obs per individual
                                                       mutate(seq.id = row_number(),
                                                              first.last.elapsed.mins = 
                                                                round(as.numeric(difftime(timestamp, lag(timestamp, default = first(timestamp)), units = "mins")), 0)) %>%
                                                       select(sc.deployment.id, seq.id, first.last.elapsed.mins) %>% 
                                                       arrange(sc.deployment.id, seq.id) %>% 
                                                       filter(seq.id == 2), .id = "species.scDatasetID") #species.movebankID

## 7b) GRAB IDs FOR TRACK LENGTH LESS THAN OR EQUAL TO 7 DAYS ####
shortIDs <- track_length_df %>% filter(first.last.elapsed.mins <= 10080) %>% # 7 days in minutes
  select(sc.deployment.id) %>% pull()

## 7c) FLAG SHORT TRACKS WITH FILTER ####
dat_gps_lst_updated <- dat_gps_lst_updated %>% 
  map(~.x %>% 
        mutate(filter.track.duration.less8days = 
                 case_when(sc.deployment.id %in% shortIDs ~ 0, .default = 1)))

rm(shortIDs)
rm(track_length_df)

## 7d) UPDATE THE ANY FAILED FILTERS COLUMN ####
dat_gps_lst_updated <- dat_gps_lst_updated %>%
  map(~.x %>% mutate(any.failed.sc.filters = if_any(starts_with('filter.'), ~ . == 0)))


# 8) FILTER LOW NUMBER OF VISIBLE DETECTIONS PER DEP ################################
## 8a) COUNT NUMBER OF VISIBLE DETECTIONS ####
dep_visible_counts_lst <- dat_gps_lst_updated %>% map(~ .x %>% 
                                                          filter(any.failed.sc.filters != "TRUE" | 
                                                                   sc.location.origin %in% c("deploy_on_lat_long", "deploy_off_lat_long")) %>%
                                                          group_by(sc.deployment.id) %>% 
                                                          select(sc.deployment.id, event.id) %>%
                                                          count() %>% rename(n.visible.detections = n) %>% 
                                                          as.data.frame())
# HOW MANY JUST SINGLE OBS?
dep_visible_counts_lst %>% map_dfr(~.x %>% filter(n.visible.detections <= 1))

## 8b) GET IDs OF THOSE FOR FLAGGING ####
short_count_IDs <- dat_gps_lst_updated %>% map_dfr(~ .x %>% 
                                                       filter(any.failed.sc.filters != "TRUE" | 
                                                                sc.location.origin %in% c("deploy_on_lat_long", "deploy_off_lat_long")) %>%
                                                       group_by(sc.deployment.id) %>% 
                                                       select(sc.deployment.id, event.id) %>%
                                                       count() %>% rename(n.visible.detections = n)) %>% 
  filter(n.visible.detections <= 1) %>%
  select(sc.deployment.id) %>% pull()
short_count_IDs

## 8c) FLAG SHORT COUNTS FOR FILTERING ####
dat_gps_lst_updated <- dat_gps_lst_updated %>%
  map(~.x %>% 
        mutate(filter.visible.obs.less1 = 
                 case_when(sc.deployment.id %in% short_count_IDs ~ 0 , .default = 1)))

## 8d) UPDATE THE ANY FAILED FILTERS COLUMN ####
dat_gps_lst_updated <- dat_gps_lst_updated %>%
  map(~.x %>% mutate(any.failed.sc.filters = if_any(starts_with('filter.'), ~ . == 0)))

## 8e) ADD # VISIBLE DETECTIONS AS COLUMN ####
dat_gps_lst_updated <- map2(dat_gps_lst_updated, dep_visible_counts_lst, ~left_join(.x, .y)) #Joining with `by = join_by(sc.deployment.id)`


# 9) FLAG STATIONARY TAGS ############################################
# Tag lat and long never changed (found by breaking animotum)

## 9a) CALCULATE DISTINCT LOCATIONS PER DEPLOYMENT ####
distinct_locs_lst <- dat_gps_lst_updated %>% 
  map(~.x %>%
        filter(any.failed.sc.filters != "TRUE" | 
                 sc.location.origin %in% c("deploy_on_lat_long", "deploy_off_lat_long")) %>%
        group_by(sc.deployment.id,
                 location.lat, location.long) %>% count() %>%
        select(-n) %>% ungroup() %>%
        group_by(sc.deployment.id) %>%
        count() %>% 
        rename(n.unique.latlong = n))

## 9b) WHICH DEP IDS HAVE SINGLE LOCATION ####
stationary_IDs <- distinct_locs_lst %>% map_dfr(~.x %>% filter(n.unique.latlong < 2) %>%
                                                  select(sc.deployment.id)) %>% pull() # 11 BIRDS

## 9c) FLAG THESE IN ANOTHER FILTER ####
dat_gps_lst_updated <- dat_gps_lst_updated %>% 
  map(~.x %>% mutate(filter.stationary.tag = 
                       case_when(sc.deployment.id %in% stationary_IDs ~ 0 , .default = 1)))

## 9d) UPDATE THE ANY FAILED FILTERS COLUMN ####
dat_gps_lst_updated <- dat_gps_lst_updated %>%
  map(~.x %>% mutate(any.failed.sc.filters = if_any(starts_with('filter.'), ~ . == 0)))


# 10) SAVE DATA ################################################
saveRDS(dat_gps_lst_updated, "./Data/Cleaned/11_event_dat_FLAGGED_DC_DEPS_GPS_TRUNC.rds")
