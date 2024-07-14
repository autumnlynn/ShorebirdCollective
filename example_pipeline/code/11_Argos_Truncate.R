# CODE: 12_Argos_Truncate.R
# CODE PURPOSE: FLAG ARGOS DETECTIONS AFTER LARGE GAPS
# These tags likely fell off and are still transmitting or the bird died
# Flags them for removal
# Do this prior to running aniMotum so that data aren't just estimating over and over for years of dropped tag life
# These truncation steps could be improved by examining sensor data (especially activity counter and mortality)
# Alternatively could determine revised end dates and alter deployment end times then filter
# For now, it's a broad approach and fairly clunky code

# 0) SET WORKING DIRECTORY ##############################
setwd()
Sys.setenv(TZ = 'UTC')

# LOAD CUSTOM FUNCTIONS 
source("./Code/Functions/named_group_split.r")


# 1) LOAD PACKAGES #################################################
library(tidyverse)


# 2) LOAD ARGOS & ARGOS/GPS DATA ###################################
argos_dat_df <- readRDS("./Data/Cleaned/10_event_dat_FLAGGED_DC_DEPS_ARGOS.rds")


# 3) CLEANING ##############################
## 3a) RE-LEVEL LC CLASS CODES ####
ord_lc_class <- c("G", "3", "2", "1", "0", "A", "B", "Z") # the real order
argos_dat_df <- argos_dat_df %>% 
        mutate(argos.lc = factor(argos.lc, 
                                 levels = ord_lc_class))

## 3b) SPLIT BY PROJECT FOR QUICK PLOTTING ####
dat_argos_lst <- argos_dat_df %>% 
  named_group_split(sc.dataset.id) 
dat_argos_lst <- dat_argos_lst %>% #make sure they are all dataframes
  map(~.x %>% as.data.frame()) 

## 3c) CALCULATE LARGE GAPS ####
dat_argos_lst_gaps <- dat_argos_lst %>%
  map(~.x %>% filter(any.failed.sc.filters != "TRUE" | #for dat that's visible (note this also removes Z detections)
                       sc.location.origin %in% c("deploy_on_lat_long", "deploy_off_lat_long")) %>% # for lat and long deployment locations
        arrange(sc.deployment.id, timestamp) %>%
        group_by(sc.deployment.id) %>%
        mutate(elapsed.days = round(as.numeric(difftime(timestamp, lag(timestamp, default = first(timestamp)), units = "days")), 0),
               elapsed.days.fact = as.factor(elapsed.days),
               gap.marker = ifelse(elapsed.days > 7 & elapsed.days < 15, "short_gap", # assign to gap size
                                   ifelse(elapsed.days >= 15 & elapsed.days <= 30, "medium_gap", 
                                          ifelse(elapsed.days > 30, "long_gap", NA)))) %>%
        ungroup())


# PLOT THESE WITH SEPARATE SCRIPT ####################################
# 11_Argos_PlotDetectionGaps.R #Plot by lat over time & LC code 
# From here, identify datasets where consistent removal of points after certain gaps would be beneficial

# PROJECTS TO TRUNCATE:
# 1) GAPS GREATER THAN 15 DAYS: ALL BIRDS
  # a) scdatasetid5
  # b) scdatasetid6
  # c) scdatasetid7

# 2) GAPS GREATER THAN 15 DAYS: WITH A COUPLE OF EXCEPTIONS:
   # i) scdataset3 ; EXCEPT dep id == movebankdeployid1
   # ii) scdataset4; EXCEPT ids == "movebankdeployid3", "movebankdeployid5"


# 4) REMOVE FLAG FOR LARGE GAPS BETWEEN TAGGING LOCATION AND FIRST DETECTION #########################
# We don't want to truncate those
dat_argos_lst_gaps <- dat_argos_lst_gaps %>%
  map(~.x %>% group_by(sc.deployment.id) %>%
        arrange(timestamp) %>%
        mutate(gap.row.number = row_number(),
               twoweek.trunc.gaps = case_when(lag(sc.location.origin == "deploy_on_lat_long") & # row after deploy on location
                         gap.marker %in% c("medium_gap","long_gap") ~ NA, .default = gap.marker)) %>%
        ungroup() %>%
        select(-gap.row.number))


# 5) AGGREGATE IDs to TRUNCATE ##############################################
# From above notes:
## 5a) GROUP 1: TRUNCATE GAPS AFTER 14 days ####
dep_ids_1 <- argos_dat_df %>% 
            filter(sc.dataset.id %in% c("scdatasetid5",
                                        "scdatasetid6", 
                                        "scdatasetid7")) %>%
        select(sc.deployment.id) %>% 
        distinct() %>% 
        pull()

## 5b) GROUP 2: GAPS GREATER THAN 14 DAYS: WITH A COUPLE OF EXCEPTIONS ####
dep_ids_2i <- argos_dat_df %>% 
        filter(sc.dataset.id == "scdataset3" & 
                 deployment.id != "movebankdeployid1") %>%
  select(sc.deployment.id) %>% 
  distinct() %>% 
  pull()

###
dep_ids_2ii <- argos_dat_df %>%
        filter(sc.dataset.id == "scdataset3" & 
                !deployment.id %in% c("movebankdeployid3", "movebankdeployid5")) %>%
  select(sc.deployment.id) %>% 
  distinct() %>% 
  pull()
###

## 5c) COMBINE IDs TO TRUNCATE AROUND THE MEDIUM GAP (AFTER 2 WEEKS) ####
dep_ids <- c(dep_ids_1, dep_ids_2i, dep_ids_2ii)
rm(dep_ids_1)
rm(dep_ids_2i)
rm(dep_ids_2ii)


# 6) FLAG DEPS WITH 2 WEEK OR GREATER GAP TO TRUNCATE #######################################
# Use the twoweek.trunc.gaps marker column
# For these birds, truncate any obs after a 14 day gap
dat_argos_lst_gaps <- dat_argos_lst_gaps %>%
  map(~.x %>% 
        group_by(deployment.id) %>%
        arrange(timestamp) %>%
        mutate(filter.detection.after.2wk.gap = case_when(twoweek.trunc.gaps %in% c("medium_gap","long_gap") & 
                                                    sc.deployment.id %in% dep_ids ~ 0, .default = NA)) %>%
        fill(filter.detection.after.2wk.gap, .direction = "down") %>% 
        ungroup())  # fills down any after the medium gaps or larger (2 weeks or later) with "detected after gap longer than 2 weeks"


# 7) MANUALLY FLAG OBS -- SPECIAL TRUNCATION RULES #############################################
# a) Previously ID'd last scripts: scdataset8 ; deployment.id == movebankdepid21 # latitude not changing over time
# b) Previously ID'd last scripts: scdataset11 ; deployment.id == "movebankdepid35" # latitude not changing over time
# c) TRUNCATE AFTER LONG GAP

## 7a) scdataset8 ; dep id = movebankdepid21 ####
dat_argos_lst_gaps <- dat_argos_lst_gaps %>%
  map( ~.x %>% mutate(filter.sc.manual.truncation.tail = 
                        case_when(deployment.id == "movebankdepid21" &
                                    timestamp > lubridate::ymd_hms("2015-08-20 17:36:38") & 
                                    timestamp < lubridate::now() ~ 0, .default = NA)))

## 7b) scdataset11 ; deployment.id == movebankdepid35 ####
dat_argos_lst_gaps <- dat_argos_lst_gaps %>%
  map( ~.x %>% mutate(filter.sc.manual.truncation.tail = 
                        case_when(deployment.id == "movebankdepid35" & 
                                 timestamp >= lubridate::ymd_hms("2022-04-01 00:00:00") & 
                                 timestamp < lubridate::now() ~ 0, 
                                 .default = filter.sc.manual.truncation.tail)))

## 7C) scdataset15 ####
# ALL EXCEPT 1 BIRD AFTER LONG GAP
dat_argos_lst_gaps <- dat_argos_lst_gaps %>%
    map(~.x %>%
      group_by(sc.deployment.id) %>%
      arrange(timestamp) %>%
      mutate(filter.detection.after.4wk.gap = 
                   case_when(gap.marker == "long_gap" & 
                           tag.local.identifier != "bird250" & #one tag exception where this is true
                           sc.dataset.id == "scdataset15" ~ 0,.default = NA)),
    fill(filter.detection.after.4wk.gap, .direction = "down") %>%
      ungroup())


# 8 ADD OBS THAT PREVIOUSLY FAILED A FILTER BACK IN TO DATASET #############################
## 8a) GET OBS THAT PREVIOUSLY FAILED A FILTER ####
# To keep all downloaded points aggregated with flags
dat_argos_failed_lst <- dat_argos_lst %>%
  map(~.x %>% filter(any.failed.sc.filters == "TRUE") %>% 
      group_by(sc.deployment.id) %>%
        arrange(timestamp) %>%
        mutate(elapsed.days = NA, 
               elapsed.days.fact = NA,
               gap.marker = NA,
               twoweek.trunc.gaps = NA,
               filter.detection.after.2wk.gap = NA,
               filter.sc.manual.truncation.tail = NA,
               filter.sc.manual.truncation.head = NA,
               filter.detection.after.4wk.gap = NA,
               filter.detection.after.1wk.gap = NA) %>%
        ungroup())

## 8b) COMBINE COMPLETE FLAGGED DATA AND THOSE FROM 7A ####
dat_argos_lst_updated <- map2(dat_argos_lst_gaps, dat_argos_failed_lst, rbind) %>%
  map(~.x %>% arrange(sc.deployment.id, timestamp)) # arrange in proper order

## 8c) UPDATE FILTER COLUMNS TO BE 1... ####
# If they aren't zero (0's are failed filters, 1's pass)
dat_argos_lst_updated <- dat_argos_lst_updated %>%
  map(~.x %>% mutate(filter.detection.after.2wk.gap = case_when(is.na(filter.detection.after.2wk.gap) ~ 1, 
                                                                .default = filter.detection.after.2wk.gap),
                     filter.sc.manual.truncation.tail = case_when(is.na(filter.sc.manual.truncation.tail) ~ 1, 
                                                                .default = filter.sc.manual.truncation.tail),
                     filter.sc.manual.truncation.head = case_when(is.na(filter.sc.manual.truncation.head) ~ 1, 
                                                                .default = filter.sc.manual.truncation.head),
                     filter.detection.after.4wk.gap = case_when(is.na(filter.detection.after.4wk.gap) ~ 1, 
                                                                .default = filter.detection.after.4wk.gap),
                     filter.detection.after.1wk.gap = case_when(is.na(filter.detection.after.1wk.gap) ~ 1, 
                                                                .default = filter.detection.after.1wk.gap)))

# CLEAN UP:
rm(dat_argos_lst_gaps)
rm(dat_argos_failed_lst)

## 8d) UPDATE THE ANY FAILED FILTERS COLUMN ####
dat_argos_lst_updated <- dat_argos_lst_updated %>%
  map(~.x %>% mutate(any.failed.sc.filters = if_any(starts_with('filter.'), ~ . == 0)))


# 9) FLAG SHORT TRACKS FOR REMOVAL ###################################
# Short tracks not suitable for animotum caused by super short tracks ...
# E.G., Low time elapsed between first and last detection 
# Flag any less than or equal to 7 days between 

## 9a) CALCULATE TRACK DURATION (MINUTES) ####
track_length_df <- dat_argos_lst_updated %>% map_dfr(~.x %>% 
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

## 9b) GRAB IDs FOR TRACK LENGTH LESS THAN OR EQUAL TO 7 DAYS ####
shortIDs <- track_length_df %>% filter(first.last.elapsed.mins <= 10080) %>% # 7 days in minutes
                             select(sc.deployment.id) %>% pull()

## 9c) FLAG SHORT TRACKS WITH FILTER ####
dat_argos_lst_updated <- dat_argos_lst_updated %>% 
  map(~.x %>% 
        mutate(filter.track.duration.less8days = 
                 case_when(sc.deployment.id %in% shortIDs ~ 0, .default = 1)))

rm(shortIDs)
rm(track_length_df)

## 9d) UPDATE THE ANY FAILED FILTERS COLUMN ####
dat_argos_lst_updated <- dat_argos_lst_updated %>%
  map(~.x %>% mutate(any.failed.sc.filters = if_any(starts_with('filter.'), ~ . == 0)))


# 10) FILTER LOW NUMBER OF VISIBLE DETECTIONS PER DEP ################################
## 10a) COUNT NUMBER OF VISITIBLE DETECTIONS ####
# Can't run short tracks through aniMotum
dep_visible_counts_lst <- dat_argos_lst_updated %>% map(~ .x %>% 
                                   filter(any.failed.sc.filters != "TRUE" | 
                                            sc.location.origin %in% c("deploy_on_lat_long", "deploy_off_lat_long")) %>%
                                   group_by(sc.deployment.id) %>% 
                                   select(sc.deployment.id, event.id) %>%
                                   count() %>% rename(n.visible.detections = n) %>% 
                                   as.data.frame())

## 10b) GET IDs OF THOSE FOR FLAGGING (<5 obs) ####
short_count_IDs <- dat_argos_lst_updated %>% map_dfr(~ .x %>% 
                                   filter(any.failed.sc.filters != "TRUE" | 
                                            sc.location.origin %in% c("deploy_on_lat_long", "deploy_off_lat_long")) %>%
                                   group_by(sc.deployment.id) %>% 
                                   select(sc.deployment.id, event.id) %>%
                                   count() %>% rename(n.visible.detections = n)) %>% 
                                   filter(n.visible.detections <= 4) %>%
                                   select(sc.deployment.id) %>% pull()
short_count_IDs

## 10c) FLAG SHORT COUNTS FOR FILTERING ####
dat_argos_lst_updated <- dat_argos_lst_updated %>%
  map(~.x %>% 
        mutate(filter.visible.obs.less4 = 
                 case_when(sc.deployment.id %in% short_count_IDs ~ 0 , .default = 1)))

## 10d) UPDATE THE ANY FAILED FILTERS COLUMN ####
dat_argos_lst_updated <- dat_argos_lst_updated %>%
  map(~.x %>% mutate(any.failed.sc.filters = if_any(starts_with('filter.'), ~ . == 0)))

## 10e) ADD # VISIBLE DETECTIONS AS COLUMN ####
dat_argos_lst_updated <- map2(dat_argos_lst_updated, dep_visible_counts_lst, ~left_join(.x, .y)) #Joining with `by = join_by(sc.deployment.id)`


# 11) FLAG STATIONARY TAGS ############################################
# Birds where lat and long never changed (manually found by breaking aniMotum in Movebank Data)

## 11a) CALCULATE DISTINCT LOCATIONS PER DEPLOYMENT ####
distinct_locs_lst <- dat_argos_lst_updated %>% 
  map(~.x %>%
        filter(any.failed.sc.filters != "TRUE" | 
                 sc.location.origin %in% c("deploy_on_lat_long", "deploy_off_lat_long")) %>%
        group_by(sc.deployment.id,
                 location.lat, location.long) %>% count() %>%
        select(-n) %>% ungroup() %>%
        group_by(sc.deployment.id) %>%
        count() %>% 
        rename(n.unique.latlong = n))

## 11b) WHICH DEP IDS HAVE SINGLE LOCATION ####
stationary_IDs <- distinct_locs_lst %>% map_dfr(~.x %>% filter(n.unique.latlong < 2) %>%
                                select(sc.deployment.id)) %>% pull() 

## 11c) FLAG THESE IN ANOTHER FILTER ####
dat_argos_lst_updated <- dat_argos_lst_updated %>% 
  map(~.x %>% mutate(filter.stationary.tag = 
                       case_when(sc.deployment.id %in% stationary_IDs ~ 0 , .default = 1)))

## 11d) UPDATE THE ANY FAILED FILTERS COLUMN ####
dat_argos_lst_updated <- dat_argos_lst_updated %>%
  map(~.x %>% mutate(any.failed.sc.filters = if_any(starts_with('filter.'), ~ . == 0)))


# 12) SAVE DATA ################################################
saveRDS(dat_argos_lst_updated, "./Data/Cleaned/11_event_dat_FLAGGED_DC_DEPS_ARGOS.rds")
