# CODE: 08_Estimate_Duty_Cycles.R
# CODE PURPOSE: ESTIMATE DUTY CYCLES FOR EACH TAG ####
# Movebank data library for more info: http://vocab.nerc.ac.uk/collection/MVB/current/

# THE BELOW CODE DOES THE FOLLOWING: 
# ESTIMATES DUTY CYCLES FOR GPS TAGS
# ESTIMATES DUTY CYCLES FOR ARGOS & ARGOS/GPS TAGS
# ADDS ESTIMATED DUTY CYCLES AND CONTRIBUTOR SPECIFIED DUTY CYCLES INTO EVENT DATA
# Run this code because duty cycle not always provided or the actual interval differs from provided 
# Note: this does not work well for projects where the duty cycle switches 
# Script would be improved by functioning this out


# 0) SET WORKING DIRECTORY ##############################
setwd()
Sys.setenv(TZ = 'UTC')


# 1) LOAD PACKAGES #################################################
library(tidyverse)
library(conflicted)
library(janitor)
library(lubridate)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")

source("./Code/Functions/named_group_split.r")


# 2) LOAD IN DATA ##################################################
## 2a) EVENT DATA ####
event_dat_lst <- readRDS("./Data/Cleaned/07_event_dat_FLAGGED.rds")

## 2b) REFERENCE DATA ####
refdat_lst <- readRDS("./Data/Cleaned/07_refdat_CLEAN.rds")


# 3) GRAB REPORTED DUTY CYCLES PER DEPLOYMENT ##########################################
refdat_duty_c <- refdat_lst %>% map(~.x %>% 
                                  group_by(sc.deployment.id) %>%
                                  rename(contributed.duty.cycle = duty.cycle) %>%
                                  select(sc.deployment.id, contributed.duty.cycle))


# 4) SPLIT EVENT INTO 3 DATA TYPES #######################################
# Duty cycle estimation is a bit different depending on type of data collected
## 4a) CREATE DATAFRAME ####
event_dat_df <- event_dat_lst %>% 
  map_dfr(~.x %>% as.data.frame(.), .id = "species.scDatasetID") 

## 4b) SPLIT INTO GROUPS BY sensor.types.detected ####
dat_sensor_type_lst <- event_dat_df %>% 
  named_group_split(sensor.types.detected, species.scDatasetID) #species.movebankID
dat_sensor_type_lst <- dat_sensor_type_lst %>% 
  map(~.x %>% as.data.frame()) #making sure each list is a data frame


# 5) CALCULATE TIME ELAPSED DATA #########################################
## 5a) TIME ELAPSED BETWEEN EACH SEQUENTIAL OBSERVATION ####
dat_time_elapsed_lst <- dat_sensor_type_lst %>% 
  map(~.x %>%
        filter(any.failed.sc.filters != "TRUE") %>% #only do this for obs that we haven't flagged as problems
        arrange(sc.deployment.id, timestamp) %>%
        group_by(sc.deployment.id) %>%
        mutate(elapsed.days = round(as.numeric(difftime(timestamp, lag(timestamp, default = first(timestamp)), units = "days")), 0),
               elapsed.days.fact = as.factor(elapsed.days),
               elapsed.sensor.types = paste0(lag(sensor.type), ", ", sensor.type),
               elapsed.mins = plyr::round_any(as.numeric(difftime(timestamp, lag(timestamp, default = first(timestamp)), units = "mins")), 10),# to nearest 10 minutes
               elapsed.mins.fact = as.factor(elapsed.mins)) %>% 
              ungroup())

## 5b) COUNT NUMBER OF INSTANCES OF EACH DAY ELAPSED CATEGORY ####
# For figure below
# Want to know the number of times we see 2 days elapsed, or 0 days elapsed, etc.
min_count_lst <- dat_time_elapsed_lst %>% 
  map(~.x %>%
        group_by(sc.dataset.id, sc.deployment.id, individual.local.identifier, elapsed.mins.fact, elapsed.sensor.types) %>%
        count() %>% ungroup() %>%
        mutate(elapsed.mins = as.numeric(as.character(elapsed.mins.fact))) %>%
        rename(n.instances = n))


# 6) PLOT TIME LAPSES ################################################
## 6a) GET FIGURE NAMES ####
st_proj_names <- names(min_count_lst)

## 6b) MAKE FIGS ####
mins_elapsed_figs <- map2(min_count_lst, st_proj_names, (~.x %>%
                                                            filter(!(elapsed.sensor.types %in% c("NA, Argos Doppler Shift", "NA, GPS"))) %>%
                                      ggplot(aes(x = elapsed.mins, y = n.instances, color = elapsed.sensor.types, alpha = 0.5)) +
                                        scale_alpha(guide = 'none')+
                                      geom_point() +
                                      theme_bw() +
                                      xlim(c(0, 20160)) + # 4320 show only 3 days for Argos ; 20160 show 14 days for GPS
                                      facet_wrap(~individual.local.identifier, scales = "free_y")+
                                      ggtitle(.y) +
                                      labs(subtitle = "individual local identifer")+
                                      xlab("mins elapsed between each detection") +
                                      ylab("number of observations")+
                                      theme(legend.position = "bottom"))) #figures not helpful for projs with tons of tags

## 6c) MAKE FIGURE FILE PATHS ####
fig_names <- min_count_lst %>% 
  map(~.x %>% select(sc.dataset.id) %>% 
        distinct()) %>%
  map(~paste0("./Figures/DutyCycleFigs/", .x, "_min_count_plot.png"))

## 6d) SAVE THE PLOTS ####
walk2(mins_elapsed_figs, fig_names,
      ~ggsave(plot = .x,
              filename = .y,
              device = "png",
              height = 8,
              width = 11,
              units = "in",
              dpi = 350))
# For Argos detections, will remove the first 750 minutes (for uplinks with satellite)
## For Argos and GPS data combined, elapsed counts between Argos and GPS (in that order) and Argos, Argos are not what we are interested in
# ... for those need GPS to GPS OR GPS to Argos
# ... Argos to GPS is not needed because usually when an argos uplink occurs, a GPS obs is recorded at the same time
# These show up as blue and purple dots in the figs


# 7) SUMMARY STATS ON MIN ELAPSED ####################################
## 7a) MODE FUNCTION ####
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## 7b) GPS ONLY: CALCULATE SUMMARY STATS & ESTIMATE CYCLES ####
# This chunk gets the min and max and most common interval per deployment type
# does not take into account the different transitions between GPS and Argos for example
gps_only_elapsed_summary_lst <-  keep(dat_time_elapsed_lst, 
                                      str_detect(names(min_count_lst), 'GPS ')) %>% # grab gps only tags %>% 
  map(~.x %>% group_by(sc.deployment.id) %>% 
        arrange(elapsed.mins) %>%
                                         summarise(mean.gap.mins = round(mean(elapsed.mins), digits = 0),
                                                   mode.gap.mins = getmode(elapsed.mins),
                                                   median.gap.mins = median(elapsed.mins), 
                                                   mode.gap.hrs = round(mode.gap.mins/60, digits = 1),
                                                   median.gap.hrs = round(median.gap.mins/60, digits = 1), 
                                                   mode.gap.days = round(mode.gap.hrs/24, digits = 1),
                                                   median.gap.days = round(median.gap.hrs/24, digits = 1),
                                                   n.obs = n()) %>%
                mutate(estimated.duty.cycle.mins = case_when(median.gap.mins > 0 ~ median.gap.mins,
                                                    median.gap.mins == 0 & mean.gap.mins > 0 ~ mean.gap.mins, #if median is 0, use the mean
                                                    median.gap.mins == 0 & mean.gap.mins == 0 & n.obs == 1 ~ NA), #not enough data to estimate
                       estimated.duty.cycle.hrs = round(estimated.duty.cycle.mins/60, 0),
                       estimated.duty.cycle.days = round(estimated.duty.cycle.mins/60/24, 0),
                       estimated.duty.cycle.description = case_when(estimated.duty.cycle.mins < 60 ~ paste0("1 detection every ", estimated.duty.cycle.mins, " minutes"), 
                                                                    estimated.duty.cycle.mins >= 60 & estimated.duty.cycle.mins < 1440 ~ paste0("1 detection every ", estimated.duty.cycle.hrs, " hours"), 
                                                                    estimated.duty.cycle.mins >= 1440 ~  paste0("1 detection every ", estimated.duty.cycle.days, " days"), .default = "insufficient data to estimate")))

### GET GPS WITH CELL LOCATIONS ####
# Same approach as above because we are using locations estimated by cell triangulation for now
gps_cell_elapsed_summary_lst <-  keep(dat_time_elapsed_lst, 
                                      str_detect(names(min_count_lst), 'GPS,Cellular Location')) %>% 
  map(~.x %>% group_by(sc.deployment.id) %>% 
        arrange(elapsed.mins) %>%
        summarise(mean.gap.mins = round(mean(elapsed.mins), digits = 0),
                  mode.gap.mins = getmode(elapsed.mins),
                  median.gap.mins = median(elapsed.mins), 
                  mode.gap.hrs = round(mode.gap.mins/60, digits = 1),
                  median.gap.hrs = round(median.gap.mins/60, digits = 1), 
                  mode.gap.days = round(mode.gap.hrs/24, digits = 1),
                  median.gap.days = round(median.gap.hrs/24, digits = 1),
                  n.obs = n()) %>%
        mutate(estimated.duty.cycle.mins = case_when(median.gap.mins > 0 ~ median.gap.mins,
                                                     median.gap.mins == 0 & mean.gap.mins > 0 ~ mean.gap.mins, #if median is 0, use the mean
                                                     median.gap.mins == 0 & mean.gap.mins == 0 & n.obs == 1 ~ NA), #not enough data to estimate
               estimated.duty.cycle.hrs = round(estimated.duty.cycle.mins/60, 0),
               estimated.duty.cycle.days = round(estimated.duty.cycle.mins/60/24, 0),
               estimated.duty.cycle.description = case_when(estimated.duty.cycle.mins < 60 ~ paste0("1 detection every ", estimated.duty.cycle.mins, " minutes"), 
                                                            estimated.duty.cycle.mins >= 60 & estimated.duty.cycle.mins < 1440 ~ paste0("1 detection every ", estimated.duty.cycle.hrs, " hours"), 
                                                            estimated.duty.cycle.mins >= 1440 ~  paste0("1 detection every ", estimated.duty.cycle.days, " days"), .default = "insufficient data to estimate")))
### APPEND THE TWO GPS ONLY LISTS ####
gps_only_elapsed_summary_lst <- append(gps_only_elapsed_summary_lst, gps_cell_elapsed_summary_lst)


## 7c) ARGOS ONLY: CALCULATE SUMMARY STATS & ESTIMATE CYCLES ####
# Remove gaps before 600 hours because those are short gaps that occur during the 10 hour on period
10 * 60 # tags often set for 10 hours on; times 60 mins 
argos_only_elapsed_summary_lst <- discard(dat_time_elapsed_lst, str_detect(names(min_count_lst), 'GPS')) %>% # keep lists without GPS in name
  map(~.x %>% group_by(sc.deployment.id) %>% 
        arrange(elapsed.mins) %>%
        filter(elapsed.mins > 600) %>% 
        summarise(mean.gap.mins = round(mean(elapsed.mins), digits = 0),
                  mode.gap.mins = getmode(elapsed.mins),
                  median.gap.mins = median(elapsed.mins), 
                  mode.gap.hrs = round(mode.gap.mins/60, digits = 1),
                  median.gap.hrs = round(median.gap.mins/60, digits = 1), 
                  mode.gap.days = round(mode.gap.hrs/24, digits = 1),
                  median.gap.days = round(median.gap.hrs/24, digits = 1),
                  n.obs = n()) %>%
        mutate(estimated.duty.cycle.mins = case_when(median.gap.mins > 0 ~ median.gap.mins,
                                                     median.gap.mins == 0 & mean.gap.mins > 0 ~ mean.gap.mins, #if median is 0, use the mean
                                                     median.gap.mins == 0 & mean.gap.mins == 0 & n.obs == 1 ~ NA), #not enough data to estimate
               estimated.duty.cycle.hrs = round(estimated.duty.cycle.mins/60, 0),
               estimated.duty.cycle.days = round(estimated.duty.cycle.mins/60/24, 0),
               estimated.duty.cycle.description = case_when(estimated.duty.cycle.mins < 60 ~ paste0("multiple detections every ", estimated.duty.cycle.mins, " minutes"), 
                                                            estimated.duty.cycle.mins >= 60 & estimated.duty.cycle.mins < 1440 ~ paste0("multiple detections every ", estimated.duty.cycle.hrs, " hours"), 
                                                            estimated.duty.cycle.mins >= 1440 ~  paste0("multiple detections every ", estimated.duty.cycle.days, " days"), .default = "insufficient data to estimate")))


## 7d) ARGOS AND GPS: CALCULATE SUMMARY STATS & ESTIMATE CYCLES ####
# Exploratory plots in step 6 showed that we should consider only counts of instances...
#... of GPS to Argos detections or GPS to GPS detections
#... these are flagged in the elapsed.sensor.types column
# This code may not work as well for these tags when they may have irregular sampling intervals
argos_gps_elapsed_summary_lst <- keep(dat_time_elapsed_lst, str_detect(names(min_count_lst), 'GPS,Argos Doppler Shift')) %>% 
  map(~.x %>% group_by(sc.deployment.id) %>% 
        arrange(elapsed.mins) %>%
        filter(elapsed.sensor.types %in% c("GPS, Argos Doppler Shift", "GPS, GPS")) %>%
        summarise(mean.gap.mins = round(mean(elapsed.mins), digits = 0),
                  mode.gap.mins = getmode(elapsed.mins),
                  median.gap.mins = median(elapsed.mins), 
                  mode.gap.hrs = round(mode.gap.mins/60, digits = 1),
                  median.gap.hrs = round(median.gap.mins/60, digits = 1), 
                  mode.gap.days = round(mode.gap.hrs/24, digits = 1),
                  median.gap.days = round(median.gap.hrs/24, digits = 1),
                  n.obs = n()) %>%
        mutate(estimated.duty.cycle.mins = case_when(median.gap.mins > 0 ~ median.gap.mins,
                                                     median.gap.mins == 0 & mean.gap.mins > 0 ~ mean.gap.mins, #if median is 0, use the mean
                                                     median.gap.mins == 0 & mean.gap.mins == 0 & n.obs == 1 ~ NA), #not enough data to estimate
               estimated.duty.cycle.hrs = round(estimated.duty.cycle.mins/60, 0),
               estimated.duty.cycle.days = round(estimated.duty.cycle.mins/60/24, 0),
               estimated.duty.cycle.description = case_when(estimated.duty.cycle.mins < 60 ~ paste0("1 or 2 detections every ", estimated.duty.cycle.mins, " minutes"), 
                                                            estimated.duty.cycle.mins >= 60 & estimated.duty.cycle.mins < 1440 ~ paste0("1 or 2 detections every ", estimated.duty.cycle.hrs, " hours"), # say 1 or 2 detections because often there is an Argos detection and a GPS detection during the uplink
                                                            estimated.duty.cycle.mins >= 1440 ~  paste0("1 or 2 detections every ", estimated.duty.cycle.days, " days"), .default = "insufficient data to estimate")))


# 8) COMBINE ALL DUTY CYCLE DATA ##############################
## 8a) GET DATAFRAMES ####
# GPS:
gps_only_dc_df <- gps_only_elapsed_summary_lst %>% map_dfr(~.x %>% 
                                                               select(sc.deployment.id, estimated.duty.cycle.mins,
                                                                      estimated.duty.cycle.hrs, estimated.duty.cycle.days,
                                                                      estimated.duty.cycle.description))
# ARGOS ONLY:
argos_only_dc_df <- argos_only_elapsed_summary_lst %>% map_dfr(~.x %>% 
                                                             select(sc.deployment.id, estimated.duty.cycle.mins,
                                                                    estimated.duty.cycle.hrs, estimated.duty.cycle.days,
                                                                    estimated.duty.cycle.description))

# ARGOS AND GPS:
argos_gps_dc_df <- argos_gps_elapsed_summary_lst %>% map_dfr(~.x %>% 
                                            select(sc.deployment.id, estimated.duty.cycle.mins,
                                                   estimated.duty.cycle.hrs, estimated.duty.cycle.days,
                                                   estimated.duty.cycle.description))
# Combine lists
duty_cycles_df <- rbind(gps_only_dc_df, argos_only_dc_df, argos_gps_dc_df) # deployment ID and estimated duty cycle


# 9) COMBINE ESTIMATED & CONTRIBUTOR REPORTED DUTY CYCLE INFO ##########
refdat_duty_c_df <- refdat_duty_c %>% map_dfr(~.x %>% as.data.frame) 
# Merge together:
duty_cycles_df <- left_join(refdat_duty_c_df, duty_cycles_df) #Joining, by = "sc.deployment.id"


# 10) ADD DUTY CYCLE TO EVENT DATA ###############################
## 10a) ADD TOGETHER ####
event_dat_dc_df <- left_join(event_dat_df, duty_cycles_df) #Joining with `by = join_by(sc.deployment.id)`

## 10b) FIX THE "ESTIMATED DUTY CYCLE" for birds that couldn't be estimated above:
event_dat_dc_df <- event_dat_dc_df %>% 
  mutate(estimated.duty.cycle.description = 
           case_when(is.na(estimated.duty.cycle.description) ~ "insufficient data to estimate", .default = estimated.duty.cycle.description))

## 10c) RELIST DATA TO KEEP OBS IN PROJECT LISTS ####
event_dat_lst <- event_dat_dc_df %>% 
  named_group_split(individual.taxon.canonical.name, sc.dataset.id) 


# 11) ADD ESTIMATED DUTY CYCLE TO REFERENCE DATA ##################
## 11a) GET IN DATA FRAME ####
refdat_df <- refdat_lst %>% map_dfr(~.x %>% as.data.frame())

## 11b) MERGE IN DUTY CYCLE DATA ###
refdat_df <- left_join(refdat_df, duty_cycles_df) # Joining with `by = join_by(sc.deployment.id)`

## 11c) FIX ESTIMATED DUTY CYCLE FOR NAs ####
refdat_df <- refdat_df %>% 
  mutate(estimated.duty.cycle.description = 
           case_when(is.na(estimated.duty.cycle.description) ~ "insufficient data to estimate", .default = estimated.duty.cycle.description))

## 11d) RELIST DATA TO KEEP OBS IN PROJECT LISTS ####
refdat_lst <- refdat_df %>% 
  named_group_split(animal.taxon.canonical.name, sc.dataset.id) 


# 12) SAVE DATA WITH DUTY CYCLE INFO ADDED #######################
## 12a) EVENT DATA ####
saveRDS(event_dat_lst, "./Data/Cleaned/08_event_dat_FLAGGED_DC.rds")

## 12b) REF DATA ####
saveRDS(refdat_lst, "./Data/Cleaned/08_refdat_DC.rds")
