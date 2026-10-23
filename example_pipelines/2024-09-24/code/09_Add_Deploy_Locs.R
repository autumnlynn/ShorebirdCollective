# CODE: 09_Add_Deploy_Locs.R
# CODE PURPOSE: ADD DEPLOYMENT LOCATIONS AS FIRST POINT
# Movebank data library for more info: http://vocab.nerc.ac.uk/collection/MVB/current/

# THE BELOW CODE DOES THE FOLLOWING: 
# Adds deployment on/off locations (if available) to the data to help with prefiltering...
# (removing bad first and last locations)...
# And resulting in more complete tracks for birds not detected at tagging location 
# Note: First locations may be off if data owner uses a general deployment location


# 0) SET WORKING DIRECTORY ##############################
setwd()
Sys.setenv(TZ = 'UTC')

# LOAD CUSTOM FUNCTION: 
# Will create custom function (pull into separate code later)
count_decimals <- function(x) {
  #length zero input
  if (length(x) == 0) return(numeric())
  
  #count decimals
  x_nchr = x %>% abs() %>% as.character() %>% nchar() %>% as.numeric()
  x_int = floor(x) %>% abs() %>% nchar()
  x_nchr = x_nchr - 1 - x_int
  x_nchr[x_nchr < 0] = 0
  
  x_nchr
}


# 1) LOAD PACKAGES #################################################
library(tidyverse)
library(conflicted)
library(janitor)
library(lubridate)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")


# 2) LOAD IN DATA ##################################################
## 2a) EVENT DATA ####
event_dat_lst <- readRDS("./Data/Cleaned/08_event_dat_FLAGGED_DC.rds")

## 2b) REF DATA ####
refdat_lst <- readRDS("./Data/Cleaned/08_refdat_DC.rds")


# 3) GET SUITABLE DEPLOYMENT ON AND OFF LOCATIONS #############################################
# Get deployment locations with enough resolution (3 decimal digits) -- 4 digits would be more precise
# Flag argos.lc code as GPS locations so they can be run through aniMotum
# Flag as coming from deploy on or off locations

## 3a) GET DEPLOYMENT ON LOCATIONS ####
deploy.on.to.add <- refdat_lst %>% 
  map(~.x %>%
        select(animal.id, deployment.id, tag.id, study.id, animal.local.identifier,
               tag.local.identifier, animal.taxon.canonical.name, sensor.types.detected, 
               LEYE.id, HUGO.id, species.code,
               sc.dataset.id, sc.individual.id, sc.deployment.id, contributed.duty.cycle, 
               estimated.duty.cycle.mins, estimated.duty.cycle.hrs, estimated.duty.cycle.days, 
               estimated.duty.cycle.description,
               deploy.on.timestamp, deploy.on.latitude, deploy.on.longitude) %>%
        mutate(deploy.on.timestamp = as.POSIXct(deploy.on.timestamp, format = "%Y-%m-%d %T", tz = "UTC"),
               dep.lat.digits = count_decimals(deploy.on.latitude),
               dep.long.digits = count_decimals(deploy.on.longitude)) %>%
        filter(!(dep.lat.digits < 3 | dep.long.digits < 3 |  # limit to obs that have at least 3 digits (111m precision)
                   is.na(deploy.on.timestamp) | is.na(dep.lat.digits) | 
                   is.na(dep.long.digits))) %>%
        mutate(argos.lc = "G", 
               sc.location.origin = "deploy_on_lat_long") %>% # so we can separate out where the location came from (tag recording or deploy on etc.)
        rename(individual.id = animal.id, # match movebank ref and event columns
               individual.local.identifier = animal.local.identifier, # match movebank ref and event columns
               individual.taxon.canonical.name = animal.taxon.canonical.name, # match movebank ref and event columns
               timestamp = deploy.on.timestamp, 
               location.lat = deploy.on.latitude,
               location.long = deploy.on.longitude)) # to match full dataframe

## 3b) GET DEPLOYMENT OFF LOCATIONS ####
deploy.off.to.add <- refdat_lst %>% 
  map(~.x %>%
        select(animal.id, deployment.id, tag.id, study.id, animal.local.identifier,
               tag.local.identifier, animal.taxon.canonical.name, sensor.types.detected, 
               LEYE.id, HUGO.id, species.code,
               sc.dataset.id, sc.individual.id, sc.deployment.id, contributed.duty.cycle, 
               estimated.duty.cycle.mins, estimated.duty.cycle.hrs, estimated.duty.cycle.days, 
               estimated.duty.cycle.description,
               deploy.off.timestamp, deploy.off.latitude, deploy.off.longitude) %>%
        mutate(deploy.off.timestamp = as.POSIXct(deploy.off.timestamp, format = "%Y-%m-%d %T", tz = "UTC"),
               dep.lat.digits = count_decimals(deploy.off.latitude),
               dep.long.digits = count_decimals(deploy.off.longitude)) %>%
        filter(!(dep.lat.digits < 3 | dep.long.digits < 3 |
                   is.na(deploy.off.timestamp) | is.na(dep.lat.digits) | 
                   is.na(dep.long.digits))) %>%
        mutate(argos.lc = "G", 
               sc.location.origin = "deploy_off_lat_long") %>%
        rename(individual.id = animal.id, # match movebank ref and event columns
               individual.local.identifier = animal.local.identifier, # match movebank ref and event columns
               individual.taxon.canonical.name = animal.taxon.canonical.name, # match movebank ref and event columns,
               timestamp = deploy.off.timestamp, 
               location.lat = deploy.off.latitude,
               location.long = deploy.off.longitude))


# 4) ADD DEPLOYMENT LOCATION DATA TO EVENT DATA ##################################
## 4a) ADD IDENTIFIER THAT SHOWS THAT ORIGINAL DATA IS DIRECTLY COMING FROM TAGS ####
event_dat_lst <- event_dat_lst %>% map(~.x %>% 
                                         mutate(sc.location.origin = "tag"))

## 4b) ADD DEPLOYMENT ON LOCATIONS ####
event_dat_lst <- map2(event_dat_lst, deploy.on.to.add, ~bind_rows(.x,.y)) 

## 4c) ADD DEPLOYMENT OFF LOCATIONS ####
event_dat_lst <- map2(event_dat_lst, deploy.off.to.add, ~bind_rows(.x,.y)) 


# 5) SAVE EVENT DATA ##############################################################
saveRDS(event_dat_lst, "./Data/Cleaned/09_event_dat_FLAGGED_DC_DEPS.rds")


# 6) UPDATE REF DATA ####################################################
## 6a) CREATE "deployment.location.provided" COLUMN ####
refdat_add_lst <- deploy.on.to.add %>% map(~.x %>% select(sc.deployment.id) %>%
                           mutate(deployment.location.provided = "yes"))

## 6b) ADD TO REFERENCE DATA ####
refdat_lst_deps <- map2(refdat_lst, refdat_add_lst, ~left_join(.x,.y)) #Joining with `by = join_by(sc.deployment.id)`

## 6c) UPDATE deployment.location.provided LOCATION ####
# to "no" if not provided
refdat_lst_deps <- refdat_lst_deps %>% 
  map(~.x %>% mutate(deployment.location.provided = case_when(is.na(deployment.location.provided) ~ "no",
                                                              .default = deployment.location.provided)))

## 6d) SAVE UPDATED REFERENCE DATA ####
saveRDS(refdat_lst_deps, "./Data/Cleaned/09_refdat_DC_DEPS.rds")

