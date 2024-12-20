# CODE: 14_aniMotum_ssm.R
# CODE PURPOSE: RUN SC PRE-FILTERED TRACKING DATA THROUGH ANIMOTUM TO ESTIMATE MOST PROBABLE LOCATION AND SPATIAL ERROR
# This script only runs tag deployments with sufficient durations and detection sample sizes 
# after considering the aniMotum speed-distance-angle prefilter step
# Note: birds tracked with GPS only, Argos only, and both Argos and GPS are run through aniMotum separately 
# because they require different prefiltering approaches
# Example can be found here: https://cran.r-project.org/web/packages/foieGras/vignettes/basics.html


# 0) SET WORKING DIRECTORY ###############################
setwd()
Sys.setenv(TZ = 'UTC')


# 1) LOAD PACKAGES ###########################################
library(tidyverse)
library(aniMotum)
library(conflicted)
conflict_prefer("filter", "dplyr")
library(cowplot)
library(sf)
source("./Code/Functions/named_group_split.r")


# 2) LOAD DATA ############################################
## 2a) DATA PASSING SC PREFILTERS WITH SHORT TRACK DURATION/FEW DETECTIONS AFTER SDA REMOVED ####
dat_scpf_keeps_lst_updated <- readRDS("./Data/Cleaned/13_event_dat_SDAFILTER_INDS.rds")
# In previous step, might be possible to export as sf, so wouldn't need to reformat below

## 2b) FORMAT FOR ANIMOTUM ####
dat_lst_am <- dat_scpf_keeps_lst_updated %>% 
  purrr::map(~.x %>%
               select(sc.deployment.id, timestamp, argos.lc, location.long, location.lat, 
                      argos.semi.major, argos.semi.minor, argos.orientation, sensor.types.detected) %>% 
               rename(id = sc.deployment.id, 
                      date = timestamp, 
                      lc = argos.lc, 
                      lon = location.long, 
                      lat = location.lat, 
                      smaj = argos.semi.major, 
                      smin = argos.semi.minor, 
                      eor = argos.orientation) %>% # rename to match argos format
               mutate(lc = as.factor(lc), id = as.character(id)) %>% #Id has to be a character to run through aniMotum
               droplevels() %>% 
               as.data.frame())

## 2c) SPLIT BY DATA TYPE ####
dat_lst_sensors <- dat_lst_am %>%
  purrr::map_dfr(~.x %>% as.data.frame()) %>%
  named_group_split(., sensor.types.detected) %>%
  purrr::map(~.x %>% select(-sensor.types.detected))
rm(dat_lst_am, dat_scpf_keeps_lst_updated)

## 2d) CONVERT TO SF AND SPECIFY LAT/LONG ####
# Otherwise AM will guess the CRS for each project
dat_lst_sensors_ll_sf <- dat_lst_sensors %>%
  purrr::map(~sf::st_as_sf(.x, coords=c("lon","lat"), crs="+proj=longlat +datum=WGS84"))

## 2d) SPECIFY AM MERCATOR PROJECTION ####
# And convert to global mercator with the meridian at 180 again (to match animotum example)
# Meridian 180 helpful to ensure dateline is correct
dat_lst_sensors_sf_merc180 <- dat_lst_sensors_ll_sf %>%
  purrr::map(~.x %>%
        st_transform("+proj=merc +lon_0=180 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs"))
# Meridian 180 needed to ensure dateline is correct (may not be required in this version of aniMotum but keeping for consistency)
rm(dat_lst_am, dat_lst_sensors_ll_sf)


# 3)  FIT SSM (RUN aniMotum): GPS ONLY DATA #################################
# SSM to account for spatial error is not needed for GPS data if outputting at the original timestep of the tag
# Running for consistency of output across data types and can adjust if needed for timestep
## 3a) SDA PREFILTER AND FIT SSM ####
dat_ssm <- aniMotum::fit_ssm(dat_lst_sensors_sf_merc180$GPS, 
                                    vmax = 42,  #m/s (SDA max speed from Gronroos radar study)
                                    ang = c(15,25), # default angles to consider
                                    distlim = c(50000, 60000), #default is c(2500, 5000); expands distance limits to remove spikes beyond 50-60km (spike patterns were realistic in many cases up to 50km)
                                    spdf = TRUE, # FALSE shuts off speed filter
                                    min.dt = 0, # 0 second minimum allowable time between locs (allows for same timestamp to run through)
                                    model = "rw", # random walk
                                    time.step = NA, # returns locaitons at observation times
                                    fit.to.subset =  TRUE, # TRUE runs aniMotum SSM right away after prefiltering 
                                    control = ssm_control(verbose = 0))


# 4) EXPLORE SSM RESULTS ##########################################################
purrr::map(dat_ssm, ~glimpse(.x) %>% print(n = Inf)) # look at all results
purrr::map(dat_ssm, ~glimpse(.x) %>% filter(converged == "FALSE")) # check if any failed to converge: NO ISSUES
purrr::map(dat_ssm, ~glimpse(.x) %>% filter(pdHess == "FALSE")) # check if TMB solved the Hessian Matrix & Obtained SEs: NO ISSUES
# All converged and all have pdHESS = true
# Some warnings that SE's could not be calculated (from gap-y data)
# Could try changing interpolation interval (see example here: https://cran.r-project.org/web/packages/foieGras/vignettes/basics.html)

# 5) SAVE SSM RESULTS ####
saveRDS(dat_ssm, "./Data/Modeled/14_SSM.rds")


# 6) EXTRACT SSM FITTED VALUES ##################################
## GRAB MODELED DAT AS SPATIAL DATA FRAME:
fit.sf_list <- purrr::map(dat_ssm, ~ grab(.x, what = "fitted", as_sf = TRUE))


# 7) CREATE SF OF AM LOCS ############################
## 7a) ADD SC DATASET ID BACK AS COLUMN ####
fit.sf_list <-  purrr::map2(fit.sf_list, names(fit.sf_list), 
                       ~ .x %>% mutate(sc.dataset.id = .y)) 

## 7b) CHANGE A FEW COLUMN NAMES ####
fit.sf_list <- fit.sf_list %>% 
  purrr::map(~.x %>%
        rename(sc.deployment.id = id,
               timestamp = date) %>%
        mutate(sc.deployment.id = as.character(sc.deployment.id)) %>% #was as.numeric for movebank data
        arrange(sc.deployment.id, timestamp) %>%
        group_by(sc.deployment.id) %>%
        mutate(fg.modeled.seq.id = row_number()) %>%
        ungroup())

# 8) SAVE MODELED DATA ##################################################
saveRDS(fit.sf_list, "./Data/Modeled/14_MODELED_SF.rds")

