# CODE: 14_aniMotum_ssm.R
# CODE PURPOSE: RUN SC PRE-FILTERED TRACKING DATA THROUGH ANIMOTUM TO ESTIMATE MOST PROBABLE LOCATION AND SPATIAL ERROR
# This script only runs tag deployments with sufficient durations and detection sample sizes 
# after considering the aniMotum speed-distance-angle prefilter step
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


# 2) LOAD DATA ############################################
## 2a) DATA PASSING SC PREFILTERS WITH SHORT TRACK DURATION/FEW DETECTIONS AFTER SDA REMOVED ####
# Deployments with short track duration and few detections removed in previous script
# And formatted for ANIMOTUM:
dat_scpf_keeps_lst_updated <- readRDS("./Data/Cleaned/13_event_dat_SDAFILTER_INDS.rds")
# In previous step, might be possible to export as sf, so wouldn't need to reformat below

## 2b) FORMAT FOR ANIMOTUM ####
# Drop extra columns:
am_lst <- dat_scpf_keeps_lst_updated %>% 
  purrr::map( ~.x %>% 
         as.data.frame() %>% 
                   mutate(lc = as.factor(lc), id = as.character(id)) %>%
         select(id, date, lc, lon, lat, smaj, smin, eor)) # will not run if data in the wrong order 

## 2c) CONVERT TO SF AND SPECIFY LAT/LONG ####
# Otherwise AM will guess the CRS for each project
am_lst_sf_ll <- am_lst %>%
  purrr::map(~sf::st_as_sf(.x, coords=c("lon","lat"), crs="+proj=longlat +datum=WGS84"))

## 2d) SPECIFY AM MERCATOR PROJECTION ####
# And convert to global mercator with the meridian at 180 again (to match animotum example)
# Meridian 180 helpful to ensure dateline is correct
am_lst_sf_merc180 <- am_lst_sf_ll %>%
  purrr::map(~.x %>%
        st_transform("+proj=merc +lon_0=180 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs"))
# Meridian 180 needed to ensure dateline is correct (may not be required in this version of aniMotum but keeping for consistency)
rm(am_lst, am_lst_sf_ll)


# 3) FIT SSM (RUN aniMotum) #######################################################
## 3a) FIT CONTINUOUS SSM ####
dat_ssm <- am_lst_sf_merc180 %>% 
  purrr::map(~aniMotum::fit_ssm(.x, 
                                spdf = FALSE, #FALSE shuts off speed filter
                                min.dt = 0, #0 second minimum allowable time between locs (allows for same timestamp to run through)
                                model = "rw", #crw is a random walk on velocity
                                pf = FALSE, # DONT PREFILTER (WE HAVE DONE THIS ALREADY) just run SSMs
                                time.step = NA, # locations estimated at observation times; can adjust to estimate and different intervals (in hours)
                                fit.to.subset =  FALSE, # DONT fit to prefiltered data (because already prefiltered)
                                control = ssm_control(verbose = 0)))


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

