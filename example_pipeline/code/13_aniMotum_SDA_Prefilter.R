# CODE: 13_aniMotum_SDA_Prefilter.R
# CODE PURPOSE: RUN SC PRE-FILTERED TRACKING DATA THROUGH aniMotum FOR ADDITIONAL FILTERING
# Before running state-space model, outliers need to be removed; hence this prefilter step
# aniMotum isn't necessary for prefiltering outliers (could use trip package instead or douglas argos filter)
# Note that GPS data is more messy than many think and really should be run through a speed or sda filter (as done here)
# Example can be found here: https://cran.r-project.org/web/packages/foieGras/vignettes/basics.html
# animotum: https://github.com/ianjonsen/aniMotum
# This uses a separate SDA prefilter for each data type (Argos only, GPS only, or Argos and GPS)


# 0) SET WORKING DIRECTORY ###############################
setwd()
Sys.setenv(TZ = 'UTC')


# 1) LOAD PACKAGES ###########################################
library(tidyverse)
# install aniMotum
# install.packages("aniMotum", 
#                  repos = c("https://cloud.r-project.org",
#                            "https://ianjonsen.r-universe.dev"),
#                  dependencies = TRUE)
# Needed to update Rtools
#install.packages('TMB', type = 'source')
library(aniMotum)
library(conflicted)
conflict_prefer("filter", "dplyr")
library(cowplot)
library(sf)
sf_use_s2(FALSE)
# Specify custom map projection:
sc_crs <- "+proj=laea +lon_0=-101.6 +lat_0=19.22 +datum=WGS84 +units=m +no_defs"

source("./Code/Functions/named_group_split.r")


# 2) LOAD DATA ############################################
dat_scpf_keeps_lst <- readRDS("./Data/Cleaned/12_event_dat_SCPREFILTER_KEEPS.rds")

# EXTRACT METADATA FOR LATER MERGING:
meta_df <- dat_scpf_keeps_lst %>%
  map_df(~.x %>% as.data.frame()) %>%
  select(sc.deployment.id, sensor.types.detected, species.code) %>%
  distinct()


# 3) DATA MANAGEMENT ##########################################
# 3a) DROP EMPTY LISTS:
# currently not needed, but needed if all data within a project prefiltered out in previous
# dat_scpf_keeps_lst <- dat_scpf_keeps_lst %>% 
#   purrr::discard(function(x) nrow(x) == 0) 


# 4) FORMAT DATA FOR aniMotum ###################################################
## 4a) FORMAT COLUMNS ####
dat_lst_am <- dat_scpf_keeps_lst %>% 
  purrr::map(~.x %>%
        select(sc.deployment.id, timestamp, argos.lc, location.long, location.lat, argos.semi.major, argos.semi.minor, argos.orientation) %>% #select input variables required for foiegras
        rename(id = sc.deployment.id, 
               date = timestamp, 
               lc = argos.lc, 
               lon = location.long, 
               lat = location.lat, 
               smaj = argos.semi.major, 
               smin = argos.semi.minor, 
               eor = argos.orientation) %>% # rename to match argos format
        mutate(lc = as.factor(lc), id = as.character(id)) %>% #Id has to be a character to run through ssm
        droplevels() %>% 
        as.data.frame())

## 4b) CONVERT TO SF AND SPECIFY LL FORMAT ####
# This basic lat/long is format of data coming out of Movebank
# If not specified aniMotum will guess the CRS for each project & ...
# there can be issues around the international dateline
dat_lst_am_sf <- dat_lst_am %>%
 purrr::map(~sf::st_as_sf(.x, coords = c("lon","lat"), 
                          crs="+proj=longlat +datum=WGS84")) # specify this is lat long format

## 4c) SET PROJECTION TO GLOBAL MERCATOR WITH 180 MERIDIAN ####
# This is the projection aniMotum uses by default
# Specify *specifically* the 180 meridian seems to resolve international dateline problems
dat_lst_am_sf_merc180 <- dat_lst_am_sf %>%
  purrr::map(~.x %>%
        st_transform("+proj=merc +lon_0=180 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs"))


# 5) PREFILTER DATA THROUGH ANIMOTUM ###################################
## 5a) GPS ONLY ####
# Use SDA on these data (important because there can be errors in GPS data causing spikes)
# Points beyond 50 - 60 km are removed if they form tight turning angles
# Allows unrealistic flight speeds locally (within 60 km); fast speeds beyond are removed
dat_prefilter_sda_gps <- aniMotum::fit_ssm(dat_lst_sensors_sf_merc180$GPS,  
                                vmax = 42, # m/s (speed from Gronroos radar study)
                                ang = c(15,25),  # c(15,25) DEFAULT (applies SDA filter in argos package); NA runs speed filter only
                                distlim = c(50000, 60000), #default is c(2500, 5000); NA shuts off SDA in favor of speed filter
                                # expands distance limits to remove spikes beyond 50-60km (spikes patterns were realistic in many cases up to 50km)
                                spdf = TRUE, # FALSE shuts off speed filter
                                min.dt = 0, # 0 second minimum allowable time between locs (allows for same timestamp to run through)
                                model = "rw", # random walk
                                pf = TRUE, # prefilter data
                                time.step = NA, # locations estimated at observation times
                                fit.to.subset =  FALSE, # Run prefilter only; TRUE runs aniMotum right away after prefiltering 
                                control = ssm_control(verbose = 0))

## 5b) ARGOS/GPS COMBO ####
# Use SDA on these data
# Allows unrealistic flight speeds at shorter distances (up to 10 km)
# Points beyond 5 km to 10 km are removed if they form an angle of < 5 and < 45 respectively
# Broadening the outer angle removes patterns of broader spikes beyond 10 km
# Unrealistic speeds removed beyond 10 km
dat_prefilter_sda_argosgps <-  aniMotum::fit_ssm(dat_lst_sensors_sf_merc180$`GPS,Argos Doppler Shift`, 
                                           vmax = 42, # m/s (speed from Gronroos radar study)
                                           ang = c(5, 45),  # broadened outer angle to account for key problems identified by visual inspection 
                                           distlim = c(5000, 10000), # default is c(2500, 5000); broadened outer filter for spikes beyond 10 km
                                           spdf = TRUE, # FALSE shuts off speed filter
                                           min.dt = 0, # 0 second minimum allowable time between locs (allows for same timestamp to run through)
                                           model = "rw", # random walk
                                           pf = TRUE, # prefilter
                                           time.step = NA, # locations estimated at observation times
                                           fit.to.subset =  FALSE,  # Run prefilter only; TRUE runs aniMotum right away after prefiltering 
                                           control = ssm_control(verbose = 0))

## 5c) ARGOS ONLY ####
# This uses the default DSA Argos filter settings for the first angles but is stricter beyond 10 km 
# Visual inspection showed patterns up to 45 degrees causing problems beyond 5 km
dat_prefilter_sda_argos <-  aniMotum::fit_ssm(dat_lst_sensors_sf_merc180$`Argos Doppler Shift`, 
                                                 vmax = 42, #m/s prefiltered based on speed specified above
                                                 ang = c(15,45),  # broadened outer angle further to account for key problems identified by visual inspection 
                                                 distlim = c(2500, 10000), #default is c(2500, 5000); broadened outer filter for spikes beyond 10 km
                                                 spdf = TRUE, # FALSE shuts off speed filter
                                                 min.dt = 0, # 0 second minimum allowable time between locs (allows for same timestamp to run through)
                                                 model = "rw", # random walk 
                                                 pf = TRUE, # run prefilter
                                                 time.step = NA, # locations estimated at observation times
                                                 fit.to.subset =  FALSE,  # Run prefilter only; TRUE runs aniMotum right away after prefiltering 
                                                 control = ssm_control(verbose = 0))

## 5d) RE-LIST DATA ####
dat_prefilter_lst_sda <- lst(dat_prefilter_sda_gps, dat_prefilter_sda_argosgps, dat_prefilter_sda_argos)


# 6) CONVERT BACK TO LAT AND LONG ########################################
dat_prefilter_lst_sda_ll <- dat_prefilter_lst_sda %>%
  purrr::map(~ .x %>% st_transform("+proj=longlat +datum=WGS84")) %>% # to match input ll
  purrr::map( ~.x %>% mutate(lon = unlist(purrr::map(.x$geometry,1)), #Add lat/lon as columns
                             lat = unlist(purrr::map(.x$geometry,2))))


# 7) GRAB FILTERED LOCATIONS ###################################
### KEEPS ####
dat_keeps_sda_lst <- dat_prefilter_lst_sda_ll %>% 
  purrr::map(~.x %>% filter(keep == "TRUE") %>% st_drop_geometry())


# 8) FLAG SHORT DURATION TRACKS (AGAIN) ####################################
# Not suitable for aniMotum
## 8a) GET TRACK DURATIONS ####
track_length_df <- dat_keeps_sda_lst %>% purrr::map_dfr(~.x %>%
                                           group_by(id) %>% # is sc.deployment.id
                                           arrange(date) %>% # is timestamp
                                           filter(row_number() == 1 | row_number() == n()) %>% # get first and last obs per individual
                                           mutate(seq.id = row_number(),
                                                  first.last.elapsed.mins = round(as.numeric(difftime(date, dplyr::lag(date, default = first(date)), units = "mins")), 0)) %>%
                                           select(id, seq.id, first.last.elapsed.mins) %>%
                                           arrange(id, seq.id) %>%
                                           filter(seq.id == 2), .id = "sc.dataset.id")

## 8b) GRAB IDs FOR TRACK LENGTH LESS THAN OR EQUAL TO 7 DAYS ####
shortIDs <- track_length_df %>%
  filter(first.last.elapsed.mins <= 10080) %>% # 7 days in minutes
  select(id) %>% pull()

## 8c) FLAG SHORT TRACKS WITH FILTER ####
# Remove these short tracks:
dat_keeps_sda_lst <- dat_keeps_sda_lst %>%
  purrr::map(~.x %>%
        mutate(filter.track.duration.less8days =
                 case_when(id %in% shortIDs ~ 0, .default = 1)))
rm(shortIDs, track_length_df)


# 9) FLAG LOW NUMBER OF VISIBLE DETECTIONS #####################################################
## 9a) COUNT # OF VISIBLE DETECTIONS ####
dep_visible_counts_df <- dat_keeps_sda_lst %>% 
  purrr::map_dfr(~ .x %>% 
            group_by(id) %>% 
            count() %>% 
            rename(n.visible.detections = n) %>%
            as.data.frame())

# Also as list for merging:
dep_visible_counts_lst <- dat_keeps_sda_lst %>% 
  purrr::map(~ .x %>% 
                   group_by(id) %>% 
                   count() %>% 
                   rename(n.visible.detections = n))

## 9b) WHICH HAVE FEWER THAN 4 DETECTIONS ####
dep_visible_counts_df %>% filter(n.visible.detections <= 4) 

## 9c) GET IDs OF THOSE FOR FLAGGING ####
short_count_IDs <- dep_visible_counts_df %>% 
  filter(n.visible.detections <= 4) %>%
  select(id) %>%
  pull()

## 9d) FLAG SHORT COUNTS FOR FILTERING ####
dat_keeps_sda_lst <- dat_keeps_sda_lst %>%
  purrr::map(~.x %>% 
        mutate(filter.visible.obs.less4 = 
                 case_when(id %in% short_count_IDs ~ 0 , .default = 1)))

## 9e) CREATE NEW ANY FAILED FILTERS COLUMN ####
dat_keeps_sda_lst <- 
  dat_keeps_sda_lst %>%
  purrr::map(~.x %>% mutate(any.failed.sc.filters = if_any(starts_with('filter.'), ~ . == 0)))

## 9f) ADD # VISIBLE DETECTIONS AS COLUMN ####
dat_keeps_sda_lst <- purrr::map2(dat_keeps_sda_lst, dep_visible_counts_lst,
                                 ~left_join(.x, .y)) #Joining with `by = join_by(id)`


# 10) SAVE KEEPS ########################################################
## 10a) FILTER OUT SC POST AM REMOVALS ####
dat_keeps_sda_lst_updated <- dat_keeps_sda_lst %>%
  purrr::map(~.x %>% filter(any.failed.sc.filters == "FALSE"))

## 10b) SAVE DATA ####
saveRDS(dat_keeps_sda_lst_updated, "./Data/Cleaned/13_event_dat_SDAFILTER_KEEPS.rds")

## 10c) CONVERT TO SF AND SAVE AS GEOPACKAGE LAYER ####
dat_keeps_sda_sf_updated <- dat_keeps_sda_lst_updated %>%
  map_df(~.x %>% as.data.frame(), .id = "sc.dataset.id") %>%
  rename(sc.deployment.id = id,
         timestamp = date, 
         argos.lc = lc, 
         location.long = lon , 
         location.lat = lat, 
         argos.semi.major = smaj, 
         argos.semi.minor = smin, 
         argos.orientation = eor) %>%
  st_as_sf(coords = c("location.long", "location.lat"),
           crs = "+proj=longlat +datum=WGS84") %>% #convert to sf
  dplyr::mutate(location.long = sf::st_coordinates(.)[,1],
                location.lat = sf::st_coordinates(.)[,2]) # Convert to sf and add coordinates back as columns on here

### MERGE SENSOR TYPES DETECTED ####
# For easier filtering
dat_keeps_sda_sf_updated <- left_join(dat_keeps_sda_sf_updated, meta_df) #Joining with `by = join_by(sc.deployment.id)`

### CONVERT TO CAMELCASE ####
dat_keeps_sda_sf_updated_sn <- dat_keeps_sda_sf_updated %>%
 rename_with(., .fn = ~ snakecase::to_lower_camel_case(.)) 
  
### EXPORT AS GEOPACKAGE LAYER ####
st_write(dat_keeps_sda_sf_updated_sn, "./Data/Geopackage/POINTS_latlon.gpkg", "sdafiltered_passed_points", append = FALSE)

### CREATE TRACKLINES AND EXPORT FOR ARCPRO ####
tracks_keeps_sda_sf_ea <- dat_keeps_sda_sf_updated %>%
  st_transform(crs = sc_crs) %>%
  group_by(sc.deployment.id) %>%
  summarize(do_union = FALSE) %>%
  st_cast("MULTILINESTRING")

# JOIN METADATA:
tracks_keeps_sda_sf_ea <- left_join(tracks_keeps_sda_sf_ea, meta_df)

# CONVERT TO CAMEL CASE FOR ARC:
tracks_keeps_sda_sf_ea <- tracks_keeps_sda_sf_ea %>%
  rename_with(., .fn = ~ snakecase::to_lower_camel_case(.)) 

### EXPORT TRACKS ####
st_write(tracks_keeps_sda_sf_ea, "./Data/Cleaned/shp/13_TRACKS_sdafilter_passed.shp", append = FALSE)

