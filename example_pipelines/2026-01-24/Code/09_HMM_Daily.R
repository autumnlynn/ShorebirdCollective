# CODE: 09_HMM_Daily.R
# CODE PURPOSE: RUN HMM ON DAILY LOCATIONS, POOLED ACROSS ALL SPECIES 
#
# To separate points during stationary periods from those during flights
# Note: moveHMM does not take into account uncertainty in the locations (and the step lengths and turning angles) estimated from the SSMs
# An alternative could be to simulate multiple plausible tracks using post_sim in aniMotum and then...
# ... use multiple imputation in momentuHMM to determine the effect of uncertainty on the state estimates
# Alternatively, aniMotum SSM could be avoided altogether, instead fitting a crw SSM fit through crawl wrap directly in momentuHMM, which does the MI and pools estimates internally
# This code is a prelim quick approach 


# 00) SETUP ####
data_version <- "2026-01-24"

## SET WORKING DIRECTORY ##############################
setwd()
Sys.setenv(TZ = 'UTC')


# 1) LOAD PACKAGES ###########################################
library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(rnaturalearth)
library(moveHMM)
sc_crs <- "+proj=laea +lon_0=-101.6 +lat_0=19.22 +datum=WGS84 +units=m +no_defs"


# 2) LOAD MODELED DATA #####################################################
plocs_sf_gps <- readRDS(paste0("./Data/rds/Modeled/07_PREDICTED_Daily_SF_gps_",data_version,".rds"))
plocs_sf_argosgps <- readRDS(paste0("./Data/rds/Modeled/07_PREDICTED_Daily_SF_argosgps_",data_version,".rds"))
plocs_sf_argos <- readRDS(paste0("./Data/rds/Modeled/07_PREDICTED_Daily_SF_argos_",data_version,".rds"))


# 3) FORMAT MODELED DATA ##################################################
# 3a) UNLIST INTO ONE SF DATAFRAME ####
plocs_single_sf <- dplyr::bind_rows(plocs_sf_gps, plocs_sf_argosgps,plocs_sf_argos) # Note, only works if data are the same projection (all are bc consistent animotum world mercator output) 
plocs_single_sf
rm(plocs_sf_gps, plocs_sf_argosgps, plocs_sf_argos)

## 3b) CONVERT TO LAT/LONG FOR CONSISTENCY ####
plocs_single_sf <- plocs_single_sf %>%
  st_transform(crs = "EPSG:4326")


# 4) FORMAT FOR MOVEHMM ###############################################################
## 4a) moveHMM FORMAT ####
dat_format_df <- plocs_single_sf %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1],
                y = sf::st_coordinates(.)[,2]) %>%
  arrange(segment_id, timestamp) %>%
  rename(ID = segment_id) %>%
  dplyr::select(ID, x, y) %>%
  st_drop_geometry()

## 4b) PREPDATA FOR HMM ####
hmm_dat_df <- prepData(dat_format_df, type = "LL")


# 5) CHECK STEP LENGTHS AND TURNING ANGLES #################################################
## 5a) ADD SPECIES CODES BACK IN ####
codes_df <- plocs_single_sf %>% 
  st_drop_geometry() %>%
  distinct(species_code, segment_id) %>%
  rename(ID = segment_id)
hmm_dat_df_sp <- left_join(hmm_dat_df, codes_df) # Joining with `by = join_by(ID)`

## 5b) CHECK STEP LENGTHS ####
# Step lengths plotted here are in km

### ACROSS ALL SPECIES/SEGMENTS:
hist(hmm_dat_df$step, xlab = "step length")

### ZOOM TO STEPS LESS THAN 30 KM:
ggplot(hmm_dat_df %>%
         filter(step < 30), aes(x=step)) + geom_histogram() 
# Most are actually less than 10km

### ZOOM TO < 500 KM:
ggplot(hmm_dat_df %>%
         filter(step < 500), aes(x=step)) + geom_histogram() 

### ZOOM TO < 100 KM:
ggplot(hmm_dat_df %>%
         filter(step < 100), aes(x=step)) + geom_histogram() 
# MOST LESS THAN 30, ALMOST ALL LESS THAN 50

## 5c) CHECK TURNING ANGLES ####
hist(hmm_dat_df$angle, breaks = seq(-pi, pi, length = 15), xlab = "turning angle")
# plot(data, compact=T)

## 5d) CHECK STEP LENGTHS BY SPECIES ####
ggplot(hmm_dat_df_sp %>%
         filter(step < 100), aes(x=step)) + 
  geom_histogram(binwidth = 5) + 
  facet_wrap(~species_code, scales = "free_y") + 
  scale_x_continuous(breaks = seq(0, 100, by = 20))+
  theme_bw() 
# Very similar across species with most <30km 

ggplot(hmm_dat_df_sp %>%
         filter(step < 500), aes(x = step)) + 
  geom_histogram() + 
  facet_wrap(~species_code, scales = "free_y") + 
  theme_bw() 


# 6) TRY A 2 STATE MODEL #########################################################
# STATE 1: STOPS (short and turny)
# STATE 2: FLIGHTS (long and straight)

## 6a) PARAMETERS STEP LENGTH ####
stepMean2 <- c(10, 100) # initial means in km for two parameters (1st is stationary, second is flights)
stepSD2 <- c(10, 100) # initial standard deviations (one for each state); keeping the same to start
stepPar2 <- c(stepMean2, stepSD2)

## ARE THERE STEPS OF 0? ####
whichzero <- which(hmm_dat_df$step == 0)
whichzero
length(whichzero) 
nrow(hmm_dat_df)
length(whichzero)/nrow(hmm_dat_df) # very small percentage though (< 1 %)

# CREATE ZERO-MASS PARAMETER TO DEAL WITH STEPS OF ZERO IN DATA:
zeromass2 <- c(0.01, 0.01)  #zero mass params are between 0 and 1: https://github.com/TheoMichelot/moveHMM/issues/2
stepPar2 <- c(stepMean2, stepSD2, zeromass2) #update the step length parameters to include the zero mass term

## 6b) PARAMETERS TURNING ANGLES ####
angleMean2 <- c(pi, 0) # initial means...
#(state 1, stopover at the end of the distributions and = more turning)
#(state 2, flights at the middle and indicate directional persistence (straight forward movement))
angleCon2 <- c(1, 10) # initial concentrations (one for each state)
anglePar2 <- c(angleMean2, angleCon2)

## 6c) FIT HMM ####
mod2 <- fitHMM(data = hmm_dat_df, nbStates = 2,
               stepPar0 = stepPar2, anglePar0 = anglePar2)
plot(mod2, plotTracks = FALSE)

## 6d) SAVE HMM RESULTS ####
saveRDS(mod2, paste0("./Data/rds/Modeled/09_1d_rw_15daygap_5ptseg_HMM_2state_", data_version,".rds"))

## 6e) PLOT INDIVIDUALS ####
plot(mod2, ask = FALSE, animals = 200) #hard to tell here and running a bit slow

## 6f) STATE PROCESS INFERENCE ####
# STATE 1: STATIONARY
# STATE 2: MIGRATION

### ADD MOST LIKELY STATE SEQUENCE TO DATA #### 
hmm_dat_df$state2 <- factor(viterbi(mod2))

### PLOT TRACKS COLORED BY STATE ####
ggplot(hmm_dat_df, aes(x, y, col = state2, group = ID)) +
  geom_path() +
  coord_equal() 

### PLOT STEP LENGTHS COLOURED BY THE STATE ####
ggplot(hmm_dat_df, aes(x = 1:nrow(hmm_dat_df),
                 y = step,
                 col = state2,
                 group = ID)) +
  geom_point(size = 0.3)

### PLOT ANGLES COLORED BY THE STATE #### 
ggplot(hmm_dat_df, aes(x = 1:nrow(hmm_dat_df),
                 y = angle,
                 col = state2,
                 group = ID)) +
  geom_point(size = 0.3)


# 7) TRY A 3 STATE MODEL #########################################################
# STATE 1: RW INTERPOLATIONS (these are strings of short steps in straight lines generated during data gaps from the rw model in the SSM)
# Note: ideally we would not need this step, instead changing the time prediction interval of the SSM or decreasing the gap length for creating a new segment in 
# ... in aniMotum
# STATE 2: STOP WITH BACK AND FORTH LOCAL MOVEMENTS
# STATE 3: MIGRATORY FLIGHT

## 7a) STEP LENGTH ####
stepMean3 <- c(0.5, 10, 300) # initial means in km for three parameters (1st rw interpolations, 2nd is stop with back and forth movements, 3rd is mig)
stepSD3 <- c(0.5, 10, 300) # initial standard deviations (one for each state); keeping the same to start
zeromass3 <- c(0.01, 0.01, 0.01)  #zero mass params are between 0 and 1: https://github.com/TheoMichelot/moveHMM/issues/2
stepPar3 <- c(stepMean3, stepSD3, zeromass3)

## 7b) TURNING ANGLES ####
angleMean3 <- c(0, pi, 0) # initial means...
#(state 1, rw straight 
#(state 2, stops with lots of turns
#(state 3, flights during migration and indicate directional persistence)
angleCon3 <- c(5, 1, 10) # initial concentrations (one for each state, most persistence for the migration flights) 
anglePar3 <- c(angleMean3, angleCon3)

## 7c) FIT HMM ###
mod3 <- fitHMM(data = hmm_dat_df, nbStates = 3,
               stepPar0 = stepPar3, anglePar0 = anglePar3)
AIC(mod2, mod3) 
plot(mod3, plotTracks = FALSE) 

## 7d) SAVE HMM RESULT ####
saveRDS(mod3, paste0("./Data/rds/Modeled/09_1d_rw_15daygap_5ptseg_HMM_3state_", data_version,".rds"))

## 7e) STATE PROCESS INFERENCE ####
### ADD MOST LIKELY STATE SEQUENCE TO DATA #### 
hmm_dat_df$state3 <- factor(viterbi(mod3))

### PLOT TRACKS COLORED BY STATE ####
ggplot(hmm_dat_df, aes(x, y, 
                 col = state3, 
                 group = ID)) + 
  geom_path() +
  coord_equal() 

### PLOT STEP LENGTHS COLOURED BY THE STATE ####
ggplot(hmm_dat_df, aes(x = 1:nrow(hmm_dat_df), 
                 y = step, 
                 col = state3, 
                 group = ID)) + 
  geom_point(size = 0.3)

### PLOT ANGLES COLORED BY THE STATE #### 
ggplot(hmm_dat_df, aes(x = 1:nrow(hmm_dat_df), 
                 y = angle, 
                 col = state3, 
                 group = ID)) + 
  geom_point(size = 0.3)


# 8) FORMAT AND SAVE DATA #########################
## 8a) FORMAT ####
hmm_dat_sf <- cbind(plocs_single_sf %>%
                   dplyr::arrange(segment_id, timestamp),
                 hmm_dat_df %>%
                   rename(segment_id = ID,
                          step_km = step) %>%
                   dplyr::select(-x, -y)) %>%
  # ADD LAT AND LONG AS COLUMNS IN DATA:
  dplyr::mutate(location_long = sf::st_coordinates(.)[,1],
                              location_lat = sf::st_coordinates(.)[,2])

## 8b) SAVE STATES ####
### RDS ####
saveRDS(hmm_dat_sf, paste0("./Data/rds/Modeled/09_1d_rw_15daygap_5ptseg_HMM_3state_SFdata_",data_version,".rds"))

### EXPORT AS GEOPACKAGE ####
st_write(hmm_dat_sf, 
         paste0("./Data/Geopackage/POINTS_latlon_",data_version,".gpkg"), 
         "1d_rw_15daygap_5ptseg_HMM_3state_points", append = FALSE)


# 9) CREATE TRACKS ################################################################################
## 9a) CREATE TRACKLINES ####
daily_segmented_tracks_sf_ea <- hmm_dat_sf %>%
  st_transform(crs = sc_crs) %>%
  arrange(sc_deployment_id, timestamp) %>%
  group_by(species_code, sc_dataset_id, sc_individual_id,
           individual_local_identifier, sc_deployment_id, 
           segment_id,
           sc_tracking_data_type, movebank_study_id,
           movebank_individual_id, movebank_deployment_id) %>%
  summarize(do_union = FALSE) %>%
  st_cast("MULTILINESTRING")

## 9b) SAVE TRACKLINES ####
### RDS ####
saveRDS(daily_segmented_tracks_sf_ea, paste0("./Data/rds/Modeled/09_TRACKS_MODELED_Daily_Segmented_SF_ea_",data_version,".rds"))

### SHP ####
st_write(daily_segmented_tracks_sf_ea, paste0("./Data/shp/09_TRACKS_MODELED_Daily_Segmented_",data_version,".shp"), append = FALSE)
