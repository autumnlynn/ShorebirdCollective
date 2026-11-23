# CODE: 08_PointsToTracks_Origts.R
# CODE PURPOSE: Convert SF data of predicted points to tracks ####
# Worked off this example here: https://joshcullen.github.io/bayesmove/articles/Exploratory-analysis-in-Shiny
#
# Can use this code to create track lines from predicted points at the original timestep, daily locations, or whatever time interval you predicted
# Here we are using the original timestep data

# 00) SETUP ####
data_version <- "2026-01-24"

## SET WORKING DIRECTORY ##############################
setwd()
Sys.setenv(TZ = 'UTC')


# 1) LOAD PACKAGES ###############################
library(tidyverse)
library(sf)
sf_use_s2(FALSE) 
library(rnaturalearth)
library(lubridate)
library(viridis)
sc_crs <- "+proj=laea +lon_0=-101.6 +lat_0=19.22 +datum=WGS84 +units=m +no_defs" # custom equal area projection centered on the data


# 2) LOAD MODELED DATA #####################################################
plocs_sf_gps <- readRDS(paste0("./Data/rds/Modeled/07_PREDICTED_Origts_SF_gps_",data_version,".rds"))
plocs_sf_argosgps <- readRDS(paste0("./Data/rds/Modeled/07_PREDICTED_Origts_SF_argosgps_",data_version,".rds"))
plocs_sf_argos <- readRDS(paste0("./Data/rds/Modeled/07_PREDICTED_Origts_SF_argos_",data_version,".rds"))


# 3) FORMAT MODELED DATA ##################################################
# 3a) UNLIST INTO ONE SF DATAFRAME ####
plocs_single_sf <- dplyr::bind_rows(plocs_sf_gps, plocs_sf_argosgps,plocs_sf_argos) # Note, only works if data are the same projection (all are bc consistent animotum world mercator output) 

## 3b) CONVERT TO LAT/LONG FOR CONSISTENCY ####
plocs_single_sf <- plocs_single_sf %>%
  st_transform(crs = "EPSG:4326")
rm(plocs_sf_gps, plocs_sf_argosgps,plocs_sf_argos)

## 3c) SAVE MODELED DATA POINTS ####
### RDS ####
saveRDS(plocs_single_sf, paste0("./Data/rds/Modeled/08_POINTS_MODELED_Origts_SF_",data_version,".rds"))

### GEOPACKAGE ####
st_write(plocs_single_sf, 
         paste0("./Data/Geopackage/POINTS_latlon_",data_version,".gpkg"), 
         "modeled_origts_points", append = FALSE)


# 4) CONVERT POINTS TO TRACKS ################################
tracks_predicted_origts_sf_ea <- plocs_single_sf %>%
  st_transform(crs = sc_crs) %>% 
  arrange(sc_deployment_id, timestamp) %>%
  group_by(species_code, sc_dataset_id, sc_individual_id,
           individual_local_identifier, sc_deployment_id, 
           sc_tracking_data_type, movebank_study_id,
           movebank_individual_id, movebank_deployment_id) %>%
  summarize(do_union = FALSE) %>%
  st_cast("MULTILINESTRING")


# 5) PLOT TRACKS ###############################################################
# 5a) LOAD LAND SPATIAL DATA ####
world_sf <- ne_countries(continent = c("North America", "South America", "Europe", "Asia", "Africa", "Australia"), 
                         scale = 50, returnclass = "sf") %>%
  st_transform(sc_crs)

wh_sf <- ne_countries(continent = c("North America", "South America"), 
                      scale = 50, returnclass = "sf") %>%
  st_transform(sc_crs)

# 5b) PLOT STATIC TRACKS ####
ggplot() +
  geom_sf(data = world_sf, fill = "lightgrey") +
  geom_sf(data = tracks_predicted_origts_sf_ea, aes(color = species_code), size = .5, alpha = 0.2) + 
  scale_color_viridis(discrete = TRUE, option = "turbo", direction = -1) +
  guides(color = guide_legend(title = "Species"))+
  theme_bw() +
  coord_sf(xlim = st_bbox(wh_sf)[c(1,3)], ylim = st_bbox(wh_sf)[c(2,4)])+
  ggtitle(paste0("Shorebird Collective Data ",data_version))

## 5c) SAVE PLOT ####
ggsave(paste0("./Maps/png/08_modeled_TRACKS_Origts_map_ea_",data_version,".png"),
       width = 11, height = 8.5, units = "in",
       dpi = 600)

## 5d) SAVE TRACK DATA ####
# SAVING AS SHP because geopackage doesn't like custom specified projections
# Note, saving as shape will shorten the column names to abbreviations
### RDS ####
saveRDS(tracks_predicted_origts_sf_ea, paste0("./Data/rds/Modeled/08_TRACKS_MODELED_Origts_SF_ea_",data_version,".rds"))

## 5e) SHP ####
st_write(tracks_predicted_origts_sf_ea, paste0("./Data/shp/08_TRACKS_MODELED_Origts_",data_version,".shp"), append = FALSE)
