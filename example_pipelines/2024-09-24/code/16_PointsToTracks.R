# CODE: 16_PointsToTracks.R
# CODE PURPOSE: Convert SF data of points to tracks ####


# 0) SET WD #################################
setwd()
Sys.setenv(TZ = 'UTC')


# 1) LOAD PACKAGES ###############################
library(tidyverse)
library(sf)
sf_use_s2(FALSE) 
library(rnaturalearth)
library(lubridate)
library(viridis)
# Specify map projection:
sc_crs <- "+proj=laea +lon_0=-101.6 +lat_0=19.22 +datum=WGS84 +units=m +no_defs"


# 2) LOAD DATA ############################################
## 2a) POINTS IN EA PROJECTION ####
points_sf <- readRDS("./Data/Modeled/15_MODELED_SF_META.rds")

## 2b) REFERENCE DATA ####
refdat_lst <- readRDS("./Data/Cleaned/13_refdat_DC_DEPS_ENDTYPE.rds")


# 3) CONVERT POINTS TO TRACKS ################################
tracks_sf <- points_sf %>%
  group_by(sc.deployment.id) %>%
  summarize(do_union = FALSE) %>%
  st_cast("MULTILINESTRING")


# 4) CALCULATE TRACK SUMMARY STATS ############################################
## 4a) # OF DETECTIONS PER TRACK ####
n_counts_df <- points_sf %>% 
  st_drop_geometry() %>%
  group_by(sc.deployment.id) %>% 
  count() %>% rename(n.detections = n)

## 4b) TRACK DURATION ####
track_length_df <- points_sf %>% 
   st_drop_geometry() %>%
   group_by(sc.deployment.id) %>%
   arrange(timestamp) %>% 
   filter(row_number() == 1 | row_number()==n()) %>% # get first and last obs per individual
   mutate(seq.id = row_number(),
         track.duration.days = 
           round(as.numeric(difftime(timestamp, lag(timestamp, default = first(timestamp)), units = "days")), 0)) %>%
  select(sc.deployment.id, seq.id, track.duration.days) %>% 
  arrange(sc.deployment.id, seq.id) %>% 
  filter(seq.id == 2) %>% select(-seq.id)

## 4c) TRACK START AND END DATES ####
track_start_and_end_df <- points_sf %>% 
  st_drop_geometry() %>%
  group_by(sc.deployment.id) %>%
  arrange(timestamp) %>% 
  filter(row_number() == 1 | row_number() == n()) %>% # get first and last obs per individual
  mutate(seq.id = row_number()) %>%
  ungroup() %>% select(sc.deployment.id, seq.id, timestamp) %>%
  tidyr::pivot_wider(names_from = seq.id, values_from = timestamp) %>%
  rename(track.start.time =`1`,
         track.end.time = `2`)

## 4d) COMBINE TRACK METADATA ####
track_sum_df <- left_join(n_counts_df, track_length_df) #Joining with `by = join_by(sc.deployment.id)`
track_sum_df <- left_join(track_sum_df, track_start_and_end_df) ##Joining with `by = join_by(sc.deployment.id)`
rm(n_counts_df, track_length_df, track_start_and_end_df)


# 5) ADD ASSOCIATED REFERENCE DATA ##########################################
## 5a) METADATA ALREADY JOINED WITH POINTS ####
# To add to tracklines
track_meta_df <- points_sf %>% 
  st_drop_geometry() %>%
  select(-timestamp, 
         -x.se, -y.se, 
         -fg.modeled.seq.id, 
         -(starts_with("detection.")), 
         -days.since.first.detection,
         -estimated.duty.cycle.days,
         -estimated.duty.cycle.hrs,
         -estimated.duty.cycle.mins,
         -deployment.location.provided,
         -deployment.location.origin,
         -first.last.detection,
         -deployment.location.yn,
         -post.am.removal.yn,
         -location.lat,
         -location.lon) %>%
  distinct()

## 5b) DEPLOYMENT LOCATIONS ####
# To add to trackline information for quick summary info
dep_locs_ll_sf <- points_sf %>%
  filter(deployment.location.yn == "yes") %>%
  select(sc.dataset.id, species.code, sc.deployment.id, geometry, deployment.location.origin, sc.dataset.id, dsa.id) %>% # add common.name back
  distinct() %>% 
  st_transform(crs = "EPSG:4326") %>%
  dplyr::mutate(location.longitude = round(unlist(purrr::map(.data$geometry, 1)), digits = 5), #add x and y back into dataframe
                location.latitude = round(unlist(purrr::map(.data$geometry, 2)), digits = 5)) %>%
  unite("deployment.coordinates", 
        c("location.latitude", "location.longitude"), sep = " , ", na.rm = TRUE, remove = FALSE)

## 5c) DEP VARIABLES TO ADD TO TRACKS ####
dep_add_df <- dep_locs_ll_sf %>%
  st_drop_geometry() %>%
  select(dsa.id, sc.dataset.id, sc.deployment.id, 
         deployment.coordinates, deployment.location.origin) 

## 5d) ADD DEPLOYMENT METADATA TO TRACKS ####
track_meta_df <- left_join(track_meta_df, dep_add_df) #Joining with `by = join_by(sc.deployment.id)`

## 5e) JOIN META WITH TRACKS ####
tracks_sf <- left_join(tracks_sf, track_meta_df) #Joining with `by = join_by(sc.deployment.id)`

# CLEAN UP #### 
rm(dep_locs_ll_sf, dep_add_df, dep_locs_ea_sf)


# 6) PLOT TRACKS ###############################################################
## 6a) LOAD LAND SPATIAL DATA ####
world_sf <- ne_countries(continent = c("North America", "South America", "Europe", "Asia", "Africa", "Australia"), 
                      scale = 50, returnclass = "sf") %>%
  st_transform(sc_crs)

wh_sf <- ne_countries(continent = c("North America", "South America"), 
                      scale = 50, returnclass = "sf") %>%
  st_transform(sc_crs)

## 6b) PLOT STATIC TRACKS ####
ggplot() +
  geom_sf(data = world_sf, fill = "lightgrey") +
  geom_sf(data = tracks_sf, aes(color = species.code), size = .5, alpha = 0.2) + #update to common.name
  scale_color_viridis(discrete = TRUE, option = "viridis", direction = -1) +
  guides(color = guide_legend(title = "Species"))+
  theme_bw() +
  coord_sf(xlim = st_bbox(wh_sf)[c(1,3)], ylim = st_bbox(wh_sf)[c(2,4)])+
  ggtitle("Shorebird Collective Data")

## 6c) SAVE PLOT ####
ggsave("./Maps/PointsToTracks_map.png",
       width = 11, height = 8.5, units = "in",
       dpi = 600)

## 6d) SAVE TRACK DATA ####
### RDS:
saveRDS(tracks_sf, "./Data/Modeled/16_MODELED_TRACKS.rds")


# 7) FORMAT FOR ARCPRO ############################################################
## 7a) FORMAT ####
tracks_sf_ap <- tracks_sf %>%
  mutate(study.id = as.character(study.id),
         deployment.id = as.character(deployment.id)) %>%
  rename(movebank.deployment.id = deployment.id,
         movebank.study.id = study.id) %>%
  select(-c(deployment.on.time.available,
            deployment.off.time.available)) %>%
  rename_with(., .fn = ~ snakecase::to_lower_camel_case(.)) 

## 7b) SAVE DATA ####
### SHP:
st_write(tracks_sf_ap, "./Data/Modeled/shp/16_MODELED_TRACKS.shp", append = FALSE)
