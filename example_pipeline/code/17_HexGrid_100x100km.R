# CODE NAME: 17_HexGrid_100x100km.R
# CODE PURPOSE: Overlay a hexagonal grid over the data...
# And calculate summary statistics: number of unique individuals per cell
# 100km x 100km


# 0) SET WORKING DIRECTORY ##############################
setwd()


# 1) LOAD PACKAGES #########################
library(tidyverse)
library(sf)
library(rnaturalearth)
library(viridis)
# Set map projection
sc_crs <- "+proj=laea +lon_0=-101.6 +lat_0=19.22 +datum=WGS84 +units=m +no_defs"


# 2) LOAD CLEANED MODELED DATA #######################
points_sf <- readRDS("./Data/Modeled/15_MODELED_SF_META.rds")


# 3) MAKE HONEYCOMB GRID ##############################
#crs in units: meters so cell size is in meters
area_honeycomb_grid = st_make_grid(points_sf,
                                   cellsize = c(100000, 100000), #~100km by 100km
                                   what = "polygons", 
                                   square = FALSE)
# CONVERT TO SF:
# To sf and add grid ID
honeycomb_grid_sf = st_sf(area_honeycomb_grid) %>%
  # add grid ID
  mutate(grid_id = 1:length(lengths(area_honeycomb_grid)))


# 4) JOIN POINTS WITH POLYGONS ####
# Assign grid ID to the points:
joined <- points_sf %>% # spatial join only
  st_join(honeycomb_grid_sf) 


# 5) CALCULATE COUNTS ###################################
## 5a) NUMBER OF INDIVIDUALS PER SPECIES PER GRID CELL ####
count_grid_sp_ind <- joined %>% 
  st_drop_geometry() %>% 
  group_by(grid_id, common.name) %>%  
  distinct(sc.individual.id) %>% # individuals per species per grid cell
  count() %>% 
  arrange(grid_id, common.name) %>% 
  rename(n_individuals = n)

## 5b) NUMBER SP per CELL ####
sp_count <- count_grid_sp_ind %>% 
  filter(common.name != "Unknown Dowitcher") %>%
  group_by(grid_id) %>% 
  count() %>%
  rename(n_species = n)

## 5c) FILL ALL IN SP IND WITH 0s ####
count_grid_sp_ind_complete <-  count_grid_sp_ind %>% 
  ungroup() %>%
  complete(grid_id = 1:nrow(honeycomb_grid_sf), common.name, 
           fill = list(n_individuals = 0))
# could pivot wider here and get counts of individuals per species

## 5d) SPECIES COUNTS (CONVERT WIDE) ####
sp_count_wide <- count_grid_sp_ind_complete %>%
  pivot_wider(names_from = common.name, 
              values_from = n_individuals)

## 5e) COUNT *AVG* & *MEDIAN* NUMBER OF INDIVIDUALS PER SPECIES PER GRID CELL ####
grid_sp_ind_summary <- count_grid_sp_ind_complete %>% 
  group_by(grid_id) %>%
  summarise(median_n_ind_per_sp = median(n_individuals), 
            mean_n_ind_per_sp = mean(n_individuals),
            n_inds_total = sum(n_individuals))


# 6) ADD COUNTS BACK INTO SF ###########################
## 6a) ADD MEAN/MEDIAN IND PER SP ####
honeycomb_grid_sf <- left_join(honeycomb_grid_sf, grid_sp_ind_summary) #joining by grid id

## 6b) ADD IN SP COUNT PER CELL ####
honeycomb_grid_sf <- left_join(honeycomb_grid_sf, sp_count) #joining by grid_id
# Replace NA with zero for sp_count
honeycomb_grid_sf <- honeycomb_grid_sf %>% 
  mutate(n_species = ifelse(is.na(n_species), 0, n_species))

## 6c) ADD SP COUNTS ####
honeycomb_grid_sf <- left_join(honeycomb_grid_sf, sp_count_wide) # Joining, by = "grid_id"


# 7) REMOVE GRID VALUES OF 0 ########################
# remove grid without any individuals: value of 0 ((i.e. no points inside that grid))
honeycomb_no0_inds = filter(honeycomb_grid_sf, n_inds_total > 0)
honeycomb_no0_sp = filter(honeycomb_grid_sf, n_species > 0)


# 8) PLOT # SPECIES ##########################################################
## 8a) LOAD LAND SPATIAL DATA ####
world_sf <- ne_countries(continent = c("North America", "South America", "Asia", "Europe","Oceania", "Seven seas (open ocean)", "Antarctica", "Africa"), 
                      scale = 50, returnclass = "sf") %>%
  st_transform(crs = sc_crs) # same equal area projection


wh_sf <- ne_countries(continent = c("North America", "South America"), 
                      scale = 50, returnclass = "sf") %>%
  st_transform(sc_crs)

## 8b) PLOT SP RICHNESS ####
# Get legend scale breaks:
break0 <- min(honeycomb_no0_sp$n_species)
break3 <- max(honeycomb_no0_sp$n_species)
break2 <- round(max(honeycomb_no0_sp$n_species)/3, digits = 0)
break1 <- round((max(honeycomb_no0_sp$n_species)/3)*2, digits = 0)

# Make plot:
sprich_plot <- ggplot() +
  geom_sf(data = world_sf, fill = "lightgrey") +
  geom_sf(data = honeycomb_no0_sp, aes(fill = n_species ), lwd = 0) + 
  scale_fill_viridis_c(alpha = .4, breaks = c(break0, break1, break2, break3)) +
  theme_bw() +
  coord_sf(xlim = st_bbox(wh_sf)[c(1,3)], ylim = st_bbox(wh_sf)[c(2,4)])+
  labs(fill =' number of species per cell') +
  theme(legend.position = "bottom",
        legend.key.width = unit(0.35, 'in'))
sprich_plot

## 8c) SAVE SPECIES FIGS ####
# SAVE AS PNG:
ggsave("./Maps/17_hexgrid_spcount.png",
       height = 11,
       width = 8.5,
       units = "in",
       dpi = 350)

# SAVE AS RDS :
saveRDS(sprich_plot, "./Maps/rds/17_hexgrid_spcount.rds")


# 9) PLOT # IND SHOREBIRDS OVERALL ############################
## 9a) MAKE PLOT ####
ind_plot <- ggplot() +
  geom_sf(data = world_sf, fill = "lightgrey") +
  geom_sf(data = honeycomb_no0_inds, aes(fill = n_inds_total), lwd = 0) + 
  scale_fill_viridis_c(alpha = .4) +
  theme_bw() +
  coord_sf(xlim = st_bbox(wh_sf)[c(1,3)], ylim = st_bbox(wh_sf)[c(2,4)])+
  labs(fill =' number of individuals per cell') +
  theme(legend.position = "bottom",
        legend.key.width = unit(0.75, 'in'))
ind_plot

## 9b) SAVE FIGS ####
# SAVE AS PNG:
ggsave("./Maps/17_hexgrid_individ_count.png",
       height = 11,
       width = 8.5,
       units = "in",
       dpi = 600)

# SAVE AS RDS:
saveRDS(ind_plot, "./Maps/rds/17_hexgrid_indcount.rds")


# 10) SAVE HONEYCOMB DATA #############################
### RDS:
saveRDS(honeycomb_grid_sf, "./Data/17_summarized_hexgrid_dat_100km.rds")
#honeycomb_grid_sf <- readRDS("./Data/summarized_hexgrid_dat_2023_10_10.rds")

### SHP:
st_write(honeycomb_grid_sf, "./Data/Calculated/shp/17_summarized_hexgrid_dat_100km.shp", append = FALSE)

