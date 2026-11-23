# CODE: 10_HexGrids.r
# CODE PURPOSE: CREATE HEXAGONAL GRIDS OF THE DATA AT MULTIPLE RESOLUTIONS]
# Creating for the original timestep data as an example


# 00) SETUP ####
data_version <- "2026-01-24"


## SET WORKING DIRECTORY ##############################
setwd()
Sys.setenv(TZ = 'UTC')


# 1) LOAD PACKAGES #########################
library(tidyverse)
library(sf)
library(rnaturalearth)
library(viridis)
sc_crs <- "+proj=laea +lon_0=-101.6 +lat_0=19.22 +datum=WGS84 +units=m +no_defs"

## LOAD CUSTOM FUNCTIONS ####
source("./Code/Functions/make_hemispheric_hexgrids.R")


# 2) LOAD DATA #############################
## 2a) ORIGINAL TS DATA ####
plocs_single_sf <- readRDS(paste0("./Data/rds/Modeled/08_POINTS_MODELED_Origts_SF_",data_version,".rds")) %>%
  st_transform(crs = sc_crs)


# 3) MAKE 100KM GRIDS AND MAPS ##################
## 3a) MAKE GRIDS ####
# WITH HEMISPHERE BASEMAP
grids_100km_lst <- make_hemispheric_hexgrids(points_sf = plocs_single_sf,
                                         crs = sc_crs, cell_size_m = 100000, 
                                         date_string = data_version)

## 3b) PLOT THE MAPS TO CHECK ####
### SPECIES:
grids_100km_lst$sprich_plot

### INDIVIDUALS:
grids_100km_lst$ind_plot

## 3c) SAVE FIGURES ####
### SPECIES COUNT ####
#### PNG ####
ggsave(plot = grids_100km_lst$sprich_plot,
       filename = paste0("./Maps/png/hexgrid_spcount_100km_",data_version,".png"),
       height = 11,
       width = 8.5,
       units = "in",
       dpi = 350)

#### RDS ####
saveRDS(grids_100km_lst$sprich_plot, paste0("./Maps/rds/hexgrid_spcount_100km_",data_version,".rds"))

### INDIVIDUAL COUNT ####
#### PNG ####
ggsave(plot = grids_100km_lst$ind_plot,
       filename = paste0("./Maps/png/hexgrid_indcount_100km_",data_version,".png"),
       height = 11,
       width = 8.5,
       units = "in",
       dpi = 350)

#### RDS ####
saveRDS(grids_100km_lst$ind_plot, paste0("./Maps/rds/hexgrid_indcount_100km_",data_version,".rds"))

## 3d) SAVE HONEYCOMB DATA ####
### RDS ####
saveRDS(grids_100km_lst$honeycomb_no0_inds_sf, paste0("./Data/rds/Modeled/10_summarized_hexgrid_dat_100km_",data_version,".rds"))

### SHP ####
st_write(grids_100km_lst$honeycomb_no0_inds_sf, paste0("./Data/shp/10_summarized_hexgrid_dat_100km_",data_version,".shp"), append = FALSE)
rm(grids_100km_lst)


# 4) MAKE 30 KM GRIDS AND MAPS ##################
## 4a) MAKE GRIDS ####
# WITH HEMISPHERE BASEMAP
grids_30km_lst <- make_hemispheric_hexgrids(points_sf = plocs_single_sf,
                                             crs = sc_crs, cell_size_m = 30000, 
                                             date_string = data_version)

## 4b) PLOT THE MAPS TO CHECK ####
### SPECIES:
grids_30km_lst$sprich_plot

### INDIVIDUALS:
grids_30km_lst$ind_plot

## 4c) SAVE FIGURES ####
### SPECIES COUNT ####
#### PNG ####
ggsave(plot = grids_30km_lst$sprich_plot,
       filename = paste0("./Maps/png/hexgrid_spcount_30km_",data_version,".png"),
       height = 11,
       width = 8.5,
       units = "in",
       dpi = 350)

#### RDS ####
saveRDS(grids_30km_lst$sprich_plot, paste0("./Maps/rds/hexgrid_spcount_30km_",data_version,".rds"))

### INDIVIDUAL COUNT ####
#### PNG ####
ggsave(plot = grids_30km_lst$ind_plot,
       filename = paste0("./Maps/png/hexgrid_indcount_30km_",data_version,".png"),
       height = 11,
       width = 8.5,
       units = "in",
       dpi = 350)

#### RDS ####
saveRDS(grids_30km_lst$ind_plot, paste0("./Maps/rds/hexgrid_indcount_30km_",data_version,".rds"))

## 4d) SAVE HONEYCOMB DATA ####
### RDS ####
saveRDS(grids_30km_lst$honeycomb_no0_inds_sf, paste0("./Data/rds/Modeled/10_summarized_hexgrid_dat_30km_",data_version,".rds"))

### SHP ####
st_write(grids_30km_lst$honeycomb_no0_inds_sf, paste0("./Data/shp/10_summarized_hexgrid_dat_30km_",data_version,".shp"), append = FALSE)
rm(grids_30km_lst)

