# CODE: 05_Calculate_Sample_Sizes.R
# CODE PURPOSE: Calculate sample sizes for downloaded and merged data

setwd()
Sys.setenv(TZ = 'UTC')


# 1) LOAD PACKAGES #################################################
library(tidyverse)


# 2) LOAD DATA #########################################
## 2a) ALL EVENT ####
event_dat_all <-  readRDS("./Data/Merged/04_all_event_dat.rds")

## 2b) ALL REFERENCE ####
ref_dat_all <- readRDS("./Data/Merged/04_all_refdat.rds")


# 3) COUNTS BY DATA TYPE ################################################
## 3a) OVERALL COUNTS ####
event_dat_all %>% count() # NUMBER OBSERVATIONS
event_dat_all %>% distinct(sensor.types.detected) # doesn't download the other sensor data unless it's associated with a location, but could using the movebank download options in script 01
event_dat_all %>% # NUMBER INDIVIDUALS BY SPECIES
  dplyr::select(sc.individual.id, species.code) %>%
  distinct() %>%
  group_by(species.code) %>%
  count() %>%
  arrange(species.code) %>%
  print(n = Inf)

## 3b) # OF INDIVIDUALS ####
tot_inds <- event_dat_all %>% 
  distinct(sc.individual.id) %>% 
  count() %>% pull() #argos/gps tags

## 3c) # OF SPECIES CONTRIBUTED ####
event_dat_all %>% 
  filter(species.code != "UNDO") %>%
  distinct(species.code) %>%
  count() 

## 3d) INDIVIDUALS BY SENSOR TYPE DETECTED ####
event_dat_all %>%
  dplyr::select(sc.individual.id, sensor.types.detected) %>% 
  distinct() %>% 
  group_by(sensor.types.detected) %>% 
  count() 

# BY SENSOR TYPES:
event_dat_all %>%
  dplyr::select(sc.individual.id, sensor.type) %>% 
  distinct() %>% 
  group_by(sensor.type) %>% 
  count() %>%
  mutate(percent_ind = round(n/tot_inds*100, digits = 1))

## 3e) SPECIES BY SENSOR TYPE DETECTED ####
tot_sp_count <- 36 # manual count including geolocator and motus
### ARGOS:
event_dat_all %>%
  filter(str_detect(sensor.types.detected, 'Argos') & 
           species.code != "UNDO") %>%
  dplyr::select(species.code) %>% 
  distinct() %>% 
  count() %>%
  mutate(percent_sp = round(n/tot_sp_count*100, digits = 1))

### GPS:
event_dat_all %>%
  filter(str_detect(sensor.types.detected, 'GPS') & 
           species.code != "UNDO") %>%
  dplyr::select(species.code) %>% 
  distinct() %>% 
  count() %>%
  mutate(percent_sp = round(n/tot_sp_count*100, digits = 1))


# 4) CALCULATE OPEN ACCESS LICENCE AGREEMENTS ######################################################
## 4a) COUNT INDS BY LICENCE TYPE ####
ref_dat_all %>% 
  group_by(license.type) %>%
  count() # NAs are those read in "Direct Send" because those don't have a license and are strictly under DSA terms

## 4b) COUNT INDS WITH OPEN ACCESS LICENCE ####
ref_dat_all %>%
  filter(str_detect(license.type, "CC")) %>% 
  count()

# CALCULATE % of TOTAL:
round(ref_dat_all %>%
  filter(str_detect(movebank.license.id, "CC")) %>%
  count() %>%
  pull()/tot_inds*100, digits = 1) 

## 4c) WHAT SPECIES HAVE OPEN ACCESS DATA ####
ref_dat_all %>% 
  filter(str_detect(movebank.license.id, "CC")) %>%
  distinct(species.code) 

# COUNT & %:
round(ref_dat_all %>% 
        filter(str_detect(movebank.license.id, "CC")) %>%
        distinct(species.code) %>%
        count() %>%
        pull()/tot_sp_count*100, digits = 1)


# 5) CALCULATE % INDIVIDUALS/SPECIES WITH PREAPPROVALS #############################
## 5a) CALCULATE PROPORTION INDS WITH PREAPPROVALS FOR CONSERVATION APPLICATIONS ####
# These are part of the SC data sharing agreement metadata
cons_dsa_counts <- ref_dat_all %>%
  group_by(dsa.conservation) %>%
  count() 

# SUM TOTALS AND CALC%:
cons_dsa_counts <- cons_dsa_counts %>%
  mutate(n_total = n, 
         percent_ind = round(n_total/tot_inds*100, digits = 1))
cons_dsa_counts

## 5b) CALCULATE PROPORTION SPECIES WITH PREAPPROVALS ####
ref_dat_all %>%
  filter(dsa.conservation == "yes_approve" & species.code != "UNDO") %>%
  distinct(species.code) %>%
  arrange(species.code)

# COUNT:
preapproval_counts <- ref_dat_all %>%
  filter(dsa.conservation == "yes_approve" & species.code != "UNDO") %>%
  distinct(species.code) %>%
  count() 

# CALC %s:
round(preapproval_counts <- ref_dat_all %>%
        filter(dsa.conservation == "yes_approve" & species.code != "UNDO") %>%
        distinct(species.code) %>%
        count() %>% 
        pull()/tot_sp_count*100, digits = 1)


# 6) CALCULATE PROPORTION DATA DIRECT SEND VS MOVEBANK ##############################################
dsa_receipt_counts <- ref_dat_all %>%
  group_by(data.receipt.type) %>%
  count() 
dsa_receipt_counts

# SUM TOTALS AND CALC%:
dsa_receipt_counts <- dsa_receipt_counts %>%
  mutate(n_total = n, 
         percent_ind = round(n_total/tot_inds*100, digits = 1))
dsa_receipt_counts

# SPECIES COUNTS MOVEBANK:
ref_dat_all %>%
  filter(data.receipt.type == "Movebank" & species.code != "UNDO") %>%
  distinct(species.code) %>%
  arrange(species.code) 

round(ref_dat_all %>%
  filter(data.receipt.type == "Movebank" & species.code != "UNDO") %>%
  distinct(species.code) %>%
  count() %>% 
  pull()/tot_sp_count*100, digits = 1)
