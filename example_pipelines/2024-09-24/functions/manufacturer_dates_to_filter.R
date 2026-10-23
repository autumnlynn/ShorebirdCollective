# CODE: manufacturer_dates_to_filter.r

# This code finds tracked birds within 1 km of a specific location (location of a manufacture facility)..
# Get points for that bird within 50 km of that location (and obtain dates for those)
# Creates a dataframe with rows of dates per bird to filter out
# These dates are within 10 days of the start being detected at the manufacturer 
# This 10 day period is because tags tested at Microwave telemetry were tested for less than a week
# These dates are then flagged as a column in the dataset


source("./Functions/split_at_gap_scdepid.R")
source("./Functions/named_group_split.r") 

# SMALLER FUNCTION USED LATER:
fill_dates <- function(df) {
  tibble(sc.deployment.id = df$sc.deployment.id, 
         date = seq(as.Date(df$start), 
                    as.Date(df$end), by = "1 day"))
}

# INPUTS:
# SF POINT OF A MANUFACTURER LOCATION IN LAEA PROJECTION:
# "+proj=laea +lon_0=-101.6 +lat_0=19.22 +datum=WGS84 +units=m +no_defs"
# event_data in a list per project (should update the "any.failed.sc.filters" column before running this)


manuf_dates_to_filter <- function(point_to_buffer, event_data_list, project_crs) {
  # CREATE BUFFERS AROUND MANUFACTURER LOCATION:
  buffered_1km <- point_to_buffer %>% sf::st_buffer(1000)
  buffered_50km <- point_to_buffer %>% sf::st_buffer(50000) 
  
  # CONVERT EVENT DATA TO SF & AND KEEP ONLY THOSE THAT PASSED FILTERS SO FAR:
  event_sf_passed_lst <- event_data_list %>% 
    map(~.x %>% filter(!any.failed.sc.filters == "TRUE") %>%
          select(individual.taxon.canonical.name, sc.deployment.id, event.id, location.long, location.lat, timestamp, orig.dep.seq.id, detection.month) %>%
          sf::st_as_sf(coords = c("location.long", "location.lat"), crs = 4326) %>%
          st_transform(crs = st_crs(project_crs)))
  
  # INTERSECT EACH PROJECT'S POINTS WITH 1km BUFFER:
  # To find points withn 1km of location
  points_in_poly <- event_sf_passed_lst %>% 
    map(~st_filter(.x, buffered_1km))
  
  # GET DEP IDs of BIRDS IN POLY:
  bird_IDs <- points_in_poly %>% map_df(~.x %>% st_drop_geometry() %>%
                                                select(sc.deployment.id) %>% distinct()) %>%
    pull()
  
  # GET ALL POINTS WITHIN 50 KM OF MANUFACTURER: 
  # Because mapping showed some detections from those birds went so far
  # And want to find the dates of those
  points_50km <- event_sf_passed_lst %>% 
    map(~st_filter(.x, buffered_50km))
  
  # SUBEST TO BIRDS DETECTED WITHIN 1 KM:
  points_50km_birds_1km <- points_50km %>% map_dfr(~ .x %>% 
                                             st_drop_geometry() %>%
                                             filter(sc.deployment.id %in% bird_IDs) %>% # only birds detected within a 1km
                                             group_by(sc.deployment.id) %>%
                                             arrange(timestamp) %>% 
                                             ungroup() %>%
                                             select(sc.deployment.id, timestamp))
  
  # SPLIT DETECTIONS INTO SEPARATE LIST PER BIRD:
  # To group dates into different bundles of times the tag was at the manufacturer
  points_50km_per_bird_lst <- named_group_split(points_50km_birds_1km, sc.deployment.id)
  
  # ASSIGN EACH BIRD'S DETECTIONS TO GROUPS (SEPARATED EVERY 30 DAYS):
  # Segmented every 30 days
  # Currently segmenting code is in minutes
  #30*24*60 # 30 days  = 43200 minutes
  point_groups_50km <- points_50km_per_bird_lst %>% 
    map_dfr(~split_at_gap_depid(.x, max_gap = 43200, shortest_track = 0))
  
  # GET IDS THAT AREN'T SINGLE OBSERVATIONS WITHIN 50km:
  # Single obs are likely flyovers
  nonsingle_obs_segIDs <- point_groups_50km %>% 
    group_by(segmentID) %>%
    count() %>% filter(n > 1) %>%
    distinct(segmentID) %>% pull()
  
  # REMOVE THE SINGLE OBS AND FORMAT DATA 
  manuf_date_ranges <- point_groups_50km %>% 
    filter(segmentID %in% nonsingle_obs_segIDs) %>% 
    group_by(segmentID) %>%
    slice_head() %>% 
    ungroup() %>%
    select(sc.deployment.id, timestamp, segmentID) %>%
    mutate(start = date(timestamp),
           end = start + 10) %>% # chose 10 days afterwards (all we observed were a week or less)
    select(-timestamp) 
  
  # CREATE SEPARATE LIST PER SEGMENT/POINT GROUP:
  filter_ranges_lst <- named_group_split(manuf_date_ranges, segmentID)
  
  # FILL DATES BETWEEN START AND END PER BIRD AND CREATE DF:
  filled_dates <- filter_ranges_lst %>% map_dfr(~ fill_dates(.x))
  
  # ADD FILTER COLUMN:
  filled_dates <- filled_dates %>% mutate(filter.manufacturer.dates = 0)
  
  return(filled_dates)
}

