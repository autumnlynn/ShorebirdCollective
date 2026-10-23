# Function to format Movebank reference columns so:
# 1) they match event data column names
# 2) timestamps are properly formatted 
# 3) columns are consistent across datasets
#
# Uses Movebank library format as of 2026-01-24
# Function does not format duty_cycle or study_site in a consistent way (keeps them as is for now)

format_movebank_reference_data <- function(df) {
  
  # GET MOVEBANK COMMENT COLUMNS THAT PROVIDE INFO ABOUT THE BIRD/DEPLOYMENT:
  comment_cols <- c(
    "deployment_end_comments",
    "deployment_comments",
    "animal_comments",
    "animal_mortality_date",
    "deployment_end_type",
    "animal_death_comments"
  )
  
  df <- df |>
    
  # ----------------
  # RENAME COLUMNS TO MATCH MOVEBANK EVENT DATA
  # ----------------
  dplyr::rename(
    dplyr::any_of(c(
      individual_taxon_canonical_name = "animal_taxon_canonical_name",
      individual_local_identifier     = "animal_local_identifier",
      individual_id                   = "animal_id"
    ))
  ) |>
    
  # ----------------
  # FORMAT TIMESTAMPS
  # ----------------
  dplyr::mutate(
    dplyr::across(
      dplyr::any_of(c("deploy_on_timestamp", "deploy_off_timestamp")),
      \(x) lubridate::ymd_hms(x, tz = "UTC")
    )
  ) |>
    
  # ----------------
  # STANDARDIZE FIELDS
  # ----------------
  dplyr::mutate(
    dplyr::across(
      dplyr::any_of("tag_manufacturer_name"),
      ~ dplyr::case_when(
        .x %in% c("Microwave", "Microwave Telemetry Inc", 
                  "Microwave Technology", "Microwave Telemtry Inc",
                  "Microwave Telemetry Inc.", "Microwave Telemetry") ~ "Microwave Telemetry",
        .x %in% c("Lotek", "Lote", " Lotek Wireless") ~ "Lotek Wireless",
        .x == "" ~ NA_character_,
        TRUE ~ .x
      )
    ),
    dplyr::across(
      dplyr::any_of("tag_model"),
      ~ dplyr::case_when(
        .x %in% c("PTT-100", "ptt-100", "Solar PTT-100") ~ "PTT-100",
        .x == "microwave telemetry" ~ NA_character_,
        .x %in% c("sunbird", "Sunbird") ~ "Sunbird",
        .x %in% c("PP-75", "Pinpoint 75") ~ "PinPoint 75",
        .x == "Pinpoint 50" ~ "PinPoint 50",
        TRUE ~ .x
      )
    ),
    dplyr::across(
      dplyr::any_of("animal_sex"),
      ~ dplyr::case_when(
        .x == "" ~ NA_character_,
        TRUE ~ .x
      )
    )
  )
  
  # ----------------
  # CLEAN EMPTY / WHITESPACE STRINGS IN COMMENT COLUMNS
  # ----------------
  existing_cols <- base::intersect(comment_cols, names(df))
  if (length(existing_cols) > 0) {
    df <- df |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(existing_cols),
          ~ {
            x <- trimws(.x)       # remove spaces / tabs
            dplyr::na_if(x, "")   # convert empty strings to NA
          }
        )
      ) |>
    # ----------------
    # UNITE COMMENT COLUMNS
    # ----------------
    tidyr::unite(
      movebank_ref_comments,
      dplyr::all_of(existing_cols),
      sep = "; ",
      na.rm = TRUE,
      remove = TRUE
    )
  }
  
  return(df)
}

# TO CHECK INDIVIDUAL COLUMNS:
# check_lst <- ref_dat_list_sub %>%
#   purrr::keep(~ "animal_death_comments" %in% names(.x)) %>%   # keep only dfs with that column
#   purrr::map(~ dplyr::filter(.x, !is.na(.data$animal_death_comments))) %>%
#   purrr::keep(~ nrow(.x) > 0)                      # drop empty dfs
# 
# check_lst %>%
#   purrr::map(~.x %>% distinct(animal_death_comments))
# 
# check_lst %>%
#   purrr::map_dfr(~.x %>% select(animal_death_comments) %>% as.data.frame()) %>%
#   distinct(animal_death_comments)