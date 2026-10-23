# ADD A COLUMN THAT FLAGS IF A ROW FAILED ANY FILTERS SO FAR
# 0 = FAILED A FILTER
# 1 = PASSED A FILTER
# Output gives TRUE (failed at least one filter) or FALSE (did not fail any filters)

flag_failed_sc_filters <- function(df) {
  
  df <- df |>
    dplyr::mutate(
      any_failed_sc_filters = if_any(starts_with("filter_"), ~ . == 0)
    )
  
  return(df)
}
