# Function to make hemispheric gridded maps for species and individuals and a specific grid size
# Requires projection crs in meters (input point data must already be in that projection)
# Default hex grid = 30km by 30km
# Defaults to a western hemisphere basemap

make_hemispheric_hexgrids <- function(points_sf, crs, cell_size_m = 30000, date_string = NULL) {
  
  # 1) GRID ----------------------------------------------------------
  area_honeycomb_grid <- sf::st_make_grid(
    points_sf,
    cellsize = c(cell_size_m, cell_size_m),
    what = "polygons",
    square = FALSE
  )
  
  honeycomb_grid_sf <- sf::st_sf(area_honeycomb_grid) |>
    dplyr::mutate(grid_id = seq_along(area_honeycomb_grid))
  
  # 2) JOIN ----------------------------------------------------------
  joined <- points_sf |>
    sf::st_join(honeycomb_grid_sf)
  
  # 3) COUNTS --------------------------------------------------------
  count_grid_sp_ind <- joined |>
    sf::st_drop_geometry() |>
    dplyr::group_by(grid_id, species_code) |>
    dplyr::distinct(sc_individual_id) |>
    dplyr::count() |>
    dplyr::rename(n_individuals = n)
  
  sp_count <- count_grid_sp_ind |>
    dplyr::filter(species_code != "UNDO") |>
    dplyr::group_by(grid_id) |>
    dplyr::count() |>
    dplyr::rename(n_species = n)
  
  count_grid_sp_ind_complete <- count_grid_sp_ind |>
    dplyr::ungroup() |>
    tidyr::complete(
      grid_id = 1:nrow(honeycomb_grid_sf),
      species_code,
      fill = list(n_individuals = 0)
    )
  
  sp_count_wide <- count_grid_sp_ind_complete |>
    tidyr::pivot_wider(
      names_from = species_code,
      values_from = n_individuals
    )
  
  grid_sp_ind_summary <- count_grid_sp_ind_complete |>
    dplyr::group_by(grid_id) |>
    dplyr::summarise(
      median_n_ind_per_sp = stats::median(n_individuals),
      mean_n_ind_per_sp   = base::mean(n_individuals),
      n_inds_total        = base::sum(n_individuals),
      .groups = "drop"
    )
  
  # 4) JOIN BACK -----------------------------------------------------
  honeycomb_grid_sf <- honeycomb_grid_sf |>
    dplyr::left_join(grid_sp_ind_summary, by = "grid_id") |>
    dplyr::left_join(sp_count, by = "grid_id") |>
    dplyr::mutate(n_species = base::ifelse(base::is.na(n_species), 0, n_species)) |>
    dplyr::left_join(sp_count_wide, by = "grid_id")
  
  # 5) FILTER --------------------------------------------------------
  honeycomb_no0_inds_sf <- dplyr::filter(honeycomb_grid_sf, n_inds_total > 0)
  honeycomb_no0_sp_sf   <- dplyr::filter(honeycomb_grid_sf, n_species > 0)
  
  # 6) TOTAL COUNTS --------------------------------------------------
  total_n_inds <- points_sf |>
    sf::st_drop_geometry() |>
    dplyr::distinct(sc_individual_id) |>
    nrow()
  
  total_n_sp <- points_sf |>
    sf::st_drop_geometry() |>
    dplyr::filter(species_code != "UNDO") |>
    dplyr::distinct(species_code) |>
    nrow()
  
  # 7) BASE MAP ------------------------------------------------------
  world_sf <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf") |>
    sf::st_transform(crs = crs)
  
  wh_sf <- rnaturalearth::ne_countries(
    continent = c("North America", "South America"),
    scale = 50,
    returnclass = "sf"
  ) |>
    sf::st_transform(crs)
  
  plot_title <- if (!base::is.null(date_string)) {
    base::paste0(
      "Shorebird Collective Contributed Data (Argos & GPS)\n",
      date_string
    )
  } else {
    "Shorebird Collective Contributed Data (Argos & GPS)"
  }
  
  # CREATE SCALE BREAKS ------------------------------------
  break0 <- min(honeycomb_no0_sp_sf$n_species)
  break3 <- max(honeycomb_no0_sp_sf$n_species)
  break2 <- round(max(honeycomb_no0_sp_sf$n_species)/3, digits = 0)
  break1 <- round((max(honeycomb_no0_sp_sf$n_species)/3)*2, digits = 0)
  
  # 8) SPECIES PLOT --------------------------------------------------
  sprich_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = world_sf, fill = "lightgrey") +
    ggplot2::geom_sf(
      data = honeycomb_no0_sp_sf,
      ggplot2::aes(fill = n_species),
      colour = NA
    ) +
    ggplot2::scale_fill_viridis_c(
      alpha = .4,
      breaks = c(break0, break1, break2, break3)
    ) +
    ggplot2::theme_bw() +
    ggplot2::coord_sf(
      xlim = sf::st_bbox(wh_sf)[c(1,3)],
      ylim = sf::st_bbox(wh_sf)[c(2,4)]
    ) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::labs(
      fill = base::paste0("number of species per cell \n (of ", total_n_sp, " total contributed)")
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.key.width = grid::unit(0.35, "in")
    )
  
  # 9) INDIVIDUALS PLOT ----------------------------------------------
  ind_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = world_sf, fill = "lightgrey") +
    ggplot2::geom_sf(
      data = honeycomb_no0_inds_sf,
      ggplot2::aes(fill = n_inds_total),
      colour = NA
    ) +
    ggplot2::scale_fill_viridis_c(alpha = .4, option = "magma") +
    ggplot2::theme_bw() +
    ggplot2::coord_sf(
      xlim = sf::st_bbox(wh_sf)[c(1,3)],
      ylim = sf::st_bbox(wh_sf)[c(2,4)]
    ) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::labs(
      fill = base::paste0(" number of individuals per cell \n (of ", total_n_inds, " total contributed)")
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.key.width = grid::unit(0.75, "in")
    )
  
  # 10) RETURN -------------------------------------------------------
  list(
    honeycomb_no0_inds_sf = honeycomb_no0_inds_sf,
    honeycomb_no0_sp_sf   = honeycomb_no0_sp_sf,
    sprich_plot           = sprich_plot,
    ind_plot              = ind_plot
  )
}
