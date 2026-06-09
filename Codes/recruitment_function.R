## code to calculate # recruits per plot for unique species between censuses
recruitment_plots<- readRDS("Data_derived/seedling_plots_20m.Rds")  # seedling plots are separated by 20m
track_recruitment <- function(data,
                              species_name,
                              min_interval = 3,
                              dbh_max = 49) {

  # Species-specific records
  sp_data <- data %>%
    dplyr::filter(spp == species_name) %>%
    dplyr::arrange(plotnum, census, ID)

  # All census years where the species was observed
  census_years <- sort(unique(sp_data$census))

  # All plots in the dataset
  all_plots <- sort(unique(data$plotnum))

  out <- list()
  k <- 1

  for (current_year in census_years) {

    future_years <- census_years[census_years >= current_year + min_interval]

    if (length(future_years) == 0)
      next

    next_year <- future_years[1]
    interval <- next_year - current_year

    for (p in all_plots) {

      current_ids <- sp_data %>%
        dplyr::filter(
          plotnum == p,
          census == current_year
        ) %>%
        dplyr::pull(ID) %>%
        unique()

      recruits <- sp_data %>%
        dplyr::filter(
          plotnum == p,
          census == next_year,
          !(ID %in% current_ids),
          is.na(dbh) | dbh <= dbh_max
        ) %>%
        dplyr::distinct(ID)

      out[[k]] <- tibble::tibble(
        plot_number = p,
        species = species_name,
        current_year = current_year,
        next_year = next_year,
        interval = interval,
        recruit_count = nrow(recruits),
        recruit_ids = list(recruits$ID)
      )

      k <- k + 1
    }
  }

  dplyr::bind_rows(out)
}
