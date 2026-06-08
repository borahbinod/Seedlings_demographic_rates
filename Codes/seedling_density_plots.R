
seedling_plot_counts <- function(dat, species_name, census_year) {
  
  # subset to census year and alive individuals
  dat_year <- dat %>%
    filter(census == census_year,
           status == "A")
  
  # focal species counts
  species_count_df <- dat_year %>%
    filter(spp == species_name) %>%
    group_by(plotnum) %>%
    summarise(
      count = n_distinct(ID),
      uniqueID = list(unique(ID)),
      species = list(unique(spp)),
      .groups = "drop"
    )
  
  # all-species counts
  total_count_df <- dat_year %>%
    group_by(plotnum) %>%
    summarise(
      count = n_distinct(ID),
      uniqueID = list(unique(ID)),
      species = list(unique(spp)),
      .groups = "drop"
    )
  
  return(list(
    species_count = species_count_df,
    total_count = total_count_df
  ))
}
