## code to calculate # recruits per plot for unique species between censuses
recruitment_plots<- readRDS("Data_derived/seedling_plots_20m.Rds")  # seedling plots are separated by 20m

track_recruitment <- function(data,
                              species_name,
                              min_interval = 3,   # census interval is 3 years or more 
                              dbh_max = 49) { # seedling dbh is 49mm or less
  
  sp_data <- data %>%
    filter(spp == species_name) %>%
    arrange(plotnum, census, ID)
  
  census_years <- sort(unique(sp_data$census))
  
  out <- list()
  k <- 1
  
  for (current_year in census_years) {
    
    # first available census at least min_interval years later
    future_years <- census_years[census_years >= current_year + min_interval]
    
    if (length(future_years) == 0)
      next
    
    next_year <- future_years[1]
    
    interval <- next_year - current_year
    
    plots <- sort(unique(sp_data$plotnum))
    
    for (p in plots) {
      
      current_ids <- sp_data %>%
        filter(
          plotnum == p,
          census == current_year
        ) %>%
        pull(ID) %>%
        unique()
      
      recruits <- sp_data %>%
        filter(
          plotnum == p,
          census == next_year,
          !(ID %in% current_ids),
          is.na(dbh) | dbh <= dbh_max
        ) %>%
        distinct(ID)
      
      out[[k]] <- tibble(
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
  
  bind_rows(out)
}
recruit_species<- track_recruitment(data = recruitment_plots,species_name = "BEILPE")
