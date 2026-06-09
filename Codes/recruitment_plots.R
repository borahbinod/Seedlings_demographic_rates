seedling_data<-  read.csv(
  "Data_derived/DataS1/BCI.seedling.census.data.2001.to.2018.csv",
  header = TRUE,
stringsAsFactors = FALSE
      # handles rows with missing values
)

count_seedling<-seedling_data %>%   ## seedling per plot
  group_by(plotnum) %>%
  summarise(n_unique_ID = n_distinct(ID)) %>%
  arrange(desc(n_unique_ID))

seedling_filtered <- seedling_data %>%  ## filtering plots that are atleast 100 m away from 50 ha plot boundaries
  filter(
    plot.x >= 30, plot.x <= 970,
    plot.y >= 30, plot.y <= 470
  )

plot_counts <- seedling_filtered %>%
  group_by(plot.x, plot.y, plotnum) %>%
  summarise(n_seedlings = n(), .groups = "drop")
plot_counts <- plot_counts %>%
  arrange(desc(n_seedlings))

selected_plots <- data.frame()

for (i in 1:nrow(plot_counts)) {
  
  candidate <- plot_counts[i, ]
  
  if (nrow(selected_plots) == 0) {
    selected_plots <- candidate
    next
  }
  
  # Compute distances to already selected plots
  dists <- sqrt(
    (selected_plots$plot.x - candidate$plot.x)^2 +
      (selected_plots$plot.y - candidate$plot.y)^2
  )
  
  # Check if all distances are >= 15
  if (all(dists >= 15)) {
    selected_plots <- rbind(selected_plots, candidate)
  }
}

# scenario 1: considering each individual seedling plot
final_data <- seedling_filtered %>%
  semi_join(selected_plots, by = c("plot.x", "plot.y", "plotnum"))
y<-final_data %>%
  group_by(spp, census) %>%
  summarise(n_individuals = n_distinct(ID), .groups = "drop")
result <- y %>%
  filter(n_individuals >= 20) %>%
  count(spp, name = "n_plot_census_ge30")

## code to calculate # recruits per plot for unique species between censuses
recruitment_plots<- readRDS("Data_derived/Recruitment/seedling_plots_20m.Rds")

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
recruit_species<- track_recruitment(data = recruitment_plots,species_name = "FARAOC")
