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
    plot.x >= 100, plot.x <= 900,
    plot.y >= 100, plot.y <= 400
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
  
  # Check if all distances are >= 20
  if (all(dists >= 20)) {
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

intervals <- final_data %>%
  distinct(census) %>%
  arrange(census) %>%
  mutate(
    next_census = lead(census),
    dt = next_census - census
  ) %>%
  filter(!is.na(next_census))

presence <- final_data %>%
  filter(status == "A") %>%
  group_by(plotnum, spp, census) %>%
  summarise(
    IDs = list(unique(ID)),
    .groups = "drop"
  )

all_plots <- unique(final_data$plotnum)
all_species <- unique(final_data$spp)

full_grid <- expand.grid(
  plotnum = all_plots,
  spp = all_species,
  census = intervals$census
)
df_all <- full_grid %>%
  left_join(presence, by = c("plotnum", "spp", "census")) %>%
  left_join(intervals, by = "census")
df_all <- df_all %>%
  left_join(
    presence,
    by = c("plotnum", "spp", "next_census" = "census"),
    suffix = c("_t", "_t1")
  )
df_all <- df_all %>%
  rowwise() %>%
  mutate(
    recruits = case_when(
      is.null(IDs_t1) ~ 0,
      is.null(IDs_t) ~ length(IDs_t1),
      TRUE ~ length(setdiff(IDs_t1, IDs_t))
    ),
    recruit_present = as.integer(recruits > 0)
  ) %>%
  ungroup()

library(furrr)
library(future)

plan(multisession, workers = parallel::detectCores() - 1)

species_split <- df_all %>%
  group_split(spp)

final_list <- future_map(
  species_split,
  function(df_sp) {
    
    spp_name <- unique(df_sp$spp)
    
    recruitment_df <- df_sp %>%
      select(plotnum, census, next_census, dt, recruit_present, recruits)
    
    census_df <- intervals
    
    species_df <- data.frame(spp = spp_name)
    
    list(
      recruitment = recruitment_df,
      census = census_df,
      species = species_df
    )
  }
)
names(final_list) <- future_map_chr(species_split, ~ unique(.x$spp))


