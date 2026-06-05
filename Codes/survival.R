# function to pick seedlings after spatially filtering
seedlings <- subset(seedling_data, dbh <= 1 | is.na(dbh))
get_spaced_seedlings <- function(species,
                                 census_year,
                                 data = seedlings,
                                 min_dist = 15) {
  
  # filter species, census, alive
  dat <- subset(
    data,
    spp == species &
      census == census_year &
      status == "A"
  )
  
  # remove duplicate IDs if present
  dat <- dat[!duplicated(dat$ID), ]
  
  if (nrow(dat) == 0) {
    return(NULL)
  }
  
  # random order
  dat <- dat[sample(nrow(dat)), ]
  
  selected <- integer(0)
  
  for (i in seq_len(nrow(dat))) {
    
    if (length(selected) == 0) {
      selected <- i
      next
    }
    
    dx <- dat$plot.x[selected] - dat$plot.x[i]
    dy <- dat$plot.y[selected] - dat$plot.y[i]
    
    d <- sqrt(dx^2 + dy^2)
    
    if (all(d >= min_dist)) {
      selected <- c(selected, i)
    }
  }
  
  dat[selected, ]
} 
seedling_year<- get_spaced_seedlings(species = "BEILPE",census_year = 2001,data = seedlings,min_dist = 15)
seedling_list<- read.csv("seedling_list.csv",header = T)

years <- unique(seedling_data$census)



for(y in years){
  colname <- paste0("seedling_", y)
  seedling_list[[colname]] <- sapply(
    seedling_list$spp,
    function(sp){
      
      out <- tryCatch(
        get_spaced_seedlings(
          species = sp,
          census_year = y,
          data = seedlings,
          min_dist = 15
        ),
        error = function(e) NULL
      )
      
      if(is.null(out)) NA_integer_ else nrow(out)
    }
  )
}



species_30plots <- final_data %>%
  filter(status == "A") %>%
  group_by(census, spp) %>%
  summarise(
    n_plots = n_distinct(plotnum),
    .groups = "drop"
  ) %>%
  filter(n_plots >= 20)
species_all_years <- species_30plots %>%
  group_by(spp) %>%
  summarise(n_years = n_distinct(census)) %>%
  filter(n_years == n_distinct(final_data$census)) %>%
  pull(spp)

species_matrix <- final_data %>%
  filter(status == "A") %>%
  semi_join(species_30plots, by = c("census", "spp")) %>%
  group_by(spp, census) %>%
  summarise(n_individuals = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = census,
    values_from = n_individuals,
    values_fill = 0
  )

species_matrix
#######################
# function to track seedling survival and growth, here seed plots are fixed and seedling fate within them are tracked

track_seedlings <- function(data, species_name, min_interval = 3, dbh_max = 49) {
  
  full_sp <- data %>%
    filter(spp == species_name) %>%
    arrange(ID, census)
  
  # START: only seedlings (DBH constraint applies here ONLY)
  starts <- full_sp %>%
    filter(
      status == "A",
      is.na(dbh) | dbh <= dbh_max
    )
  
  out_list <- list()
  
  for (i in seq_len(nrow(starts))) {
    
    id_i <- starts$ID[i]
    t0   <- starts$census[i]
    
    indiv <- full_sp %>%
      filter(ID == id_i) %>%
      arrange(census)
    
    future_idx <- which(indiv$census - t0 >= min_interval)
    
    if (length(future_idx) == 0) next
    
    j <- future_idx[1]
    
    out_list[[length(out_list) + 1]] <- tibble(
      individualID   = id_i,
      plot_number    = starts$plotnum[i],
      species        = starts$spp[i],
      
      current_year   = t0,
      next_year      = indiv$census[j],
      interval       = indiv$census[j] - t0,
      
      current_status = starts$status[i],
      next_status    = indiv$status[j],
      
      current_codes  = starts$codes[i],
      next_codes     = indiv$codes[j],
      
      current_height = starts$height[i],
      next_height    = indiv$height[j],
      
      current_dbh    = starts$dbh[i],
      next_dbh       = indiv$dbh[j]
    )
  }
  
  bind_rows(out_list)
}
x<- track_seedlings(data = final_data,species_name = "EUGEGA")
xy<-x %>%
  group_by(current_year,plot_number) %>%
  summarise(
    n_individuals = n_distinct(individualID),
    .groups = "drop"
  )

