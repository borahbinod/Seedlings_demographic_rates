# function to track seedling survival, here seed plots are fixed and seedling fate within them are tracked
final_data<- 
track_seedlings <- function(data, species_name, 
                            min_interval = 3, # minimum interval is 3 years
                            dbh_max = 49) { # seedling dbh is 49mm or less
  
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
survival_species<- track_seedlings(data = final_data,species_name = "EUGEGA")
