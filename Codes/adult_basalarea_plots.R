compute_basal_area_by_radius <- function(result_list, radii, focal_species) {
  
  n_plots <- nrow(result_list[[1]])
  n_radii <- length(radii)
  
  plotnums <- result_list[[1]]$plotnum
  
  # storage matrices
  cons_mat <- matrix(0, nrow = n_plots, ncol = n_radii)
  total_mat <- matrix(0, nrow = n_plots, ncol = n_radii)
  
  for (r_i in seq_along(radii)) {
    
    dat <- result_list[[r_i]]
    
    for (i in seq_len(n_plots)) {
      
      sp <- dat$species[[i]]
      ba <- dat$ba[[i]]
      
      # safety checks
      if (length(sp) == 0 || length(ba) == 0) next
      if (length(sp) != length(ba)) next  # prevents silent bugs
      
      sp <- as.character(sp)
      ba <- as.numeric(ba)
      
      # total BA (all trees)
      total_mat[i, r_i] <- sum(ba, na.rm = TRUE)
      
      # conspecific BA
      cons_mat[i, r_i] <- sum(ba[sp == focal_species], na.rm = TRUE)
    }
  }
  
  cons_df <- as.data.frame(cons_mat)
  total_df <- as.data.frame(total_mat)
  
  names(cons_df) <- paste0("r", radii)
  names(total_df) <- paste0("r", radii)
  
  cons_df$plotnum <- plotnums
  total_df$plotnum <- plotnums
  
  cons_df <- cons_df[, c("plotnum", paste0("r", radii))]
  total_df <- total_df[, c("plotnum", paste0("r", radii))]
  
  list(
    conspecific_ba = cons_df,
    total_ba = total_df
  )
}