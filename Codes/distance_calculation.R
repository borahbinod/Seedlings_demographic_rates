seedling_plots<- readRDS("seedling_plots.Rds") ## spatially thinned seedling plots
adult_trees<- readRDS("adult_tree.Rds") ## adult trees in 50 ha plot
radii <- c(5,8,10,12,15,17,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100) # radii tested for recruitment

# adult census years: 2000, 2005, 2006, 2010, 2011, 2013, 2015, 2016
# seedling census years: 2001 (2000), 2002 (2000), 2003 (2000), 2004 (2005), 2006 (2005), 2008 (2010), 2009 (2010), 2011 (2010), 2012 (2010), 2013 (2015), 2014 (2015), 2016 (2015), 2017 (2015), 2018 (2015)


# searching adult trees within radius i around individual seedling plot
library(data.table)
library(RANN)
library(dbscan)

# ----------------------------
# 1. Filter years first
# ----------------------------

adult2000 <- adult_trees[adult_trees$year == 2015, ]
seedling2001 <- seedling_plots

# ----------------------------
# 2. Keep only needed columns (unique plots)
# ----------------------------

plots2001 <- unique(
  seedling2001[, c("plotnum", "plot.x", "plot.y")]
)

adult2000 <- adult2000[, c("treeID", "sp", "ba", "gx", "gy")]

# ----------------------------
# 3. Force numeric (important safety step)
# ----------------------------

adult2000$gx <- as.numeric(adult2000$gx)
adult2000$gy <- as.numeric(adult2000$gy)

plots2001$plot.x <- as.numeric(plots2001$plot.x)
plots2001$plot.y <- as.numeric(plots2001$plot.y)

# ----------------------------
# 4. Remove NA / NaN / Inf
# ----------------------------

adult2000 <- adult2000[
  is.finite(adult2000$gx) &
    is.finite(adult2000$gy) &
    !is.na(adult2000$treeID),
]

keep <- is.finite(plots2001$plot.x) &
  is.finite(plots2001$plot.y)

plots2001 <- plots2001[keep, , drop = FALSE]
# ----------------------------
# 5. Build matrices for RANN
# ----------------------------

adult_xy <- cbind(adult2000$gx, adult2000$gy)
plot_xy  <- cbind(plots2001$plot.x, plots2001$plot.y)

# ----------------------------
# 6. Radii
# ----------------------------

radii <-  c(5,8,10,12,15,17,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)


# ----------------------------
# 7. Radius search once (max radius)
# ----------------------------

nn <- frNN(
  x = adult_xy,
  eps = max(radii),
  query = plot_xy
)

# ----------------------------
# 8. Build lookup object
# ----------------------------

result_list <- vector("list", length(radii))
names(result_list) <- paste0("r", radii)

# ----------------------------
# 9. Loop over radii
# ----------------------------

for (k in seq_along(radii)) {
  
  r <- radii[k]
  
  out <- plots2001
  
  out$treeIDs <- vector("list", nrow(out))
  out$species <- vector("list", nrow(out))
  out$ba      <- vector("list", nrow(out))
  
  for (i in seq_len(nrow(out))) {
    
    idx <- nn$id[[i]]
    
    if (length(idx) > 0) {
      
      dx <- adult2000$gx[idx] - out$plot.x[i]
      dy <- adult2000$gy[idx] - out$plot.y[i]
      
      dist <- sqrt(dx^2 + dy^2)
      
      keep <- dist <= r
      
      out$treeIDs[[i]] <- adult2000$treeID[idx][keep]
      out$species[[i]] <- adult2000$sp[idx][keep]
      out$ba[[i]]      <- adult2000$ba[idx][keep]
      
    } else {
      
      out$treeIDs[[i]] <- integer(0)
      out$species[[i]] <- character(0)
      out$ba[[i]]      <- numeric(0)
      
    }
  }
  
  out$radius <- r
  
  result_list[[k]] <- out
}
