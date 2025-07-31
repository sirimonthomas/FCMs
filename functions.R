# functions for FCM processing
# Sirimon Thomas
#07/2025


## function to average all FCM adjacency matrices----

average_adjacency_matrices <- function(mat_list) {
  # Convert sparse matrices to regular ones
  mat_list <- lapply(mat_list, function(m) {
    if (inherits(m, "Matrix")) as.matrix(m) else m
  })
  
  # Get all unique node names
  all_names <- unique(unlist(lapply(mat_list, function(m) union(rownames(m), colnames(m)))))
  n <- length(all_names)
  
  # Create 3D array of NAs
  arr <- array(NA, dim = c(n, n, length(mat_list)),
               dimnames = list(all_names, all_names, NULL))
  
  for (k in seq_along(mat_list)) {
    m <- mat_list[[k]]
    
    # Create full-size NA matrix and fill it
    tmp <- matrix(NA, nrow = n, ncol = n,
                  dimnames = list(all_names, all_names))
    tmp[rownames(m), colnames(m)] <- m
    
    #add full matrix to 3d array
    arr[, , k] <- tmp
  }
  
  # Average across third dimension, ignoring NA
  avg_mat <- apply(arr, c(1, 2), function(x) mean(x, na.rm = TRUE))
  avg_mat[is.nan(avg_mat)] <- NA  # Clean up empty entries
  
  return(avg_mat)
}

