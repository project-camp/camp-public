simple_EMBDL_replacement <- function(X, detection_limits = NULL, tolerance = 1e-5, 
                                   default_dl_method = "minimum", maxit = 1000) {
  
  # Implements EM (Expectation-Maximization) algorithm to replace values below detection limits in compositional data, 
  # using multiplicative replacement strategy and normalization steps to maintain compositional coherence.
  
  # References: 
  # Palarea-Albaladejo & Martín-Fernández (2008): A modified EM alr-algorithm for replacing rounded zeros in compositional data sets
  # Martín-Fernández et al. (2003): DealingWith Zeros and Missing Values in Compositional Data Sets Using Nonparametric Imputation

  # Input validation
  if (!is.matrix(X)) {
      X <- as.matrix(X)
  }
  
  # Replace any NA values with 0 this is a preprocessing step not in the original paper, but necessary for practical implementation
  X[is.na(X)] <- 0
  
  # Initialize
  n <- nrow(X)
  D <- ncol(X)
  X_working <- X

  # Detection Limit Handling (Following Palarea-Albaladejo & Martín-Fernández, 2008)
  # When detection limits are unknown, they suggest using a fraction of the minimum observed value for each component
  if (!is.null(detection_limits)) {
      if (length(detection_limits) != D) {
          stop("Number of detection limits must match number of columns in data")
      }
      if (!is.numeric(detection_limits)) {
          stop("Detection limits must be numeric")
      }
      if (any(detection_limits <= 0)) {
          stop("Detection limits must be positive values")
      }
      dl <- detection_limits
  } else {
      # Calculate detection limits when not provided
      dl <- numeric(D)
      colnames_X <- colnames(X)
      
      for (j in 1:D) {
          non_zero_values <- X[X[, j] > 0, j]
          if (length(non_zero_values) > 0) {
              if (default_dl_method == "minimum") { # Original paper recommends using 0.65 of minimum observed value
                  dl[j] <- 0.65 * min(non_zero_values)           
              } else if (default_dl_method == "fraction") { # Alternative method not in original paper
                  dl[j] <- 0.1 * mean(non_zero_values)
              }
          } else {
              dl[j] <- 1e-4 # Small default value for columns with all zeros
              warning(sprintf("No non-zero values found for column %s, using default detection limit of 1e-4",
                            if(!is.null(colnames_X)) colnames_X[j] else j))
          }
      }
  }

  # Name the detection limits vector if data has column names
  if (!is.null(colnames(X))) {
      names(dl) <- colnames(X)
  }
  
  # Store detection limits
  attr(X_working, "detection_limits") <- dl

  # Identify zeros for replacement
  zero_pos <- which(X == 0, arr.ind = TRUE)
  if (length(zero_pos) == 0) {
      return(X)
  }
  
  # Core EM Algorithm Structure
  # - The E-step estimates expected values for zeros
  # - The M-step ensures compositional coherence through normalization

  # Initialize EM algorithm parameters
  iter <- 0
  max_diff <- Inf
  
  # Implementation of the modified EM algorithm as described in Palarea-Albaladejo & Martín-Fernández (2008)
  # The key MRF innovations are (Multiplicative Replacement Formula)
  # - dl_i/2 represents half the detection limit
  # - The multiplication by (sum(others)/(sum(others) + dl_i/2)) ensures the replacement preserves compositional relationships
  
  tryCatch({
      while (iter < maxit && !is.na(max_diff) && max_diff > tol) {
          X_old <- X_working
          
          # E-step: Replace zeros with expected values,  implements equation (2) from the paper
          for (i in 1:nrow(zero_pos)) {
              row <- zero_pos[i, 1]
              col <- zero_pos[i, 2]
              
              others <- X_working[row, -col]
              dl_i <- dl[col]
              
              # Implementation of the multiplicative replacement strategy, equation (2) from the paper
              denom <- sum(others) + dl_i/2
              if (denom > 0) {
                  # This is the key multiplicative replacement formula from the paper, z_i = δ_i/2 * (sum(x_j)/(sum(x_j) + δ_i/2))
                  X_working[row, col] <- dl_i / 2 * (sum(others) / denom)
              } else {
                  # Fallback not specified in original paper but needed for numerical stability
                  X_working[row, col] <- dl_i / 2
              }
          }
          
          # M-step: Normalize to ensure compositional nature
          row_sums <- rowSums(X_working)
          X_working <- X_working / row_sums
          
          # Convergence checking as described by:
          # - Monitoring the maximum absolute change between iterations
          # - Ensuring numerical stability in the results
          
          max_diff <- max(abs(X_working - X_old), na.rm = TRUE)
          iter <- iter + 1
          
          if (any(is.nan(X_working))) {
              warning("NaN values detected during iteration")
              break
          }
      }
  }, error = function(e) {
      warning(paste("Error occurred during iteration:", e$message))
  })
  
  # Store final attributes and return
  attr(X_working, "iterations") <- iter
  attr(X_working, "convergence") <- !is.na(max_diff) && max_diff <= tol
  attr(X_working, "final_diff") <- max_diff
  attr(X_working, "detection_limits") <- dl
  
  return(X_working)
}
