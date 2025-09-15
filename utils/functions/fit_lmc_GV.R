#' Fit Linear Model of Coregionalization using Goulard & Voltz Algorithm
#'
#' Implements the Goulard & Voltz (1992) algorithm for fitting Linear Models of 
#' Coregionalization (LMC) with guaranteed positive semi-definiteness.
#'
#' @param semvar List of empirical (cross-)variograms
#' @param model List of initial variogram models to be fitted
#'
#' @return List of variogram models with optimized sill parameters
#'
#' @details
#' The function iteratively optimizes sill parameters while maintaining positive 
#' semi-definiteness through the following steps:
#' 1. Data preparation and structure setup
#' 2. Parameter extraction (sills, empirical values, theoretical values)
#' 3. Iterative optimization with weighted least squares
#' 4. Positive semi-definiteness enforcement at each step
#' 5. Model updating with optimized parameters
#'
#' The algorithm continues until either:
#' - Convergence is reached (WSS change < 1e-6)
#' - Maximum iterations (500) is reached
#'
#' @references
#' Goulard, M., & Voltz, M. (1992). Linear coregionalization model: tools for estimation
#' and choice of cross-variogram matrix. Mathematical Geology, 24(3), 269-286.
#'
#' @author Jonas Alcaina-Mateos (Original R adaptation of algorithm)
#' @author Abel Ruiz-Giralt (Added comments and documentation)
#'
#' @examples
#' \dontrun{
#' v <- variogram(g, cutoff=100, width=10)
#' g.fit <- fit.variogram(v, vgm(1, "Sph", 50))
#' fitted_lmc <- fitGV(v, g.fit)
#' }
#'
#' @export
fit_lmc_GV <- function(semvar, model) {
  # Step 1: Data Preparation
  # Split variogram data by ID to handle direct and cross-variograms separately
  semvar <- split(semvar, f=semvar$id)
  # Remove any models that don't have corresponding variograms
  model <- model[1:length(semvar)] 
  # Align variograms with their models
  semvar <- semvar[names(model)]

  # Step 2: Initialize Parameters
  # Create matrix of sill parameters - rows are variables, columns are structures
  sills <- outer(1:length(model), 1:nrow(model[[1]]), 
                 FUN=Vectorize(function(i,j) model[[i]]$psill[j]))
  # Extract experimental variogram values into a matrix
  yvect <- sapply(1:length(semvar), 
                  function(i) matrix(semvar[[i]]$gamma, ncol=1))
  # Calculate theoretical variogram values for each basic structure
  values <- sapply(1:nrow(model[[1]]), function(x) 
    variogramLine(vgm(1, as.character(model[[1]][x,]$model), 
                     model[[1]][x,]$range), 
                 dist_vector = semvar[[1]]$dist)$gamma)
  # Get weights (number of pairs) for weighted least squares
  w <- sapply(1:length(semvar), function(x) semvar[[x]]$np)

  # Initialize Weighted Sum of Squares for convergence tracking
  WSS <- c(sum(w * (yvect - (values %*% t(sills)))^2))

  # Step 3: Main Optimization Loop
  # Iteratively adjust sill parameters to minimize WSS while maintaining positive definiteness
  for (it in 1:500) {
    # For each basic structure
    for (s in 1:ncol(sills)) {
      # Calculate residuals excluding current structure
      aux1 <- (yvect - (values[,-s] %*% t(sills[,-s])))
      # Weight residuals by variogram values of current structure
      aux2 <- w * apply(aux1, 2, function(x) values[,s]*x)
      # Calculate new sill estimates using weighted least squares
      aux3 <- colSums(aux2) / colSums(w * values[,s]^2)
      # Ensure positive definiteness of coregionalization matrices
      sills[,s] <- posDef(aux3)
    }
    
    # Update WSS and check for convergence
    WSS[it+1] <- sum(w * (yvect - (values %*% t(sills)))^2)
    # Stop if improvement is below threshold
    if(it > 1 & abs(WSS[it] - WSS[it+1]) < 10^-6) break
  }

  # Step 4: Update Model Parameters
  # Replace original sill values with optimized ones
  for (i in 1:length(model)) model[[i]]$psill <- sills[i,]
  
  return(model)
}

# Helper function to construct symmetric matrix from vector
# Input: Vector containing lower triangular elements
# Output: Full symmetric matrix
buildMat <- function(x) {
  # Calculate matrix dimension from vector length
  n <- (sqrt(1 + (8*length(x)))-1) / 2
  # Create empty matrix
  m <- matrix(NA, n, n)
  # Fill lower triangle and diagonal
  m[lower.tri(m, diag=TRUE)] <- x
  # Mirror to upper triangle
  m <- t(m)
  m[lower.tri(m, diag=TRUE)] <- x
  return(m)
}

# Helper function to ensure matrix positive definiteness
# Input: Vector representing lower triangular matrix
# Output: Vector representing positive definite matrix
posDef <- function(x) {
  # Convert vector to matrix and get eigendecomposition
  q <- eigen(buildMat(x))
  d <- q$values
  # Replace negative eigenvalues with small positive number
  d[d < 0] <- 1e-10
  # Reconstruct matrix with corrected eigenvalues
  res <- q$vectors %*% diag(d) %*% t(q$vectors)
  # Return lower triangular part
  return(res[lower.tri(res, diag=T)])
}