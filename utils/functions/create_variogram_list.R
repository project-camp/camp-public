#' Create Directional Variograms List
#' 
#' @description 
#' Creates a list of directional variograms from a gstat object at specified angles.
#' This function is useful for analyzing spatial anisotropy by computing variograms
#' in different directions.
#' 
#' @details
#' The function computes experimental variograms for each specified direction,
#' allowing for the analysis of spatial continuity in different directions.
#' Cross-variograms can also be computed if multiple variables are present in
#' the gstat object.
#' 
#' @param g gstat object containing the variables for variogram calculation.
#'          Should be created using gstat().
#' @param width Numeric. Distance lag for variogram bins. Controls the resolution
#'             of the experimental variogram.
#' @param cutoff Numeric. Maximum distance for variogram calculation. Points beyond
#'              this distance are not considered.
#' @param alpha Numeric vector. Angles (in degrees) for directional variograms.
#'             0 degrees points east, 90 degrees points north.
#'             Default is c(0, 45, 90, 135).
#' @param tol.hor Numeric. Horizontal tolerance angle in degrees. Points within
#'                this angular tolerance are included in each direction.
#' @param cross Logical. If TRUE, computes cross-variograms between variables.
#' 
#' @return A list containing variogram objects for each direction. Each element
#' is named as "VGi_angle" where i is the index and angle is the direction in
#' degrees.
#' 
#' @examples
#' \dontrun{
#' # Create a gstat object
#' g <- gstat(formula = z~1, data = measurements)
#' 
#' # Calculate directional variograms
#' vario_list <- create_variogram_list(
#'   g,
#'   width = 100,
#'   cutoff = 1000,
#'   alpha = c(0, 45, 90, 135),
#'   tol.hor = 22.5
#' )
#' 
#' # Plot the variograms
#' plot(vario_list[[1]])
#' }
#' 
#' @seealso 
#' \code{\link[gstat]{variogram}}, \code{\link[gstat]{gstat}}
#' 
#' @export
create_variogram_list <- function(g, width, cutoff,
                                alpha = c(0, 45, 90, 135),
                                tol.hor = 22.5,
                                cross = TRUE) {
  # Initialize list to store directional variograms
  variograms_list <- list()
  
  # Calculate variograms for each direction
  for (i in seq_along(alpha)) {
    a <- alpha[i]
    
    # Compute directional variogram
    v <- variogram(
      g,                    # gstat object with variables
      width = width,        # lag distance
      cutoff = cutoff,      # maximum distance
      alpha = a,            # angle direction
      tol.hor = tol.hor,   # angular tolerance
      cross = cross        # include cross-variograms
    )
    
    # Store with descriptive name including angle
    variograms_list[[paste("VG", i, "_", a, sep = "")]] <- v
  }
  
  return(variograms_list)
}