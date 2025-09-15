#' Calculate Lag Distance from SpatialPointsDataFrame
#'
#' Calculates the median of the minimum distances between points in a
#' SpatialPointsDataFrame. This is useful for determining appropriate
#' lag distances in spatial analysis.
#'
#' @param spdf A SpatialPointsDataFrame object containing the points
#'
#' @return Numeric. The median minimum distance between points in the units of the input CRS
#'
#' @importFrom sp coordinates
#' @importFrom stats dist median
#'
#' @author Abel Ruiz-Giralt
#'
#' @examples
#' \dontrun{
#' # Create a sample SpatialPointsDataFrame
#' pts <- data.frame(x = runif(10), y = runif(10))
#' coordinates(pts) <- ~x+y
#' 
#' # Calculate lag distance
#' lag <- lag_distance_from_spdf(pts)
#' }
#'
#' @export
#' 
lag_distance_from_spdf <- function(spdf) {
    # Validate input
    if (!inherits(spdf, "SpatialPointsDataFrame")) {
        stop("Input must be a SpatialPointsDataFrame")
    }
    
    # Calculate the pairwise Euclidean distances between all coordinates
    ddm <- as.matrix(dist(coordinates(spdf)))
    
    # Set the diagonal values (distance from a point to itself) to NA
    diag(ddm) <- NA
    
    # Calculate the median of minimum distances
    lag_dist <- ceiling(median(apply(ddm, 1, min, na.rm = TRUE)))
    
    return(lag_dist)
}