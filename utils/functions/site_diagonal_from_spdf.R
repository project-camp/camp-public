#' Calculate Site Diagonal from SpatialPointsDataFrame
#'
#' Calculates the diagonal distance of the bounding box containing all points
#' in a SpatialPointsDataFrame.
#'
#' @param spdf A SpatialPointsDataFrame object containing the points
#'
#' @return Numeric. The length of the diagonal of the bounding box in the units of the input CRS
#'
#' @importFrom sp bbox
#'
#' @author Abel Ruiz-Giralt
#'
#' @examples
#' \dontrun{
#' # Create a sample SpatialPointsDataFrame
#' pts <- data.frame(x = runif(10), y = runif(10))
#' coordinates(pts) <- ~x+y
#' 
#' # Calculate site diagonal
#' diagonal <- site_diagonal_from_spdf(pts)
#' }
#'
#' @export
site_diagonal_from_spdf <- function(spdf) {
    # Validate input
    if (!inherits(spdf, "SpatialPointsDataFrame")) {
        stop("Input must be a SpatialPointsDataFrame")
    }
    
    # Get the bounding box of the spatial object
    bbox <- bbox(spdf)
    
    # Calculate diagonal using Pythagorean theorem
    site_diag <- sqrt((bbox[1,2] - bbox[1,1])^2 + 
                      (bbox[2,2] - bbox[2,1])^2)
    
    return(site_diag)
}