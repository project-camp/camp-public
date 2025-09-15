#' Create a Regular Grid from a SpatialPointsDataFrame
#'
#' @description
#' Creates a regular grid of points based on the extent of a SpatialPointsDataFrame.
#' The grid can optionally be clipped to the convex hull of the input points.
#'
#' @param spdf A SpatialPointsDataFrame object containing the points
#' @param resolution Numeric. The cell size of the grid in the units of the input CRS
#' @param buffer Numeric. Additional distance beyond the extent of points (in CRS units)
#' @param convex_hull Logical. If TRUE, clips the grid to the convex hull of points
#'
#' @return A SpatialPoints object containing the regular grid points
#'
#' @importFrom sp coordinates proj4string CRS SpatialPolygons Polygons Polygon chull
#' @importFrom sf st_bbox
#'
#' @note The grid density is controlled by the resolution parameter. Smaller values create
#' a denser grid but increase computation time.
#'
#' @author Antonis Koutrompas, Abel Ruiz-Giralt
#'
#' @examples
#' \dontrun{
#' # Create a sample SpatialPointsDataFrame
#' pts <- data.frame(x = runif(10), y = runif(10))
#' coordinates(pts) <- ~x+y
#' proj4string(pts) <- CRS("+proj=longlat +datum=WGS84")
#'
#' # Generate grid with 0.1 degree resolution and 1 degree buffer
#' grid <- create_grid_from_spdf(pts, resolution = 0.1, buffer = 1)
#'
#' # Create a clipped grid using convex hull
#' grid_clipped <- create_grid_from_spdf(pts, resolution = 0.1, 
#'                                      buffer = 0, convex_hull = TRUE)
#' }
#'
#' @export

create_grid_from_spdf <- function(spdf, resolution = res, buffer = 0, convex_hull = FALSE) {
    # Validate inputs
    if (!inherits(spdf, "SpatialPointsDataFrame")) {
        stop("Input must be a SpatialPointsDataFrame")
    }
    if (!is.numeric(resolution) || resolution <= 0) {
        stop("Resolution must be a positive number")
    }
    if (!is.numeric(buffer) || buffer < 0) {
        stop("Buffer must be a non-negative number")
    }

    # Get coordinates of the points
    coords <- coordinates(spdf)

    # Define grid parameters
    x_start <- min(coords[,1]) - buffer
    x_end <- max(coords[,1]) + buffer
    y_start <- min(coords[,2]) - buffer
    y_end <- max(coords[,2]) + buffer
    
    # Create grid points
    grid <- expand.grid(
        x = seq(x_start, x_end, by = resolution),
        y = seq(y_start, y_end, by = resolution)
    )
    
    # Convert to spatial points
    coordinates(grid) <- ~x + y
    # Use the same projection as the data
    proj4string(grid) <- proj4string(spdf)  

    if (convex_hull) {
        #Calculate convex hull using chull()
        hull_indices <- chull(coords)
        # Close the polygon by appending the first point at the end
        hull_coords <- coords[hull_indices, ]
        hull_coords <- rbind(hull_coords, hull_coords[1, ])
        # Create a SpatialPolygons object from the convex hull
        hull_polygon <- SpatialPolygons(
            list(Polygons(list(Polygon(hull_coords)), ID = 1)),
            proj4string = CRS(proj4string(spdf)))
        # Convert the hull to sf and add buffer
        hull_sf <- st_as_sf(hull_polygon)
        buffered_hull_sf <- st_buffer(hull_sf, dist = buffer)  # Apply the buffer
        # Clip the grid to the buffered convex hull using st_intersection
        grid_sf <- st_as_sf(grid)  # Convert the grid to sf
        grid_clipped <- st_intersection(grid_sf, buffered_hull_sf)  # Clip the grid
        # Convert the clipped grid back to SpatialPointsDataFrame
        grid_sp <- as(grid_clipped, "Spatial")
        grid <- SpatialPoints(grid_sp)
        colnames(grid@coords) <- c("x", "y")
    }
    
    return(grid)
}
