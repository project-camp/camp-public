#' Inverse Distance Weighted Interpolation for Compositional Data
#' 
#' @description 
#' Performs IDW interpolation on compositional data with isometric log-ratio (ILR)
#' back-transformation and optional smoothing. Specifically designed for handling
#' compositional data where components sum to a constant (e.g., 100%).
#' 
#' @details
#' The function performs the following steps:
#' 1. Interpolates each component separately using IDW
#' 2. Applies optional smoothing using a focal mean
#' 3. Back-transforms the results to compositional space
#' 4. Creates visualization plots if requested
#' 
#' @param spdf SpatialPointsDataFrame containing the compositional data. Each column
#'             should represent a component of the composition.
#' @param grid SpatialPixelsDataFrame or SpatialGridDataFrame defining the prediction
#'             locations.
#' @param orig Original compositional data structure used for back-transformation.
#' @param idp Numeric. Power parameter for IDW. Higher values give more weight to
#'            closer points.
#' @param maxdist Numeric. Maximum distance beyond which points are not considered
#'                in the interpolation.
#' @param focal_window Integer. Size of the smoothing window for focal mean
#'                     calculation.
#' @param plot_idw Logical. If TRUE, creates visualization plots for each component.
#' 
#' @return A data frame containing:
#'   \item{x,y}{Coordinates of prediction locations}
#'   \item{...}{One column per compositional component with interpolated values}
#'   
#' @examples
#' \dontrun{
#' # Create sample data
#' coords <- expand.grid(x = 1:10, y = 1:10)
#' values <- data.frame(
#'   A = runif(100),
#'   B = runif(100),
#'   C = runif(100)
#' )
#' spdf <- SpatialPointsDataFrame(coords, values)
#' 
#' # Create prediction grid
#' grid <- expand.grid(x = seq(1, 10, 0.5), y = seq(1, 10, 0.5))
#' grid <- SpatialPixelsDataFrame(grid, data.frame(id = 1:nrow(grid)))
#' 
#' # Perform interpolation
#' result <- idw_compositional(spdf, grid, orig = values,
#'                            idp = 2, maxdist = 5, focal_window = 3)
#' }
#' 
#' @seealso 
#' \code{\link[gstat]{idw}}, \code{\link[raster]{focal}}
#' 
#' @importFrom ggplot2 ggplot geom_tile geom_sf scale_fill_viridis_c theme_minimal labs
#' @importFrom purrr map
#' @export
idw_compositional <- function(spdf, grid, orig, idp = idp, maxdist = maxdist, 
                            focal_window = focal_window, plot_idw = TRUE, plot_ncol = 4) {
  # Create an empty data frame to hold results
  result <- data.frame()
  
  # Loop through each column in the data
  for (colname in colnames(spdf)) {
    element <- spdf[[colname]]
    
    # Run IDW interpolation with var1.pred as the dependent variable
    invisible(capture.output(
        idw_result <- idw(element ~ 1, spdf, newdata = grid, idp = idp, maxdist = maxdist)
    ))
    coordinates <- coordinates(idw_result)
    values <- idw_result@data$var1.pred
    
    # Create raster data frame for this element
    raster_data <- data.frame(x = coordinates[, 1], y = coordinates[, 2], value = values)
    raster_result <- rasterFromXYZ(raster_data)
    smoothed_raster <- focal(raster_result, w = matrix(1, nrow = focal_window, ncol = focal_window), fun = mean)
    
    # Convert to SpatialPixelsDataFrame and then to data.frame
    raster_df <- as.data.frame(as(smoothed_raster, "SpatialPixelsDataFrame"))
    
    # Rename the 'layer' column to the element name (colname)
    names(raster_df)[names(raster_df) == "layer"] <- colname
    
    # If it's the first iteration, initialize result with x, y, and the element
    if (nrow(result) == 0) {
      result <- raster_df[, c("x", "y", colname)]
    } else {
      # For subsequent iterations, just add the new column
      result[[colname]] <- raster_df[[colname]]
    }
  }
  
  # Back transform ILR-data to percentages
  percdata <- 100 * as.data.frame(ilrInv(result[, !(names(result) %in% c("x", "y"))], orig = orig))
  coordata <- result[, (names(result) %in% c("x", "y"))]
  result <- cbind(coordata, percdata)
  
  # Plotting function for individual elements
  create_element_plot <- function(element, result) {
    ggplot() +
      geom_tile(
        data = data.frame(x = result$x, y = result$y, value = result[[element]]),
        aes(x = x, y = y, fill = value)
      ) +
      geom_sf(data = structures_utm, color = "black") +
      scale_fill_viridis_c(option = "turbo") +
      theme_minimal() +
      labs(title = element, fill = "Value") +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        panel.spacing = unit(0.5, "lines"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(50, 50, 50, 50),
        legend.title = element_blank(),
        legend.text = element_text(size = 12)
      ) +
      coord_sf(
        xlim = c(min(result$x) - 1, max(result$x) + 1),
        ylim = c(min(result$y) - 1, max(result$y) + 1)
      )
  }
  
  # Create and display plots if requested
  if (plot_idw) {
    elements <- names(result)[!(names(result) %in% c("x", "y"))]
    plot_list <- map(elements, ~create_element_plot(.x, result))
    print(wrap_plots(plot_list, ncol = plot_ncol))
  }
  
  invisible(result)
}