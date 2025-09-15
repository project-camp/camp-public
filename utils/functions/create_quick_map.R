#' Create quick static map of sample points and structures
#' 
#' @description
#' Creates a static plot displaying sample points and site layout/structures,
#' with optional outlier, imputed data, group, or cluster highlighting.
#'
#' @param dataset data.frame or sf object. Must contain columns:
#'        Longitude, 
#'        Latitude, 
#'        Serial, 
#'        and classification columns if specific map types are requested
#' @param structures sf object. Site layout/structures to be displayed as polylines
#' @param group_data character. Name of the column containing group information (e.g., outliers, imputed data, groups, clusters)
#'
#' @return A base R plot object showing the sample points and structures
#'
#' @details
#' The function creates two types of maps:
#' 1. Regular sample map (default):
#'    - Blue points for all samples
#'    - Black lines for structures
#' 2. Group map (group_data specified):
#'    - Points colored by group categories
#'    - Black lines for structures
#'
#' @importFrom sf st_as_sf st_geometry
#' @importFrom graphics plot par legend
#' 
#' @author Antonios Koutroumpas, Abel Ruiz-Giralt
#'
#' @examples
#' \dontrun{
#' # Create regular sample map
#' create_quick_map(samples_df, structures_sf)
#'
#' # Create group map
#' create_quick_map(samples_df, structures_sf,
#'                 group_data = "sample_group")
#' }
#'
#' @export
create_quick_map <- function(dataset, structures, 
                            group_data = NULL) {
  # Input validation
  required_cols <- c("Longitude", "Latitude", "Serial")
  if (!all(required_cols %in% names(dataset))) {
    stop("dataset must contain columns: ", paste(required_cols, collapse = ", "))
  }
  
  # Validate group_data column existence
  if (!is.null(group_data) && !group_data %in% names(dataset)) {
    stop("group_data column '", group_data, "' not found in dataset")
  }

  # Verify structures is proper spatial object
  if (!inherits(structures, "sf")) {
    stop("structures must be an sf object")
  }

  par(bg = "white")
  par(mar = c(4, 4, 4, 8))  # Adjust the right margin for legend space
  
  if (!is.null(group_data)) {
    group_points <- st_as_sf(dataset[, c("Longitude", "Latitude", group_data)], 
                      coords = c("Longitude", "Latitude"))
    plot(st_geometry(group_points), col = as.factor(dataset[[group_data]]), pch = 19)
  } else {
    # Default map
    points <- st_as_sf(dataset[, c("Longitude", "Latitude")], 
                      coords = c("Longitude", "Latitude"))
    plot(st_geometry(points), col = 'blue', pch = 19)
  }
  
  # Add structures to all map types
  plot(st_geometry(structures), col = "black", add = TRUE)
}
