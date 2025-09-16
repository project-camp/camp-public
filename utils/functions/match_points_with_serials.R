#' Match XRF data with spatial points based on serial numbers.
#' 
#' @description
#' Combines XRF data from an Excel file with spatial coordinates from a GeoJSON file
#' by matching serial numbers. The function reports matching statistics and identifies
#' any unmatched serial numbers.
#'
#' @param xlsx_path Character string. Path to Excel file containing XRF data.
#'        Must include a 'Serial' column with numeric identifiers.
#' @param geojson_path Character string. Path to GeoJSON file containing point features.
#'        Points must have a 'Name' property with a 4-digit serial number at the end.
#'
#' @return A data frame containing all columns from the Excel file plus the following
#' spatial coordinates from matched points:
#' \itemize{
#'   \item Easting
#'   \item Northing
#'   \item Longitude
#'   \item Latitude
#'   \item Elevation
#' }
#'
#' @details
#' The function performs the following operations:
#' 1. Reads Excel data and converts serial numbers to character type
#' 2. Reads GeoJSON points and extracts 4-digit serial numbers from point names
#' 3. Joins the data using serial numbers as the matching key
#' 4. Reports statistics about matched and unmatched records
#' 5. Unmatched records will have NA values for spatial coordinates.
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate filter left_join select
#' @importFrom sf st_read st_drop_geometry
#'
#' @author Antonios Koutroumpas, Abel Ruiz-Giralt
#' 
#' @examples
#' \dontrun{
#' # Basic usage
#' result <- match_points_with_serials(
#'   xlsx_path = "XRF_data.xlsx",
#'   geojson_path = "points.geojson"
#' )
#'
#' # Check the first few rows of the result
#' head(result)
#' }
#'
#' @export

match_points_with_serials <- function(xlsx_path, geojson_path) {
  # Input validation
  if (!file.exists(xlsx_path)) {
    stop("Excel file not found: ", xlsx_path)
  }
  if (!file.exists(geojson_path)) {
    stop("GeoJSON file not found: ", geojson_path)
  }
  
  # Read Excel data
  xlsx_data <- read_excel(xlsx_path) %>%
    mutate(Serial = as.character(Serial))
  
  # Check for duplicates in Excel data
  duplicate_serials_excel <- xlsx_data %>%
    group_by(Serial) %>%
    filter(n() > 1)
  if (nrow(duplicate_serials_excel) > 0) {
    cat("Duplicate serials found in Excel data:\n")
    print(duplicate_serials_excel$Serial)
    cat("\n")  
  } else {
    cat("No duplicate serials found in Excel data.\n")
    cat("\n")  
  }
  
  # Read and process spatial points
  points_sf <- st_read(geojson_path, quiet = TRUE)
  
  # Process spatial points - extract serial number from field_1
  points_coords <- points_sf %>%
    mutate(field_1 = gsub("\\s+", " ", field_1)) %>%  
    mutate(Serial = sub(".*SAMP_(\\d+)$", "\\1", field_1)) %>%  
    mutate(Serial = as.character(Serial))
  
  # Extract coordinates and add them as columns
  coords_matrix <- st_coordinates(points_coords)
  
  points_coords <- points_coords %>%
    mutate(
      Easting = field_2,    
      Northing = field_3,   
      Elevation = field_4,
      Longitude = coords_matrix[,1],  # X coordinate from geometry
      Latitude = coords_matrix[,2]    # Y coordinate from geometry
    ) %>%
    st_drop_geometry() %>%  
    dplyr::select(Serial, Easting, Northing, Longitude, Latitude, Elevation)
  
  # Debug: Check if columns were created properly
  cat("Columns in points_coords:\n")
  print(names(points_coords))
  cat("\n")
  
  # Check for duplicates in GeoJSON data
  duplicate_serials_geojson <- points_coords %>%
    group_by(Serial) %>%
    filter(n() > 1)
  if (nrow(duplicate_serials_geojson) > 0) {
    cat("Duplicate serials found in GeoJSON data:\n")
    print(duplicate_serials_geojson$Serial)
    cat("\n")  
  } else {
    cat("No duplicate serials found in GeoJSON data.\n")
    cat("\n")
  }
  
  # Join data
  result <- xlsx_data %>%
    left_join(points_coords, by = "Serial")
  
  # Debug: Check if columns exist in result
  cat("Columns in final result:\n")
  print(names(result))
  cat("\n")
  
  # Report matching statistics
  n_matched <- sum(!is.na(result$Easting))
  cat("Number of records in Excel data: ", nrow(xlsx_data), "\n")
  cat("Number of records in GeoJSON data: ", nrow(points_coords), "\n")
  cat("\n")  
  cat("Matched", n_matched, "out of", nrow(xlsx_data), "records\n")
  
  # Report unmatched serials
  unmatched_serials <- xlsx_data$Serial[!xlsx_data$Serial %in% points_coords$Serial]
  if(length(unmatched_serials) > 0) {
    cat("\nSerials not found in the geojson file:\n")
    print(unmatched_serials)
    cat("\n") 
  } else {
    cat("\nAll serials were found in the geojson file\n")
    cat("\n") 
  }
 
  return(result)
}