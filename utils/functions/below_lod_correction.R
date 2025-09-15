#' Correct values below Limit of Detection (LOD)
#' 
#' @description
#' Processes geochemical data to handle values below the Limit of Detection (LOD).
#' Values below LOD thresholds are replaced with NA based on the specified method.
#'
#' @param dataset data.frame containing the geochemical measurements. Should contain
#'        chemical elements as columns with their corresponding measurements
#' @param lod_values list containing LOD thresholds in ppm. Can be either:
#'        - A simple list with element names and their LOD values
#'        - A nested list with $oxide_method and $sulfide_method sublists
#' @param method character. Specifies which LOD values to use:
#'        - "oxide": use oxide_method LOD values
#'        - "sulfide": use sulfide_method LOD values
#'        - NULL: use direct LOD values from lod_values
#'
#' @return A list containing:
#'   \item{data}{data frame with values below LOD replaced by NA}
#'   \item{changes}{data frame summarizing changes per element:
#'     \itemize{
#'       \item{element}{Element name}
#'       \item{below_lod}{Number of values found below LOD}
#'       \item{total}{Total number of non-NA values}
#'       \item{percentage}{Percentage of values below LOD}
#'     }
#'   }
#'
#' @details
#' The function performs the following operations:
#' 1. Selects appropriate LOD values based on method
#' 2. Converts values to numeric format
#' 3. Applies LOD thresholds (LOD values are divided by 10000 to convert from ppm to percentage)
#' 4. Replaces values below threshold with NA
#' 
#' Special cases:
#' - For oxide method, SiO2 is assigned a very low LOD value (0.1 ppm) to avoid NA errors
#' - The function assumes input values are in percentage while LOD values are in ppm
#' 
#' @author Antonios Koutroumpas, Abel Ruiz-Giralt
#'
#' @examples
#' \dontrun{
#' # Example data
#' data <- data.frame(
#'   Fe = c(0.001, 0.5, 1.2),
#'   Cu = c(0.0005, 0.3, 0.8)
#' )
#' 
#' # LOD values in ppm
#' lod <- list(
#'   oxide_method = list(Fe = 50, Cu = 30),
#'   sulfide_method = list(Fe = 100, Cu = 50)
#' )
#' 
#' # Using oxide method
#' result <- below_lod_correction(data, lod, method = "oxide")
#' 
#' # Using sulfide method
#' result <- below_lod_correction(data, lod, method = "sulfide")
#' }
#'
#' 
#' @export

below_lod_correction <- function(dataset, lod_values, method) {
    #Processes geochemical data to handle values below the Limit of Detection (LOD).
    #Converts ">LOD" markers to NA and applies LOD thresholds to data based on specified method.

    # Choose method for below LOD substitution
    if (method == "oxide") {
      lod_list <- lod_values$oxide_method
      lod_list$SiO2 <- 0.1 # Imputing a very low Silicon LOD value (0.1 ppm) to avoid NA errors
    }
    else if (method == "sulfide") {
      lod_list <- lod_values$sulfide_method
    }
    else {
      lod_list <- lod_values
    }

    # Get the column names of the dataset and the LOD list
    cols_dataset <- names(dataset)
    cols_lod <- names(lod_list)

    # Track if any changes were made
    any_changes <- FALSE

    # Loop through all columns of the dataset
    for (col_name in cols_dataset) {
    # Check if the column contains numeric data (and ignore non-numeric columns)
    if (col_name %in% cols_lod) {
      # Convert columns to numeric
      dataset[[col_name]] <- as.numeric(as.character(dataset[[col_name]]))
      # Apply the LOD threshold substitution
      threshold <- lod_list[[col_name]] / 10000  # Get the LOD value for the column in percentage
      before_change <- sum(dataset[[col_name]] < threshold, na.rm = TRUE)  # Count values below threshold
      dataset[[col_name]] <- ifelse(dataset[[col_name]] < threshold, NA, dataset[[col_name]])
      
      # Print a message about changes
      if (before_change > 0) {
        cat(paste(col_name, ":",
                    before_change, "values below LOD were replaced with NA.\n"))
        any_changes <- TRUE
        }
    }
  }
  if (!any_changes) {
    cat("No values below LOD were found in the dataset.\n")
  }
  return(dataset)
}