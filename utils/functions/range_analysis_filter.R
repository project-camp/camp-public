#' Filter dataset based on analysis ranges and threshold
#'
#' @description
#' Filters the dataset by removing columns where the percentage of values outside the specified range
#' exceeds the given threshold. The function helps identify and remove unreliable measurements.
#'
#' @param dataset data.frame containing measurements to be filtered
#' @param analysis_ranges list containing range values for different methods.
#'        Each method has min and max acceptable values for each element
#' @param method character string specifying the analysis method ("oxide", "sulfide", or other)
#' @param threshold numeric value (0-100) specifying the maximum allowed percentage of out-of-range values
#'
#' @return A data frame with columns removed where out-of-range values exceed the threshold
#'
#' @details
#' The function performs the following operations:
#' 1. Selects appropriate ranges based on the analysis method
#' 2. For each column, calculates percentage of values outside specified ranges
#' 3. Removes columns (and their error columns) where out-of-range percentage exceeds threshold
#'
#' @author Antonios Koutroumpas, Abel Ruiz-Giralt
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' filtered_data <- range_analysis_filter(
#'   dataset = xrf_data,
#'   analysis_ranges = ranges_list,
#'   method = "oxide",
#'   threshold = 10
#' )
#' }
#'
#' @export
range_analysis_filter <- function(dataset, analysis_ranges, method, threshold = 0) {
    # Select appropriate range list based on analysis method
    if (method == "oxide") {
      range_list <- analysis_ranges$oxide_method
    }
    else if (method == "sulfide") {
      range_list <- analysis_ranges$sulfide_method
    }
    else {
      range_list <- analysis_ranges
    }
  
    # Get columns from dataset and ranges list for comparison
    cols_dataset <- names(dataset)
    cols_lod <- names(range_list)

    # Initialize results tracking dataframe
    results <- data.frame(
      Element = cols_lod,
      NTotal = NA,
      OutsideRange = NA,
      Percentage = NA,
      stringsAsFactors = FALSE
    )
    
    # Track columns that need to be removed
    cols_to_remove <- c()
    
    # Analyze each column in the dataset
    for (col_name in cols_dataset) {
      if (col_name %in% cols_lod) {
        values <- dataset[[col_name]]
        range_vals <- range_list[[col_name]]
        
        # Calculate statistics for values outside range
        if (!is.null(range_vals)) {
          # Count values outside the specified range
          out_of_range_count <- sum(values < range_vals[1] | values > range_vals[2], na.rm = TRUE)
          n_count <- length(na.omit(values))
          out_of_range_per <- round(out_of_range_count / n_count * 100, 2)
          
          # Mark column for removal if it exceeds threshold
          if (!is.na(out_of_range_per) && out_of_range_per >= threshold) {
            error_col_name <- paste(col_name, "Err", sep = " ")
            cols_to_remove <- c(cols_to_remove, col_name, error_col_name)
          }
        } else {
          out_of_range_count <- NA
          n_count <- NA
          out_of_range_per <- NA
        }
        
        # Update results tracking
        results[results$Element == col_name, "NTotal"] <- n_count
        results[results$Element == col_name, "OutsideRange"] <- out_of_range_count
        results[results$Element == col_name, "Percentage"] <- out_of_range_per
      }
    }
    
    # Remove columns that exceeded threshold
    dataset <- dataset[, !names(dataset) %in% cols_to_remove]
    return(dataset)
}

