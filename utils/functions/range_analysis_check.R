#' Check dataset against analysis ranges
#' 
#' @description
#' Analyzes a dataset to determine the count and percentage of values that fall outside
#' specified analysis ranges for each column. This function helps assess data quality.
#'
#' @param dataset data.frame containing measurements to be checked
#' @param analysis_ranges list containing range values for different methods.
#'        Each method has min and max acceptable values for each element
#' @param method character string specifying the analysis method ("oxide", "sulfide", or other)
#'
#' @return A data frame containing statistics for each column:
#'         Element (column name), N (total values), OutsideRange (count), Percentage
#'
#' @details
#' The function performs the following operations:
#' 1. Selects appropriate ranges based on the analysis method
#' 2. For each column, counts values outside specified ranges
#' 3. Calculates percentage of out-of-range values
#'
#' @author Antonios Koutroumpas, Abel Ruiz-Giralt
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' range_stats <- range_analysis_check(
#'   dataset = xrf_data,
#'   analysis_ranges = ranges_list,
#'   method = "oxide"
#' )
#'
#' # View results
#' print(range_stats)
#' }
#'
#' @export
range_analysis_check <- function(dataset, analysis_ranges, method) {
    # Choose XRF analysis method, if any
    if (method == "oxide") {
      range_list <- analysis_ranges$oxide_method
    }
    else if (method == "sulfide") {
      range_list <- analysis_ranges$sulfide_method
    }
    else {
      range_list <- analysis_ranges
    }
  
    # Get the column names of the dataset and the LOD list
    cols_dataset <- names(dataset)
    cols_lod <- names(range_list)

    # Initialize results dataframe
    results <- data.frame(
      Element = cols_dataset[cols_dataset %in% cols_lod],
      NTotal = NA,
      OutsideRange = NA,
      Percentage = NA,
      stringsAsFactors = FALSE
    )
    
    # Loop through all columns of the dataset
    for (col_name in cols_dataset) {
      # Check if the column contains numeric data (and ignore non-numeric columns)
      if (col_name %in% cols_lod) {
        values <- dataset[[col_name]]
        range_vals <- range_list[[col_name]]
        # Check percentage of values outside range
        if (!is.null(range_vals)) {
          out_of_range_count <- sum(values < range_vals[1] | values > range_vals[2], na.rm = TRUE)
          n_count <- length(na.omit(values))
          out_of_range_per <- round(out_of_range_count / n_count * 100, 2)
        } else {
          out_of_range_count <- NA
          n_count <- NA
          out_of_range_per <- NA
        }
        
        # Store results
        results[results$Element == col_name, "NTotal"] <- n_count
        results[results$Element == col_name, "OutsideRange"] <- out_of_range_count
        results[results$Element == col_name, "Percentage"] <- out_of_range_per
      }
    }
    
    return(results)
}

