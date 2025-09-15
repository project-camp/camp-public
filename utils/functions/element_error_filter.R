#' Filter dataset based on measurement error threshold
#' 
#' @description
#' Filters a dataset by removing columns where the percentage of values below 3 times
#' their error exceeds a specified threshold. This function helps remove unreliable measurements.
#'
#' @param dataset data.frame containing measurements and their errors.
#'        Error columns should be named as "<element> Err"
#' @param threshold numeric value (0-100) specifying the maximum allowed percentage of
#'        values below 3 times their error
#'
#' @return A data frame with columns removed where values below 3x error exceed the threshold
#'
#' @details
#' The function performs the following operations:
#' 1. Identifies element and oxide columns using regex pattern
#' 2. Compares each value to 3 times its associated error
#' 3. Removes columns where too many values fall below this threshold
#'
#' @author Antonios Koutroumpas, Abel Ruiz-Giralt
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' filtered_data <- element_error_filter(
#'   dataset = xrf_data,
#'   threshold = 15
#' )
#' }
#'
#' @export

element_error_filter <- function(dataset, threshold = 0) {
  # Get element and oxide columns using regex pattern
  cols_dataset <- names(dataset)
  cols_dataset <- grep("^[A-Z][a-z]?$|^[A-Z][a-z]?[0-9]*O[0-9]*$", cols_dataset, value = TRUE)

  # Initialize results tracking dataframe
  results <- data.frame(
    Element = cols_dataset,
    NTotal = numeric(length(cols_dataset)),
    Below3xError = numeric(length(cols_dataset)),
    Percentage = numeric(length(cols_dataset)),
    stringsAsFactors = FALSE
  )

  # Track columns that need to be removed
  cols_to_remove <- c()

  # Analyze each column for error values
  for (col_name in cols_dataset) {
    values <- dataset[[col_name]]

    # Get corresponding error column and check values
    error_col_name <- paste(col_name, "Err", sep = " ")
    if (error_col_name %in% names(dataset)) {
      error_values <- dataset[[error_col_name]]
      # Count values below 3x error threshold
      total_count <- length(na.omit(values))
      below_3x_error_count <- sum(values < 3 * error_values, na.rm = TRUE)
      below_3x_error_percentage <- round(below_3x_error_count / total_count * 100, 2)
    } else {
      total_count <- NA
      below_3x_error_count <- NA
      below_3x_error_percentage <- NA
    }

    # Mark for removal if percentage exceeds threshold
    if (below_3x_error_percentage >= threshold) {
      cols_to_remove <- c(cols_to_remove, col_name, error_col_name)
    }

    # Store results
    results[results$Element == col_name, ] <- c(col_name, total_count, 
        below_3x_error_count, below_3x_error_percentage)
  }

  # Convert numeric columns to proper type
  results$NTotal <- as.numeric(results$NTotal)
  results$Below3xError <- as.numeric(results$Below3xError)
  results$Percentage <- as.numeric(results$Percentage)

  # Remove columns that exceeded threshold
  dataset <- dataset[, !names(dataset) %in% cols_to_remove]

  return(dataset)
}