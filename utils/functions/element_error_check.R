#' Check dataset for measurement error values
#' 
#' @description
#' Analyzes a dataset to determine the count and percentage of values that fall below
#' 3 times their error for each column. This function helps assess measurement quality.
#'
#' @param dataset data.frame containing measurements and their errors.
#'        Error columns should be named as "<element> Err"
#'
#' @return A data frame containing statistics for each column:
#'         Element, Total (count), Below3xError (count), PercentageBelow3xError
#'
#' @details
#' The function performs the following operations:
#' 1. Identifies element and oxide columns using regex pattern
#' 2. Compares each value to 3 times its associated error
#' 3. Calculates statistics for values below this threshold
#'
#' @author Antonios Koutroumpas, Abel Ruiz-Giralt
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' error_stats <- element_error_check(xrf_data)
#'
#' # View results
#' print(error_stats)
#' }
#'
#' @export

element_error_check <- function(dataset) {
  # Analyzes a dataset to determine the count and percentage of values that fall below
  # 3 times their error for each column. This function helps assess measurement quality.

  # Get the column names of the dataset
  cols_dataset <- names(dataset)
  cols_dataset <- grep("^[A-Z][a-z]?$|^[A-Z][a-z]?[0-9]*O[0-9]*$", cols_dataset, value = TRUE)

  # Initialize results dataframe
  results <- data.frame(
    Element = cols_dataset,
    NTotal = numeric(length(cols_dataset)),
    Below3xError = numeric(length(cols_dataset)),
    Percentage = numeric(length(cols_dataset)),
    stringsAsFactors = FALSE
  )

  # Loop through all columns of the dataset
  for (col_name in cols_dataset) {
    values <- dataset[[col_name]]

    # Check for values below 3 times their error
    error_col_name <- paste(col_name, "Err", sep = " ")
    if (error_col_name %in% names(dataset)) {
      error_values <- dataset[[error_col_name]]
      total_count <- length(na.omit(values))
      below_3x_error_count <- sum(values < 3 * error_values, na.rm = TRUE)
      below_3x_error_percentage <- round(below_3x_error_count / total_count * 100, 2)
    } else {
      total_count <- NA
      below_3x_error_count <- NA
      below_3x_error_percentage <- NA
    }

    # Store results
    results[results$Element == col_name, ] <- c(col_name, total_count, below_3x_error_count, below_3x_error_percentage)
  }

  # Convert numeric columns to numeric type
  results$NTotal <- as.numeric(results$NTotal)
  results$Below3xError <- as.numeric(results$Below3xError)
  results$Percentage <- as.numeric(results$Percentage)

  return(results)
}



