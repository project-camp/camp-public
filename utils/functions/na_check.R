#' Check dataset for NA values
#' 
#' @description
#' Analyzes a dataset to determine the count and percentage of NA values in each column.
#' This function helps assess data completeness.
#'
#' @param dataset data.frame containing the measurements to be checked
#'
#' @return A data frame containing statistics for each column:
#'         Element (column name), Total (count), TotalNA (NA count), Percentage
#'
#' @details
#' The function performs the following operations:
#' 1. Identifies element and oxide columns using regex pattern
#' 2. Counts total and NA values in each column
#' 3. Calculates percentage of NA values
#'
#' @author Antonios Koutroumpas, Abel Ruiz-Giralt
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' na_stats <- na_check(xrf_data)
#'
#' # View results
#' print(na_stats)
#' }
#'
#' @export
na_check <- function(dataset) {
    # Get column names from dataset
    cols_dataset <- names(dataset)
    
    # Filter columns that match element patterns (e.g., Fe, Na2O)
    cols_dataset <- grep("^[A-Z][a-z]?$|^[A-Z][a-z]?[0-9]*O[0-9]*$", 
        cols_dataset, value = TRUE)
    
    # Initialize results data frame
    results <- data.frame(
        Element = cols_dataset,
        Total = numeric(length(cols_dataset)), 
        TotalNA = numeric(length(cols_dataset)),
        Percentage = numeric(length(cols_dataset)), 
        stringsAsFactors = FALSE
    )
    
    # Calculate statistics for each column
    for (col_name in cols_dataset) {
        # Extract column values
        values <- dataset[[col_name]]
        
        # Calculate metrics
        total_count <- length(values)
        na_count <- sum(is.na(values))
        na_percentage <- round(na_count/total_count * 100, 2)
        
        # Update results data frame
        results[results$Element == col_name, ] <- c(col_name, 
            total_count, na_count, na_percentage)
    }
    
    # Convert columns to numeric type
    results$Total <- as.numeric(results$Total)
    results$TotalNA <- as.numeric(results$TotalNA)
    results$Percentage <- as.numeric(results$Percentage)
    
    return(results)
}