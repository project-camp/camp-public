#' Filter dataset based on NA values threshold
#' 
#' @description
#' Filters a dataset by removing columns where the percentage of NA values exceeds
#' a specified threshold. This function helps remove columns with insufficient data.
#'
#' @param dataset data.frame containing the measurements to be filtered
#' @param threshold numeric value (0-100) specifying the maximum allowed percentage of NA values
#'
#' @return A data frame with columns removed where NA percentage exceeds the threshold
#'
#' @details
#' The function performs the following operations:
#' 1. Identifies element and oxide columns using regex pattern
#' 2. Calculates percentage of NA values in each column
#' 3. Removes columns (and their error columns) where NA percentage exceeds threshold
#'
#' @author Antonios Koutroumpas, Abel Ruiz-Giralt
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' filtered_data <- na_filter(
#'   dataset = xrf_data,
#'   threshold = 20
#' )
#' }
#'
#' @export
na_filter <- function(dataset, threshold = 0) {
    # Get element and oxide columns using regex pattern
    cols_dataset <- names(dataset)
    cols_dataset <- grep("^[A-Z][a-z]?$|^[A-Z][a-z]?[0-9]*O[0-9]*$", 
        cols_dataset, value = TRUE)
    
    # Initialize results tracking dataframe
    results <- data.frame(Element = cols_dataset, 
        Total = numeric(length(cols_dataset)), 
        TotalNA = numeric(length(cols_dataset)), 
        Percentage = numeric(length(cols_dataset)), 
        stringsAsFactors = FALSE)
    
    # Track columns that need to be removed
    cols_to_remove <- c()
    
    # Analyze each column for NA values
    for (col_name in cols_dataset) {
        values <- dataset[[col_name]]
        # Calculate NA statistics
        total_count <- length(values)
        na_count <- sum(is.na(values))
        na_percentage <- round(na_count/total_count * 100, 2)
        
        # Store results
        results[results$Element == col_name, ] <- c(col_name, 
            total_count, na_count, na_percentage)
        
        # Mark for removal if NA percentage exceeds threshold
        if (na_percentage >= threshold) {
          error_col_name <- paste(col_name, "Err", sep = " ")
          cols_to_remove <- c(cols_to_remove, col_name, error_col_name)
        }
    }
    
    # Convert numeric columns to proper type
    results$Total <- as.numeric(results$Total)
    results$TotalNA <- as.numeric(results$TotalNA)
    results$Percentage <- as.numeric(results$Percentage)
    
    # Remove columns that exceeded threshold
    dataset <- dataset[, !names(dataset) %in% cols_to_remove]
    return(dataset)
}