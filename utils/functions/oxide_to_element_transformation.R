#' Transform oxide values to elemental values
#' 
#' @description
#' Converts geochemical measurements and associated error values from oxide form to elemental form using
#' predefined conversion factors. The function creates new columns for elemental values and optionally
#' their errors, removing the original oxide columns.
#'
#' @param dataset data.frame containing oxide measurements and their errors.
#'        Error columns should be named as "<oxide> Err"
#' @param oxide_factors data.frame containing conversion factors with columns:
#'        Oxide (oxide names), Element (element names), Factor (conversion factors)
#' @param convert_errors logical, whether to convert error columns. If TRUE, error columns
#'        named "<oxide> Err" will be converted to "<element> Err". Default is FALSE.
#'
#' @return A data frame containing the elemental values and optionally their errors,
#'         with oxide columns removed. If convert_errors is TRUE, both oxide and error
#'         columns are removed and replaced with element equivalents.
#'
#' @details
#' The function performs the following operations:
#' 1. Identifies oxide columns in the dataset
#' 2. Converts oxide values to element values using conversion factors
#' 3. If convert_errors=TRUE, transforms associated error values using the same factors
#' 4. Creates new columns for elements and optionally their errors
#' 5. Removes original oxide columns (and error columns if convert_errors=TRUE)
#'
#' @author Antonios Koutrompas, Abel Ruiz-Giralt
#' 
#' @examples
#' \dontrun{
#' # Basic usage without error conversion
#' result <- oxide_to_element_transformation(
#'   dataset = xrf_data,
#'   oxide_factors = factors_df
#' )
#'
#' # Usage with error conversion
#' result_with_errors <- oxide_to_element_transformation(
#'   dataset = xrf_data,
#'   oxide_factors = factors_df,
#'   convert_errors = TRUE
#' )
#' }
#'
#' @export

oxide_to_element_transformation <- function(dataset, oxide_factors, convert_errors = FALSE) {
  
  # Get the column names of the dataset and the oxide factors
  cols_dataset <- names(dataset)
  cols_oxide <- oxide_factors$Oxide

  # Track if any oxide is not found in the dataset
  not_found_oxides <- setdiff(cols_oxide, cols_dataset)
  
  # Print a message for oxides that were not found
  if (length(not_found_oxides) > 0) {
    message("The following oxides were not found in the dataset: ", paste(not_found_oxides, collapse = ", "))
  }
  
  if(convert_errors) {
  # Loop through all columns of the dataset
  for (col_name in cols_dataset) {
    # Check if the column contains oxide data (and ignore non-oxide columns)
    if (col_name %in% cols_oxide) {
      # Calculate value conversion and store in new column
      element_name <- oxide_factors$Element[oxide_factors$Oxide == col_name]
      dataset[[element_name]] <- dataset[[col_name]] * oxide_factors$Factor[oxide_factors$Oxide == col_name]
      # Calculate error conversion and store in a new colum
      element_error_name <- paste0(element_name, " Err")
      oxide_error_name <- paste0(col_name, " Err")
      dataset[[element_error_name]] <- dataset[[oxide_error_name]] * oxide_factors$Factor[oxide_factors$Oxide == col_name]

      # Print a message indicating successful transformation
      message(paste(col_name, "to", element_name, "successfully transformed."))
    }
  }

  # Remove oxide columns
  oxide_cols_to_remove <- c(cols_oxide, paste0(cols_oxide, " Err"))
  dataset <- dataset[, !(names(dataset) %in% oxide_cols_to_remove)]

  } else {
      # Loop through all columns of the dataset
  for (col_name in cols_dataset) {
    # Check if the column contains oxide data (and ignore non-oxide columns)
    if (col_name %in% cols_oxide) {
      # Calculate value conversion and store in new column
      element_name <- oxide_factors$Element[oxide_factors$Oxide == col_name]
      dataset[[element_name]] <- dataset[[col_name]] * oxide_factors$Factor[oxide_factors$Oxide == col_name]
      # Print a message indicating successful transformation
      message(paste(col_name, "to", element_name, "successfully transformed."))
    }
  }

  # Remove oxide columns
  dataset <- dataset[, !(names(dataset) %in% cols_oxide)]

}

return(dataset)

}


