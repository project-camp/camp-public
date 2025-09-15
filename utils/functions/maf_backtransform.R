#' Back Transform MAF Scores
#' 
#' This function performs back transformation of Maximum Autocorrelation Factor (MAF) 
#' scores to their original space using the inverse loadings and center from the MAF model.
#' 
#' @param maf_result A list containing MAF model components, must include:
#'   \itemize{
#'     \item invLoadings - The inverse loadings matrix
#'     \item center - The center (mean) vector used in MAF transformation
#'   }
#' @param newdata A matrix or data frame containing the MAF scores to be back-transformed.
#'   The number of columns should match the number of components in the MAF model.
#' 
#' @return A data frame containing the back-transformed values in the original space.
#'   The column names will match those of the original variables.
#' 
#' @examples
#' \dontrun{
#' # Assuming 'maf_model' is a fitted MAF model and 'scores' contains MAF scores
#' original_data <- maf_backtransform(maf_model, scores)
#' }
#' 
#' @export
maf_backtransform <- function(maf_result, newdata) {
    
    # Prepare data
    scores <- newdata
    loadings <- maf_result$invLoadings[1:ncol(newdata), ] # Loadings from the MAF model
    center <- maf_result$center # Center from MAF model

    # Back transform:
    Z <- scores %*% unclass(loadings) # Back-transformed MAF scores
    Z2 <- as.data.frame(Z + outer(rep(1, nrow(Z)), center)) # Add the center to the back-transformed MAF scores
    colnames(Z2) <- names(center)

    # Return the back-transformed MAF scores
    return(Z2)
}