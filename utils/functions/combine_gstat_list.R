#' Combine Multiple Gstat Objects with Anisotropic Models
#' 
#' @description 
#' Combines multiple gstat objects containing variogram models fitted at different angles
#' to create a comprehensive anisotropic model. This function is particularly useful
#' for handling geometric anisotropy in spatial data.
#' 
#' @param g A gstat object to store the combined models. Should be initialized with
#'          the same variables as the input gstat_list objects.
#' @param gstat_list List of gstat objects, each containing variogram models fitted
#'                   at different angles. All objects should have the same variables.
#' @param alpha Numeric vector of angles (in degrees) used in the original models.
#'             Default is c(0, 45, 90, 135).
#' 
#' @return A gstat object containing:
#'   \item{model}{List of combined variogram models with anisotropy parameters}
#'   \item{data}{Original data from input gstat object}
#'
#' @details
#' The function processes each variable pair in the input gstat objects and combines
#' their models while preserving the anisotropic characteristics. For each model:
#' - Angles are assigned from the input alpha vector
#' - Anisotropy ratios are calculated based on range parameters
#' - Nugget effects are assigned an anisotropy ratio of 1
#' - Range parameters are normalized relative to the maximum range
#'
#' @note The anisotropy parameters are automatically calculated based on the relative
#' ranges of the variogram models at different angles.
#'
#' @seealso 
#' \code{\link[gstat]{gstat}}, \code{\link[gstat]{vgm}}
#'
#' @examples
#' \dontrun{
#' # Create gstat objects for different directions
#' g0 <- gstat(formula = z~1, data = measurements)
#' g45 <- gstat(formula = z~1, data = measurements)
#' g90 <- gstat(formula = z~1, data = measurements)
#' 
#' # Fit variogram models
#' g0 <- fit.variogram(vario0, model = vgm(psill = 1, model = "Sph", range = 100))
#' g45 <- fit.variogram(vario45, model = vgm(psill = 1, model = "Sph", range = 150))
#' g90 <- fit.variogram(vario90, model = vgm(psill = 1, model = "Sph", range = 200))
#' 
#' # Initialize empty gstat object
#' g_combined <- gstat(formula = z~1, data = measurements)
#' 
#' # Combine models
#' g_result <- combine_gstat_list(g_combined, list(g0, g45, g90), 
#'                               alpha = c(0, 45, 90))
#' }
#'
#' @importFrom stats na.omit
#' @export
combine_gstat_list <- function(g, gstat_list, alpha = c(0, 45, 90, 135)) {

  # Get variable names from first gstat object
  var_names <- names(gstat_list[[1]]$data)
  
  # For each variable pair, combine models with anisotropy
  for(i in 1:length(var_names)) {
    for(j in i:length(var_names)) {
      var1 <- var_names[i]
      var2 <- var_names[j]
      
      model_name <- if(i == j) var1 else paste(var1, var2, sep=".")
      
      # Initialize empty combined model with required structure for gstat
      combined_model <- data.frame(
        model = character(),    # Variogram model type (e.g., Sph, Exp, Nug)
        psill = numeric(),      # Partial sill parameter
        range = numeric(),      # Range parameter
        ang1 = numeric(),       # Anisotropy angle
        anis1 = numeric(),      # Anisotropy ratio
        stringsAsFactors = FALSE
      )
      
      # Add models from all angles
      for(angle_idx in 1:length(alpha)) {
        gstat_obj <- gstat_list[[angle_idx]]
        current_model <- gstat_obj$model[[model_name]]
        
        # Add angle
        current_model$ang1 <- alpha[angle_idx]
        
        # Add 0.5 anisotropy ratio
        current_model$anis1 <- ifelse(current_model$model == "Nug", 1.0, 0.5)
  
        # Combine with previous models
        combined_model <- rbind(combined_model, current_model)
      }
      
      # Store the combined model
      if(i == j) {
        g$model[[var1]] <- combined_model
      } else {
        g$model[[paste(var1, var2, sep=".")]] <- combined_model
      }
    }
  }
  
  # Recalculate anisotropy ratios based on range values
  g$model <- lapply(g$model, function(model_data) {
    # Group models by type (Sph, Exp, etc.)
    split_data <- split(model_data, model_data$model)
    
    # For each model type, calculate anisotropy ratio relative to max range
    result <- do.call(rbind, lapply(split_data, function(group) {
      max_range <- suppressWarnings(max(group$range, na.rm = TRUE))
      # Ratio is 1 for nugget effect, otherwise range/max_range
      group$anis1 <- suppressWarnings(ifelse(group$range == 0, 1, (group$range / max_range)))
      return(group)
    }))
    
    # Restore original row order and clean up
    result <- result[order(as.numeric(row.names(result))), ]
    rownames(result) <- NULL
    return(result)
  })
  
  return(g)
}