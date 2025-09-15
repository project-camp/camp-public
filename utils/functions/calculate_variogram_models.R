#' Calculate Variogram Models for Spatial Data
#'
#' This function calculate variogram models to spatial data, including both direct and cross-variograms.
#' It automatically selects appropriate models based on the data and provides a comprehensive
#' gstat object with fitted models.
#'
#' @param spdf SpatialPointsDataFrame containing the data
#' @param v Variogram object computed from the data
#' @param g Initial gstat object
#' @param threshold Percentage threshold for model filtering (default = 0)
#' @param vgm_models Character vector of variogram model types to try (default = c("Sph", "Exp", "Gau"))
#'
#' @return A gstat object with calculated variogram models
#'
#' @details
#' The function performs the following steps:
#' 1. Calculates direct variograms for each variable
#' 2. Calculates cross-variograms for variable pairs
#' 3. Combines models and filters based on frequency
#' 4. Creates a final gstat object with the selected models
#'
#' @import gstat
#' @import automap
#' 
#' @author Abel Ruiz-Giralt
#'
#' @examples
#' \dontrun{
#' # Create initial gstat object
#' g <- gstat(id = "var1", formula = var1 ~ 1, data = spdf)
#' # Compute variogram
#' v <- variogram(g, width = lag_dist/2, cutoff = site_diag/3, cross = TRUE)
#' # Calculate gstat models
#' g_with_models <- calculate_variogram_models(spdf, v, g)
#' }
#'
#' @export
calculate_variogram_models <- function(spdf = spdf, gstat = g, variogram = v, 
                                       vgm_models = c("Sph", "Exp", "Gau"), 
                                       threshold = 0) {
  
  # Initialize empty lists to store the fitted variogram models
  # Direct variograms are stored in fitted_variograms
  # Cross variograms are stored in fitted_cross_variograms
  fitted_variograms <- list()
  fitted_cross_variograms <- list()

  # Extract variable names from the spatial data frame
  variables <- names(spdf@data)

  # Step 1: Fit direct variograms for each variable
  # These describe spatial correlation within each variable
  for (var in variables) {
    # Create formula for current variable
    formula <- as.formula(paste(var, "~ 1"))
    
    # Extract initial parameters from empirical variogram
    nugget_value <- v$gamma[v$id == paste(var)][1]  # Initial variance at h=0
    sill_value <- max(v$gamma[v$id == paste(var)])  # Maximum variance
    range_value <- v$dist[which.min(abs(v$gamma[v$id == paste(var)] - sill_value))]  # Distance where variance stabilizes
    
    # Fit variogram model using automap package
    fitted_variograms[[var]] <- autofitVariogram(formula, spdf, model = vgm_models)
  }

  # Step 2: Fit cross-variograms between pairs of variables
  # These describe spatial correlation between different variables
  processed_combinations <- character()  # Track which pairs have been processed
  for (i in 1:(length(variables) - 1)) {
    for (j in (i + 1):length(variables)) {
      var1 <- variables[i]
      var2 <- variables[j]
      combination1 <- paste(var1, ".", var2, sep = "")
      combination2 <- paste(var2, ".", var1, sep = "")

      # Check if this pair has already been processed
      if (!(combination1 %in% processed_combinations || 
            combination2 %in% processed_combinations)) {
        
        # Create cross-variogram formula and extract initial parameters
        cross_formula <- as.formula(paste(var1, "~", var2))
        nugget_value <- v$gamma[v$id == combination1][1]
        sill_value <- max(v$gamma[v$id == combination1])
        range_value <- v$dist[which.min(abs(v$gamma[v$id == combination1] - sill_value))]
        
        # Fit cross-variogram model
        fitted_cross_variograms[[combination1]] <- autofitVariogram(
          cross_formula, spdf, model = vgm_models
        )
        processed_combinations <- c(processed_combinations, combination1, combination2)
      }
    }
  }

  # Step 3: Rebuild gstat object with all fitted models (using Helper Function, see below)
  gg <- rebuild_gstat_object(fitted_variograms, fitted_cross_variograms, spdf)

  # Step 4: Process and filter models
  # Combine all fitted models into one dataframe
  combined_df <- do.call(rbind, gg$model)
  # Split by model type (Sph, Exp, Gau, etc.)
  split_data <- split(combined_df, combined_df$model)
  
  # Calculate median parameters for each model type
  grouped_medians <- lapply(split_data, function(df) {
    apply(df[, c("psill", "range", "kappa")], 2, median)
  })
  
  # Create summary dataframe with model frequencies
  grouped_medians_df <- as.data.frame(do.call(rbind, grouped_medians))
  rownames(grouped_medians_df) <- names(grouped_medians)
  grouped_medians_df$n <- as.vector(table(combined_df$model)[rownames(grouped_medians_df)])

  # Step 5: Filter models based on frequency threshold and print
  fitted_models <- grouped_medians_df[grouped_medians_df$n > 0, ]
  total_n <- sum(fitted_models$n)
  thres <- total_n * threshold
  filtered_models <- fitted_models[fitted_models$n >= thres, ]
  ordered_models <- filtered_models[order(-filtered_models$n), ]
  cat("Selected models:\n")
  print(ordered_models)

  # Step 6: Create final model combination
  # Start with nugget effect model
  nugget_model <- vgm(
    psill = ordered_models$psill[1],
    model = rownames(ordered_models)[1],
    range = ordered_models$range[1],
    kappa = ordered_models$kappa[1]
  )
  
  # Add remaining models
  model_list <- lapply(2:nrow(ordered_models), function(i) {
    vgm(
      psill = ordered_models$psill[i],
      model = rownames(ordered_models)[i],
      range = ordered_models$range[i],
      kappa = ordered_models$kappa[i]
    )
  })

  # Step 7: Update original gstat object with final models
  g <- gstat(g, model = rbind(nugget_model, do.call(rbind, model_list)),
           fill.all = TRUE)
  # Return final gstat object
  return(g)
}

#' Rebuild gstat object (Helper Function)
#'
#' @param fitted_variograms List of fitted direct variograms
#' @param fitted_cross_variograms List of fitted cross-variograms
#' @param spdf SpatialPointsDataFrame containing the data
#' @return Updated gstat object
#' @keywords internal

rebuild_gstat_object <- function(fitted_variograms, fitted_cross_variograms, spdf) {
  # Step 1: Initialize gstat object with first variable
  gg <- gstat(
    id = names(fitted_variograms)[1],
    formula = as.formula(paste(names(fitted_variograms)[1], " ~ 1")),
    model = fitted_variograms[[names(fitted_variograms)[1]]]$var_model,
    data = spdf
  )
  
  # Step 2: Add remaining variables to gstat object
  for (i in 2:length(fitted_variograms)) {
    gg <- gstat(
      gg, 
      id = names(fitted_variograms)[i],
      formula = as.formula(paste(names(fitted_variograms)[i], " ~ 1")),
      model = fitted_variograms[[names(fitted_variograms)[i]]]$var_model,
      data = spdf
    )
  }
  
  # Step 3: Add all cross-variograms to gstat object
  n_cvgs <- length(fitted_variograms)
  for (i in 1:(n_cvgs - 1)) {
    for (j in (i + 1):n_cvgs) {
      # Construct cross-variogram name
      cross_name <- paste(names(fitted_variograms)[i], ".", 
                         names(fitted_variograms)[j], sep = "")
      
      # Add cross-variogram to gstat object
      gg <- gstat(
        gg, 
        id = c(names(fitted_variograms)[i], names(fitted_variograms)[j]),
        model = fitted_cross_variograms[[cross_name]]$var_model,
        data = spdf
      )
    }
  }
  
  return(gg)
}
