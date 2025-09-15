#' Calculate Directional Variogram Models
#'
#' @description
#' Fits variogram models for both direct and cross-variograms in different directions.
#' The function tries multiple theoretical variogram models and selects the best fit
#' based on the smallest sum of squared errors.
#'
#' @param spdf SpatialPointsDataFrame containing the data
#' @param gstat Initial gstat object to be modified
#' @param variogram_list List of experimental variograms for different directions
#' @param vgm_models Character vector of variogram model types to try (default: c("Sph", "Exp", "Gau"))
#' @param threshold Minimum frequency threshold for model selection (default: 0)
#'
#' @return List of gstat objects containing fitted variogram models for each direction
#'
#' @details
#' The function performs the following steps:
#' 1. Fits direct variograms for each variable
#' 2. Fits cross-variograms for each pair of variables
#' 3. Selects the best fitting model based on SSErr
#' 4. Combines models into a gstat object for each direction
#'
#' @examples
#' # gstat_models <- calculate_directional_variogram_models(
#' #   spdf = my_spdf,
#' #   gstat = g,
#' #   variogram_list = my_variograms,
#' #   vgm_models = c("Sph", "Exp")
#' # )
#'
calculate_directional_variogram_models <- function(spdf = spdf, gstat = g, variogram_list = list, 
                                                 vgm_models = c("Sph", "Exp", "Gau"), 
                                                 threshold = 0) {
  
  # Step 1: Initialize storage structures for results
  gstat_list <- list()  # Store final gstat objects for each direction
  fitted_variograms <- list()  # Store direct variogram fits
  fitted_cross_variograms <- list()  # Store cross-variogram fits

  # Step 2: Extract variables from spatial data frame for processing
  variables <- names(spdf@data)

  # Step 3: Process each directional variogram in the input list
  for (dir_idx in seq_along(variogram_list)) {
    current_variogram <- variogram_list[[dir_idx]]
    variogram_name <- names(variogram_list)[dir_idx]
    processed_combinations <- character()

    if (is.null(fitted_variograms[[variogram_name]])) {
        fitted_variograms[[variogram_name]] <- list()
        fitted_cross_variograms[[variogram_name]] <- list()
    }

    for (var in variables) {
        formula <- as.formula(paste(var, "~ 1"))
        nugget_value <- current_variogram$gamma[current_variogram$id == paste(var)][1]
        sill_value <- max(current_variogram$gamma[current_variogram$id == paste(var)])
        range_value <- current_variogram$dist[which.min(abs(current_variogram$gamma[current_variogram$id == paste(var)] - sill_value))]
    
        # Try different models
        models <- vgm_models
        fits <- list()
        SSerr <- numeric(length(models))
    
        for (model_idx in seq_along(models)) {
            initial_model <- vgm(
                psill = max(0, sill_value - nugget_value),
                model = models[model_idx],
                range = range_value,
                nugget = nugget_value,
                anis = c(as.numeric(current_variogram$dir.hor[1]), 0.5))
                
                fits[[model_idx]] <- try(
                fit.variogram(
                    current_variogram[current_variogram$id == paste(var), ],
                    model = initial_model,
                    fit.method = 7,
                    debug.level = 0),
                silent = TRUE)
        
            if (!inherits(fits[[model_idx]], "try-error")) {
                SSerr[model_idx] <- attr(fits[[model_idx]], "SSErr")
            } else {
                SSerr[model_idx] <- Inf
            }
        }
    
        best_id <- which.min(SSerr)
        fitted_model <- fits[[best_id]]
    
        if (inherits(fitted_model, "try-error") || is.null(fitted_model)) {
            warning(paste("Could not fit variogram model for variable", var))
        }
        fitted_variograms[[variogram_name]][[var]] <- fitted_model
    }
  
    # Cross-variogram fitting
    for (var1_idx in 1:(length(variables) - 1)) {
        for (var2_idx in (var1_idx + 1):length(variables)) {
            var1 <- variables[var1_idx]
            var2 <- variables[var2_idx]
      
            combination1 <- paste(var1, ".", var2, sep = "")
            combination2 <- paste(var2, ".", var1, sep = "")

            if (!(combination1 %in% processed_combinations || combination2 %in% processed_combinations)) {
                nugget_value <- current_variogram$gamma[current_variogram$id == combination1][1]
                sill_value <- max(current_variogram$gamma[current_variogram$id == combination1])
                range_value <- current_variogram$dist[which.min(abs(current_variogram$gamma[current_variogram$id == combination1] - sill_value))]
        
                # Try different models for cross-variogram
                models <- vgm_models
                fits <- list()
                SSerr <- numeric(length(models))
        
                for (model_idx in seq_along(models)) {
                    initial_model <- vgm(
                        psill = max(0, sill_value - nugget_value),
                        model = models[model_idx],
                        range = range_value,
                        nugget = nugget_value,
                        anis = c(as.numeric(current_variogram$dir.hor[1]), 0.5))
            
                    fits[[model_idx]] <- try(
                        fit.variogram(
                            current_variogram[current_variogram$id == combination1, ],
                            model = initial_model,
                            fit.method = 7,
                            debug.level = 0),
                        silent = TRUE)
            
                    if (!inherits(fits[[model_idx]], "try-error")) {
                        SSerr[model_idx] <- attr(fits[[model_idx]], "SSErr")
                    } else {
                        SSerr[model_idx] <- Inf
                    }
                }
        
                best_id <- which.min(SSerr)
                fitted_model <- fits[[best_id]]
        
                if (inherits(fitted_model, "try-error") || is.null(fitted_model)) {
                    warning(paste("Could not fit cross-variogram model for", combination1))
                }
                fitted_cross_variograms[[variogram_name]][[combination1]] <- fitted_model
                processed_combinations <- c(processed_combinations, combination1, combination2)
            }
        }
    }
  }

  # Step 4: Combine fitted models into final gstat objects
  variograms <- names(fitted_variograms)

  # Step 5: Process each variogram direction
  for (variogram in variograms) {
    # Step 5.1: Extract current variogram data
    current_variogram <- fitted_variograms[[variogram]]
    current_cross_variogram <- fitted_cross_variograms[[variogram]]

    # Step 5.2: Initialize gstat object with first variable
    gg <- gstat(id = names(current_variogram)[1],
              formula = as.formula(paste(names(current_variogram)[1], " ~ 1")),
              model = current_variogram[[names(current_variogram)[1]]],
              data=spdf)
  
    # Step 5.3: Add remaining variables to gstat object
    for (i in 2:length(current_variogram)) {
        gg <- gstat(gg, id=names(current_variogram)[i],
                formula=as.formula(paste(names(current_variogram)[i], " ~ 1")),
                model=current_variogram[[names(current_variogram)[i]]],
                data=spdf)
    }
  
    # Step 5.4: Process cross-variograms
    n_cvgs <- length(current_variogram)
    for (i in 1:(n_cvgs - 1)) {
        for (j in (i + 1):n_cvgs) {
        gg <- gstat(gg, id = c(names(current_variogram)[i], names(current_variogram)[j]),
                  model = current_cross_variogram[[paste(names(current_variogram)[i], ".", names(current_variogram)[j], sep = "")]],
                  data = spdf)
        }
    }
  
    # Step 5.5: Combine and analyze model results
    combined_df <- as.data.frame(do.call(rbind, gg$model))
    split_data <- split(combined_df, combined_df$model)
    grouped_medians <- lapply(split_data, function(df) {
      apply(df[, c("psill", "range", "kappa")], 2, median)
    })
    grouped_medians_df <- as.data.frame(do.call(rbind, grouped_medians))
    rownames(grouped_medians_df) <- names(grouped_medians)
    grouped_medians_df$n <- as.vector(table(combined_df$model)[rownames(grouped_medians_df)])
    fitted_models <- grouped_medians_df[grouped_medians_df$n > 0, ]
    total_n <- sum(fitted_models$n)
    threshold <- total_n * 0.02
    filtered_models <- fitted_models[fitted_models$n >= threshold, ]
    ordered_models <- filtered_models[order(-filtered_models$n), ]
    cat("Selected models for", variogram, ":\n")
    print(ordered_models)

    # Step 5.6: Create and store final combined model
    nugget_model <- vgm(psill = ordered_models$psill[1], model = rownames(ordered_models)[1],
                      range = ordered_models$range[1], kappa = ordered_models$kappa[1])
    model_list <- lapply(2:nrow(ordered_models), function(i) {
        vgm(psill = ordered_models$psill[i], model = rownames(ordered_models)[i],
            range = ordered_models$range[i], kappa = ordered_models$kappa[i])
    })
    g <- gstat(g, model = rbind(nugget_model, do.call(rbind, model_list)), fill.all = TRUE)
    gstat_list[[variogram]] <- g
  }

  # Step 6: Return final results
  return(gstat_list)
}