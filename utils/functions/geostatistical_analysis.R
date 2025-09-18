#' Core Geostatistical Analysis and Prediction
#'
#' @description
#' Performs variogram modeling, cokriging prediction, and map visualization 
#' for compositional data. Supports both omnidirectional and directional 
#' approaches with MAF transformations.
#'
#' @param spdf_data SpatialPointsDataFrame containing the spatial data for analysis
#' @param method_name character. Name identifier for the method (e.g., "ILR_Omnidirectional")
#' @param lag_dist numeric. Lag distance for variogram calculation
#' @param site_diag numeric. Site diagonal distance for variogram cutoff
#' @param directional logical. Whether to use directional variogram modeling (default: FALSE)
#' @param maf_result list. MAF analysis results if applicable (default: NULL)
#' @param resolution numeric. Grid resolution for kriging prediction (default: 0.5)
#'
#' @return A list containing:
#'   \item{ck}{Cokriging prediction results}
#'   \item{g}{Final gstat object with fitted model}
#'
geostatistical_analysis <- function(spdf_data, method_name, lag_dist, site_diag, 
                                   directional = FALSE, maf_result = NULL, resolution = 0.5) {
  
  # Create method-specific directories
  method_dir <- paste0("./test/ck/", tolower(gsub("_", "-", method_name)), "/")
  maps_dir <- paste0(method_dir, "maps/")
  if (!dir.exists(method_dir)) dir.create(method_dir, recursive = TRUE)
  if (!dir.exists(maps_dir)) dir.create(maps_dir, recursive = TRUE)
  
  # Source required functions
  source("./utils/functions/create_gstat_from_spdf.R")
  source("./utils/functions/calculate_variogram_models.R")
  source("./utils/functions/fit_lmc_GV.R")
  source("./utils/functions/create_grid_from_spdf.R")
  source("./utils/functions/create_ck_maplist.R")
  
  par(bg = "white")
  options(repr.plot.width = 15, repr.plot.height = 12)
  
  if (!directional) {
    # === OMNIDIRECTIONAL APPROACH ===
    g <- create_gstat_from_spdf(spdf_data, method = "ordinary")
    v <- variogram(g, width = lag_dist / 2, cutoff = site_diag / 3, cross = TRUE)
    
    # Scoping fix for calculate_variogram_models function
    assign("v", v, envir = .GlobalEnv)
    assign("g", g, envir = .GlobalEnv)
    assign("spdf_data", spdf_data, envir = .GlobalEnv)
    
    g <- calculate_variogram_models(spdf = spdf_data, gstat = g, variogram = v,
                                   vgm_models = c("Sph", "Exp", "Gau"), threshold = 0)
    
    # Cleanup global environment
    rm("v", "g", "spdf_data", envir = .GlobalEnv)
    
    # Fit Linear Model of Coregionalization
    tryCatch({
      fitted_lmc <- fit_lmc_GV(v, g$model)
      plot(v, fitted_lmc, main = "Fitted variogram")
      g$model <- fitted_lmc
    }, error = function(e) {
      cat("LMC fitting failed, using original model parameters\n")
      plot(v, g$model, main = "Fitted variogram (fallback)")
    })
    
  } else {
    # === DIRECTIONAL APPROACH (simplified fallback) ===
    cat("Directional approach not fully implemented yet - using omnidirectional as fallback\n")
    
    g <- create_gstat_from_spdf(spdf_data, method = "ordinary")
    v <- variogram(g, width = lag_dist / 2, cutoff = site_diag / 3, cross = TRUE)
    
    # Apply same scoping fix
    assign("v", v, envir = .GlobalEnv)
    assign("g", g, envir = .GlobalEnv)
    assign("spdf_data", spdf_data, envir = .GlobalEnv)
    
    g <- calculate_variogram_models(spdf = spdf_data, gstat = g, variogram = v,
                                   vgm_models = c("Sph", "Exp", "Gau"), threshold = 0)
    
    rm("v", "g", "spdf_data", envir = .GlobalEnv)
    
    tryCatch({
      fitted_lmc <- fit_lmc_GV(v, g$model)
      plot(v, fitted_lmc, main = "Fitted variogram (directional fallback)")
      g$model <- fitted_lmc
    }, error = function(e) {
      cat("LMC fitting failed, using original model parameters\n")
      plot(v, g$model, main = "Fitted variogram (fallback)")
    })
  }
  
  # === COKRIGING PREDICTION ===
  g_krig <- create_gstat_from_spdf(spdf_data, method = "ordinary")
  g_krig$model <- g$model
  
  grid <- create_grid_from_spdf(spdf_data, resolution = resolution, buffer = 5, convex_hull = TRUE)
  sp::proj4string(grid) <- sp::proj4string(spdf_data)
  
  ck <- predict(g_krig, newdata = grid)
  
  # Save cokriging results
  area_name <- names(which.max(table(dataset_spdf$Area)))
  saveRDS(ck, file = paste0(method_dir, name, "_CK", area_name, ".rds"))
  
  # === VISUALIZATION AND MAP GENERATION ===
  par(bg = "white")
  options(repr.plot.width = 20, repr.plot.height = 20)
  
  if (is.null(maf_result)) {
    ck_maplist <- create_ck_maplist(ck, orig = dataset_c_closed,
                                   compositional_transformation = "ilr",
                                   shapefile = structures_utm)
  } else {
    ck_maplist <- create_ck_maplist(ck, orig = dataset_c_closed,
                                   maf_result = maf_result,
                                   compositional_transformation = "ilr",
                                   shapefile = structures_utm)
  }
  
  # Display grid arrangement
  grid.arrange(grobs = ck_maplist, ncol = 4)
  
  # Save individual maps
  mapnames <- sort(colnames(dataset_c_closed))
  method_dir <- paste0("./test/ck/", tolower(gsub("_", "-", method_name)), "/")
  maps_dir <- paste0(method_dir, "maps/")
  
  for (i in seq_along(ck_maplist)) {
    png_filename <- file.path(maps_dir, 
                             paste0("plot_", name, "_", mapnames[i], "_", area_name, "_", method_name, ".png"))
    png(png_filename, width = 1200, height = 900, res = 300)
    print(ck_maplist[[i]])
    dev.off()
  }
  cat("Saved", length(ck_maplist), "maps to:", maps_dir, "\n")
  
  return(list(ck = ck, g = g_krig))
}