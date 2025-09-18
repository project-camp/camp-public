#' Validate Geostatistical Predictions
#'
#' @description
#' Performs comprehensive validation of cokriging predictions including
#' cross-validation and spatial validation with diagnostic plots.
#'
#' @param spdf_data SpatialPointsDataFrame containing the original spatial data
#' @param ck Cokriging prediction results from geostatistical_analysis()
#' @param g_krig gstat object with fitted model from geostatistical_analysis()
#' @param nfold integer. Number of folds for cross-validation (default: 5)
#' @param plot_scatter logical. Whether to plot scatter plots in cross-validation (default: TRUE)
#' @param plot_hist logical. Whether to plot histograms in cross-validation (default: TRUE)
#' @param plot_qq logical. Whether to plot Q-Q plots in cross-validation (default: TRUE)
#' @param accuracy_plot logical. Whether to plot accuracy plots in spatial validation (default: TRUE)
#'
#' @return NULL (function used for validation and plotting)
#'
validate_predictions <- function(spdf_data, ck, g_krig, nfold = 5, 
                                plot_scatter = TRUE, plot_hist = TRUE, 
                                plot_qq = TRUE, accuracy_plot = TRUE) {
  
  # === CROSS VALIDATION ===
  par(bg = "white")
  options(repr.plot.width = 12, repr.plot.height = 12)
  source("./utils/functions/ck_cross_validation.R")
  ck_cross_validation(spdf_data, g_krig, nfold = nfold, 
                     plot_scatter = plot_scatter, plot_hist = plot_hist, plot_qq = plot_qq)
  
  # === SPATIAL VALIDATION ===
  par(bg = "white")
  options(repr.plot.width = 9, repr.plot.height = 9)
  source("./utils/functions/ck_spatial_validation.R")
  ck_spatial_validation(spdf_data, ck, accuracy_plot = accuracy_plot)
}