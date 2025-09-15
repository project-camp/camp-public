#' Cross-validation for Kriging Models
#' 
#' @description Performs k-fold cross-validation on spatial kriging models and 
#'              calculates error metrics (ME, MSE) with optional diagnostic plots.
#' 
#' @param spdf A SpatialPointsDataFrame object containing the data
#' @param g A gstat object with the variogram model
#' @param nfold Number of folds for cross-validation (default = 5)
#' @param plot_scatter Logical; if TRUE, creates scatter plots of predicted vs observed values
#' @param plot_hist Logical; if TRUE, creates histograms of prediction residuals
#' @param plot_qq Logical; if TRUE, creates Q-Q plots of normalized residuals
#' 
#' @return A data frame containing the following metrics for each variable:
#'   \itemize{
#'     \item Variable - Name of the variable
#'     \item ME - Mean Error
#'     \item MSE - Mean Squared Error
#'   }
#'   Additionally, if plotting options are enabled, generates diagnostic plots.
#' 
#' @importFrom gstat gstat.cv
#' @importFrom ggplot2 ggplot aes geom_point geom_abline geom_smooth labs theme_minimal stat_qq stat_qq_line
#' @importFrom gridExtra grid.arrange
#' 
#' @examples
#' \dontrun{
#' # Create a gstat model first
#' g <- gstat(id = "var1", formula = var1 ~ 1, data = spdf_data)
#' g <- gstat(g, id = "var2", formula = var2 ~ 1, data = spdf_data)
#' 
#' # Run cross-validation with all diagnostic plots
#' result <- ck_cross_validation(spdf_data, g, nfold = 5,
#'                              plot_scatter = TRUE,
#'                              plot_hist = TRUE,
#'                              plot_qq = TRUE)
#' }

ck_cross_validation <- function(spdf, g, nfold = 5,
                              plot_scatter = FALSE,
                              plot_hist = FALSE,
                              plot_qq = FALSE) {
    
    # Initialize validation process
    # Extract true values from spatial data frame
    true <- as.data.frame(spdf@data)
    
    # Perform cross-validation using gstat
    # Suppress messages for cleaner output
    invisible(capture.output(
        residuals <- as.data.frame(gstat.cv(g, nfold = nfold,
                                          remove.all = TRUE,
                                          all.residuals = TRUE))
    ))

    # Calculate predictions from residuals
    preds <- true - residuals
    
    # Initialize results data frame
    results <- data.frame(Variable = character(),
                         ME = numeric(),
                         MSE = numeric(),
                         stringsAsFactors = FALSE)
    
    variables <- colnames(residuals)[1:ncol(true)]
    
    for (var in variables) {
        errors <- residuals[[var]]
        observed <- true[[var]]
        predicted <- preds[[var]]
        
        # Mean Error (ME)
        ME <- mean(errors)

        # Mean Squared Error (MSE)
        MSE <- mean(errors^2)
        
        # Store results
        results <- rbind(results,
                         data.frame(Variable = var,
                                    ME = ME,
                                    MSE = MSE))
    }

    # Format results for display
    results_table <- as.data.frame(t(results))
    colnames(results_table) <- results_table[1,]
    results_table <- results_table[-1,]
    print(results_table)

    # Create plots if requested
    if(any(c(plot_scatter, plot_hist, plot_qq))) {
        plot_data <- data.frame(
            Variable = rep(colnames(true), each = nrow(true)),
            Observed = unlist(true),
            Predicted = unlist(preds),
            Residuals = unlist(residuals)
        )
        
        scatter_plots <- list()
        hist_plots <- list()
        qq_plots <- list()
        
        # Scatter plots
        if(plot_scatter) {
            for(var in colnames(true)) {
                sub_data <- subset(plot_data, Variable == var)
                p <- ggplot(sub_data, aes(x = Predicted, y = Observed)) +
                    geom_point() +
                    geom_abline(intercept = 0, slope = 1, color = "red") +
                    geom_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x) +
                    labs(title = paste("Scatter plot for", var),
                         x = "Predicted",
                         y = "Observed") +
                    theme_minimal()
                scatter_plots[[length(scatter_plots) + 1]] <- p
            }
        }
        
        # Histograms
        if(plot_hist) {
            for(var in colnames(residuals)) {
                sub_data <- subset(plot_data, Variable == var)
                p <- ggplot(sub_data, aes(x = Residuals)) +
                    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
                    labs(title = paste("Histogram of residuals for", var),
                         x = "Residuals",
                         y = "Count") +
                    theme_minimal()
                hist_plots[[length(hist_plots) + 1]] <- p
            }
        }
        
        # For QQ plots, ensure data is normalized
        if(plot_qq) {
            for(var in colnames(residuals)) {
                sub_data <- subset(plot_data, Variable == var)
                # Normalize residuals
                norm_residuals <- scale(sub_data$Residuals)
                p <- ggplot(data.frame(norm_residuals), aes(sample = norm_residuals)) +
                    stat_qq() +
                    stat_qq_line(color = "red") +
                    labs(title = paste("Q-Q plot for", var),
                         x = "Theoretical Quantiles",
                         y = "Sample Quantiles") +
                    theme_minimal()
                qq_plots[[length(qq_plots) + 1]] <- p
            }
        }
        
        # Display plots with fully suppressed output
        n_cols <- min(3, ncol(true))

        if(plot_scatter && length(scatter_plots) > 0) {
            invisible(capture.output(
                suppressWarnings(
                    suppressMessages({
                        print(gridExtra::grid.arrange(
                            grobs = scatter_plots, 
                            ncol = n_cols
                        ))
                    })
                )
            ))
        }

        if(plot_hist && length(hist_plots) > 0) {
            invisible(capture.output(
                suppressWarnings(
                    suppressMessages({
                        print(gridExtra::grid.arrange(
                            grobs = hist_plots, 
                            ncol = n_cols
                        ))
                    })
                )
            ))
        }

        if(plot_qq && length(qq_plots) > 0) {
            invisible(capture.output(
                suppressWarnings(
                    suppressMessages({
                        print(gridExtra::grid.arrange(
                            grobs = qq_plots, 
                            ncol = n_cols
                        ))
                    })
                )
            ))
        }
        
    }
    
    # Return results
    invisible(results)
}