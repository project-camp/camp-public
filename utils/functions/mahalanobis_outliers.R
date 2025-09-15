#' Detect Multivariate Outliers using Mahalanobis Distance
#' 
#' @description
#' Identifies multivariate outliers in a dataset using robust Mahalanobis distance
#' calculation and optional visualization through distance plots and PCA.
#'
#' @param data data.frame or matrix. Numerical data to analyze for outliers
#' @param alpha numeric. Confidence level for outlier detection threshold (default: 0.975)
#' @param plot logical. Whether to create visualization plots (default: TRUE)
#'
#' @return Invisibly returns a data.frame containing:
#'         - Original data
#'         - mahalanobis_dist: calculated Mahalanobis distances
#'         - is_outlier: logical indicating if each point is an outlier
#'
#' @details
#' The function uses robust covariance estimation (MCD) to calculate Mahalanobis
#' distances and detect outliers. Points are classified as outliers if their
#' distance exceeds the chi-square critical value at the specified confidence level.
#' 
#' When plot=TRUE, generates two visualizations:
#' 1. Distance plot showing Mahalanobis distances with outlier threshold
#' 2. PCA plot with confidence ellipse showing the first two principal components
#'
#' @importFrom robustbase covMcd
#' @importFrom stats mahalanobis qchisq prcomp
#' @importFrom ggplot2 ggplot aes geom_segment geom_point geom_hline 
#' @importFrom ggplot2 scale_color_manual theme_minimal labs geom_path
#'
#' @author Antonios Koutroumpas, Abel Ruiz-Giralt
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' results <- mahalanobis_outliers(my_data)
#'
#' # Adjust confidence level and suppress plots
#' results <- mahalanobis_outliers(my_data, alpha = 0.99, plot = FALSE)
#' }
#'
#' @export

mahalanobis_outliers <- function(data, alpha = 0.975, plot = TRUE) {
     
     # Compute Mahalanobis distances and detect outliers
     
     # Calculate robust estimates to handle potential outliers
     cov_mat <- robustbase::covMcd(data)

     # Calculate distances of each point from the center of the data
     md <- mahalanobis(data, 
                         center = cov_mat$center,
                         cov = cov_mat$cov)
     
     # Set threshold for what counts as an outlier
     critical_value <- qchisq(alpha, df = ncol(data) - 1)
     
     results <- data.frame(
          data,
          mahalanobis_dist = md,
          is_outlier = md > critical_value
     )

     # Count outliers
     num_outliers <- sum(results$is_outlier)
     
     # Print message with number of outliers
     cat("Number of outliers detected:", num_outliers, "out of", nrow(data), "values\n")
     
     if(plot) {
          # Prepare data for plotting
          plot_data <- data.frame(
               Index = 1:length(md),
               Distance = md,
               Type = factor(ifelse(md > critical_value, "Outlier", "Normal"))
          )

          # Create PCA for 2D visualization
          pca <- prcomp(data)
          pca_data <- data.frame(
               PC1 = pca$x[,1],
               PC2 = pca$x[,2],
               Type = factor(ifelse(md > critical_value, "Outlier", "Normal"))
          )

          # Plot 1: Distance plot
          p1 <- ggplot(plot_data, aes(x = Index, y = Distance, color = Type)) +
               geom_segment(aes(xend = Index, yend = 0), size = 1) +
               geom_point(size = 3) +
               geom_hline(yintercept = critical_value, linetype = "dashed", color = "red") +
               scale_color_manual(values = c("Normal" = "black", "Outlier" = "red")) +
               theme_minimal() +
               labs(title = "Mahalanobis Distances",
                    y = "Distance",
                    x = "Index")

          # Create ellipse coordinates
          theta <- seq(0, 2*pi, length.out = 100)
          radius <- sqrt(qchisq(alpha, df = 2))
          ell <- data.frame(
               PC1 = radius * cos(theta) * sqrt(eigen(cov(pca$x[,1:2]))$values[1]) + mean(pca$x[,1]),
               PC2 = radius * sin(theta) * sqrt(eigen(cov(pca$x[,1:2]))$values[2]) + mean(pca$x[,2])
          )
          
          # Plot 2: PCA with confidence ellipse
          p2 <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Type)) +
               geom_point(size = 3) +
               geom_path(data = ell, aes(x = PC1, y = PC2), color = "blue", 
                         linetype = "dashed", inherit.aes = FALSE) +
               scale_color_manual(values = c("Normal" = "black", "Outlier" = "red")) +
               theme_minimal() +
               labs(title = "PCA with 95% Ellipse")
          
          print(p1)
          print(p2)
     }
     
     return(invisible(results))
     }