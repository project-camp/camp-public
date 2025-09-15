#' Create an Enhanced Scree Plot for PCA Results
#'
#' @description
#' Creates a detailed scree plot showing both individual and cumulative variance
#' explained by each principal component, with enhanced visualization features
#' for better interpretation of PCA results.
#'
#' @param pca_result A prcomp object containing PCA results with $sdev component
#'
#' @return A ggplot2 object containing:
#'         - Bar plot showing individual variance explained
#'         - Line plot showing cumulative variance
#'         - Percentage labels for both metrics
#'         - Reference line at 80% cumulative variance
#'
#' @details
#' The function creates a comprehensive scree plot combining:
#' - Individual variance bars with percentage labels
#' - Cumulative variance line with percentage labels
#' - Reference line at 80% cumulative variance for dimensionality assessment
#' - Customized theme and color scheme for clarity
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_point geom_text
#' @importFrom ggplot2 geom_hline scale_y_continuous scale_x_continuous
#' @importFrom ggplot2 scale_color_manual theme_minimal theme labs
#'
#' @author Antonios Koutroumpas, Abel Ruiz-Giralt
#'
#' @examples
#' \dontrun{
#' # Basic usage with iris dataset
#' data(iris)
#' pca_res <- prcomp(iris[,1:4], scale. = TRUE)
#' quick_pca_screeplot(pca_res)
#'
#' # Save plot to variable for further customization
#' p <- quick_pca_screeplot(pca_res)
#' p + theme_bw()
#' }
#'
#' @export

quick_pca_screeplot <- function(pca_result){
  # Calculate variance metrics
  eigenvalues <- (pca_result$sdev)^2
  var_explained <- eigenvalues/sum(eigenvalues) * 100
  cum_var_explained <- cumsum(var_explained)

  # Prepare data frame for plotting
  scree_data <- data.frame(
    PC = 1:length(var_explained),
    Individual_Variance = var_explained,
    Cumulative_Variance = cum_var_explained
  )

  # Create base plot
  ggplot(scree_data, aes(x = PC)) +
    # Bar plot for individual variance
    geom_bar(aes(y = Individual_Variance), 
             stat = "identity", 
             fill = "steelblue", 
             alpha = 0.7,
             color = "black") +
    
    # Line and points for cumulative variance
    geom_line(aes(y = Cumulative_Variance, 
                  color = "Cumulative Variance"), 
              linewidth = 1.2,
              group = 1) +
    geom_point(aes(y = Cumulative_Variance, 
                   color = "Cumulative Variance"), 
               size = 4) +
    
    # Add percentage labels
    geom_text(aes(y = Individual_Variance, 
                  label = sprintf("%.1f%%", Individual_Variance)),
              vjust = -0.5,
              size = 4) +
    geom_text(aes(y = Cumulative_Variance, 
                  label = sprintf("%.1f%%", Cumulative_Variance)),
              vjust = 1.5,
              color = "red",
              size = 4) +
    
    # Reference line at 80%
    geom_hline(yintercept = 80, 
               linetype = "dashed", 
               color = "red", 
               alpha = 0.5) +
    
    # Axis customization
    scale_y_continuous(
      name = "Variance Explained (%)",
      breaks = seq(0, 100, 10),
      limits = c(0, 105)  # Extra space for labels
    ) +
    scale_x_continuous(
      name = "Principal Components",
      breaks = 1:length(var_explained)
    ) +
    
    # Legend customization
    scale_color_manual(
      name = "Variance Type",
      values = c("Cumulative Variance" = "red")
    ) +
    
    # Theme customization
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold")
    ) +
    
    # Plot labels
    labs(
      title = "Screen Plot: Variance Explained by Principal Components",
      subtitle = "Individual and Cumulative Variance"
    )
}