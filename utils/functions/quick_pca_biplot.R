#' Create an Enhanced PCA Biplot with Group Visualization
#'
#' @description
#' Creates a comprehensive biplot from PCA results, displaying both scores and loadings
#' with customized aesthetics, group-based coloring, and optional point labeling.
#'
#' @param pca_result A prcomp object containing PCA results
#' @param group_data Factor or vector for coloring data points by group
#' @param x Character specifying x-axis component (default: "PC1")
#' @param y Character specifying y-axis component (default: "PC2")
#' @param add_labels Logical indicating whether to add point labels (default: FALSE)
#'
#' @return A ggplot2 object containing:
#'         - Score plot with points colored by group
#'         - Loading arrows showing variable contributions
#'         - Variable labels with smart positioning
#'         - Reference grid and axis labels with variance explained
#'
#' @details
#' The biplot combines:
#' - Score plot showing sample positions in PC space
#' - Loading arrows indicating variable contributions
#' - Automatic scaling of loading arrows for optimal visualization
#' - Smart label positioning to avoid overlaps
#' - Variance explained information in axis labels
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_segment geom_text_repel
#' @importFrom ggplot2 theme_minimal theme scale_x_continuous scale_y_continuous
#' @importFrom ggrepel geom_text_repel
#'
#' @author Antonios Koutroumpas, Abel Ruiz-Giralt
#' 
#' @examples
#' \dontrun{
#' # Basic usage with iris dataset
#' data(iris)
#' pca_res <- prcomp(iris[,1:4], scale. = TRUE)
#' quick_pca_biplot(pca_res, iris$Species)
#'
#' # Using different components and adding labels
#' quick_pca_biplot(pca_res, iris$Species, 
#'                  x = "PC2", y = "PC3", 
#'                  add_labels = TRUE)
#' }
#'
#' @export

quick_pca_biplot <- function(pca_result, group_data = NULL, x = "PC1", y = "PC2", 
                             arrow_scale_factor = "auto", add_labels = FALSE) {
    # Input validation
    if (!inherits(pca_result, "prcomp")) {
        stop("pca_result must be a prcomp object")
    }
    if (!is.null(group_data) && length(group_data) != nrow(pca_result$x)) {
        stop("Length of group_data must match number of observations in PCA")
    }

    # Prepare PCA components and variance
    eigenvalues <- (pca_result$sdev)^2
    var_explained <- eigenvalues/sum(eigenvalues) * 100

    # Prepare scores data frame
    scores <- as.data.frame(pca_result$x)
    if (!is.null(group_data)) {
        scores$Group <- as.factor(group_data)
    }
    scores$Label <- rownames(scores)  # Add row names for labels

    # Prepare loadings data frame
    loadings <- as.data.frame(pca_result$rotation)
    loadings$Variable <- rownames(loadings)

    if (arrow_scale_factor == "auto") {
        # Calculate scaling factor for loading arrows
        scale_factor <- min(
            (max(scores[[x]]) - min(scores[[x]])),
            (max(scores[[y]]) - min(scores[[y]]))
        )
    } else {
        scale_factor <- arrow_scale_factor
    }

    # Create the biplot
    p <- ggplot() +
        # Add reference lines
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey80") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey80")
    
    # Add score points with or without group coloring
    if (!is.null(group_data)) {
        p <- p + geom_point(data = scores, 
                           aes_string(x = x, y = y, color = "Group"), 
                           alpha = 0.7, 
                           size = 3)
    } else {
        p <- p + geom_point(data = scores, 
                           aes_string(x = x, y = y), 
                           alpha = 0.7, 
                           size = 3,
                           color = "steelblue")
    }
    
    # Conditionally add point labels
    if (add_labels) {
        p <- p + geom_text_repel(data = scores,
                                aes_string(x = x, y = y, label = "Label"),
                                size = 3, 
                                color = "black")
    }
    
    p <- p +
        # Add loading arrows
        geom_segment(data = loadings,
                    aes_string(x = 0, y = 0, 
                              xend = paste0(x, "* scale_factor"), 
                              yend = paste0(y, "* scale_factor")),
                    arrow = arrow(length = unit(0.3, "cm"), type = "closed"), 
                    color = "red3",
                    alpha = 0.7) +
        
        # Add variable labels using ggrepel
        geom_text_repel(data = loadings,
                       inherit.aes = FALSE,
                       aes_string(x = paste0(x, "* scale_factor * 1.2"),
                                y = paste0(y, "* scale_factor * 1.2"),
                                label = "Variable"),
                       size = 5, 
                       color = "black", 
                       fontface = "bold",
                       segment.color = NA,
                       bg.color = "white", 
                       bg.r = 0.05) +
        
        # Customize theme with reduced margins
        theme_minimal() +
        theme(
        panel.grid.major = element_line(color = "grey95"),
        panel.grid.minor = element_line(color = "grey98", linetype = "dotted"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Increase title size
        plot.subtitle = element_text(hjust = 0.5, size = 14),  # Increase subtitle size
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.margin = margin(t = 10),
        legend.text = element_text(size = 12),  # Increase legend text size
        legend.title = element_text(size = 14),  # Optionally, increase legend title size
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
        ) +
        labs(
        title = "PCA Biplot",
        subtitle = "Score Plot with Variable Loadings",
        x = sprintf("%s (%.1f%% Variance)", x, var_explained[which(colnames(scores) == x)]),
        y = sprintf("%s (%.1f%% Variance)", y, var_explained[which(colnames(scores) == y)])
        ) +
        
        # Set plot dimensions with reduced expansion
        scale_x_continuous(expand = expansion(mult = 0.15)) +
        scale_y_continuous(expand = expansion(mult = 0.15))
    
    return(p)
}