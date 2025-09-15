#' Plot Directional Variogram Comparison
#'
#' Creates a plot comparing directional variograms to visualize anisotropy in spatial data.
#'
#' @param variogram A directional variogram object from gstat package
#' @param directions Vector of two direction angles in degrees (e.g., c(0, 90))
#' @param direction_labels Vector of two labels for the directions (e.g., c("NS", "EW"))
#' @param colors Vector of two colors for plotting the directions. Defaults to c("blue", "red")
#'
#' @return A ggplot2 object showing the directional variogram comparison
#' @export
#'
#' @author Abel Ruiz-Giralt
#'
#' @examples
#' v_dir <- variogram(g, alpha = c(0, 90), ...)  # Create directional variogram
#' quick_anis_variogram(v_dir, c(0, 90), c("NS", "EW"))
#' quick_anis_variogram(v_dir, c(45, 135), c("NE-SW", "NW-SE"), c("#1f77b4", "#d62728"))
#'
#' @details
#' The function creates a faceted plot showing the semivariance vs distance for two 
#' selected directions, allowing easy comparison of spatial continuity along different
#' orientations. This is useful for detecting anisotropy in spatial data.
quick_anis_variogram <- function(variogram, directions = directions, direction_labels = direction_labels, 
                               colors = c("blue", "red")) {
    
    # Input validation
    if(length(directions) != 2) stop("Must provide exactly 2 directions")
    if(length(direction_labels) != 2) stop("Must provide exactly 2 labels")
    if(length(colors) != 2) stop("Must provide exactly 2 colors")
    
    # Filter variogram data for selected directions
    v_filtered <- variogram[variogram$dir.hor %in% directions, ]
    
    # Create title based on direction labels
    plot_title <- paste("Anisotropic variogram (", 
                       direction_labels[1], " vs. ", 
                       direction_labels[2], ")", 
                       sep="")
    
    # Create color mapping
    color_values <- setNames(colors, as.character(directions))
    
    # Create plot
    p <- ggplot(v_filtered, 
                aes(x = dist, y = gamma, color = factor(dir.hor))) +
        geom_line(size = 1) +
        labs(title = plot_title, 
             x = "Distance", 
             y = "Semivariance", 
             color = "Direction") +
        scale_color_manual(values = color_values, 
                          labels = direction_labels) + 
        theme_minimal() +
        theme(
            legend.position = "bottom",
            legend.title = element_text(size = 14, face = "bold"),
            legend.text = element_text(size = 12),
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            strip.text = element_text(size = 14)
        ) +
        facet_wrap(~ id, scales = "free")
    
    return(p)
}
