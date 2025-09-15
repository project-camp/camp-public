#' Create Cokriging Map List
#' 
#' @description Creates a list of ggplot objects from cokriging results for compositional data.
#' Each plot represents a component of the composition with a color-coded map showing
#' the predicted values.
#' 
#' @param ck Cokriging object (SpatialPixelsDataFrame) containing the predicted values
#'        and their variances in alternating columns
#' @param orig Original compositional data matrix/dataframe where columns represent
#'        components and rows represent samples. Values should sum to 1 for each row
#' @param maf_result Optional MAF transformation results list containing:
#'        \itemize{
#'          \item invLoadings: Matrix of inverse loadings for back-transformation
#'          \item center: Vector of center values used in MAF transformation
#'        }
#' @param compositional_transformation Character string specifying the type of compositional
#'        transformation used. Must be either:
#'        \itemize{
#'          \item "ilr": Isometric log-ratio transformation
#'          \item "clr": Centered log-ratio transformation
#'        }
#' @param shapefile sf or sp object containing the boundary polygons for the study area.
#'        Used to draw borders on the maps.
#' 
#' @return A list of ggplot objects, one for each compositional component. Each plot contains:
#'         \itemize{
#'           \item A raster layer showing predicted values using viridis color scale
#'           \item Boundary lines from the provided shapefile
#'           \item A color key showing the value range
#'           \item Customized theme and formatting
#'         }
#' 
#' @import raster sp rasterVis latticeExtra viridis grid ggplot2 sf
#' 
#' @examples
#' \dontrun{
#' # Load required data
#' data(soil_compositions)  # Example compositional data
#' data(study_area)        # Example shapefile
#' 
#' # Perform cokriging (assuming you have a cokriging result)
#' ck_result <- perform_cokriging(...)
#' 
#' # Create maps without MAF transformation
#' maps <- create_ck_maplist(
#'   ck = ck_result,
#'   orig = soil_compositions,
#'   compositional_transformation = "ilr",
#'   shapefile = study_area
#' )
#' 
#' # Plot the first component
#' maps[[1]]
#' 
#' # Create maps with MAF transformation
#' maf_result <- perform_maf(...)  # Assuming you have MAF results
#' maps_maf <- create_ck_maplist(
#'   ck = ck_result,
#'   orig = soil_compositions,
#'   maf_result = maf_result,
#'   compositional_transformation = "ilr",
#'   shapefile = study_area
#' )
#' }
#' 
#' @seealso 
#' \code{\link{ilrInv}} for inverse ilr transformation
#' \code{\link{clrInv}} for inverse clr transformation
create_ck_maplist <- function(ck, 
                         orig = orig,
                         maf_result = NULL,
                         compositional_transformation = "ilr", 
                         shapefile = shapefile) {
    # Validate input transformation type to prevent invalid processing
    if (!compositional_transformation %in% c("ilr", "clr")) {
        stop("compositional_transformation must be either 'ilr' or 'clr'")
    }
    
    # Extract predicted values from cokriging results
    # For both ILR and CLR, we take every other column (odd columns) which contain predictions
    # Even columns contain variances which we don't use for mapping
    if (compositional_transformation == "ilr") {
        # For ILR, we need (n-1) components where n is number of original components
        df <- as.matrix(ck@data[, seq(1, (ncol(orig) - 1) * 2, 2)])
    } else if (compositional_transformation == "clr") {
        # For CLR, we need n components where n is number of original components
        df <- as.matrix(ck@data[, seq(1, ncol(orig) * 2, 2)])
    }
    
    # If MAF transformation was used, back-transform the data
    # This involves multiplying by inverse loadings and adding back the center
    if (!is.null(maf_result)) {
        scores <- df  # MAF scores from prediction
        loadings <- maf_result$invLoadings[1:ncol(scores), ]  # Inverse MAF loadings
        center <- maf_result$center  # Original data center
        # Back-transform: X = scores * loadings^T + center
        df <- scores %*% unclass(loadings) + outer(rep(1, nrow(scores)), center)
    }
    
    # Convert back from log-ratio space to compositional space
    if (compositional_transformation == "ilr") {
        df <- as.data.frame(ilrInv(df), orig = orig)  # Inverse ILR transformation
    } else if (compositional_transformation == "clr") {
        df <- as.data.frame(clrInv(df))  # Inverse CLR transformation
    }
    
    # Convert compositional data to percentages for easier interpretation
    df <- as.data.frame(df * 100)
    colnames(df) <- colnames(orig)

    # Create breaks for continuous color scale
    # 40 breaks for smooth color transitions
    brks <- lapply(1:ncol(df), function(x) {
        seq(min(df[, x]), max(df[, x]), length.out = 40)
    })
    
    # Create breaks for legend labels
    # 5 breaks for readable legend, slightly adjusted to avoid exact boundary values
    brksL <- lapply(1:ncol(df), function(x) {
        aux <- round(seq(min(df[, x]), max(df[, x]), length.out = 5), 4)
        aux[1] <- aux[1] + 0.0001  # Adjust lower bound
        aux[length(aux)] <- aux[length(aux)] - 0.0001  # Adjust upper bound
        return(aux)
    })

    # Convert spatial data to raster brick for plotting
    CKbrk <- raster::brick(sp::SpatialPixelsDataFrame(as(ck, "SpatialPixels"), data = df))
    
    # Helper function to convert raster layers to data frames for ggplot
    raster_to_df <- function(r) {
        df_raster <- as.data.frame(r, xy = TRUE)
        colnames(df_raster) <- c("x", "y", "value")
        return(df_raster)
    }
    
    # Prepare spatial objects for plotting
    shp_proj <- shapefile  # Boundary polygon
    shp_bbox <- sf::st_bbox(CKbrk)  # Bounding box for plot limits
    
    # Generate maps for each compositional component
    result <- lapply(order(colnames(df)), function(x) {
        # Convert current component's raster to data frame
        df_raster <- raster_to_df(CKbrk[[x]])
        
        # Create ggplot map with:
        # 1. Raster layer for predictions
        # 2. Shapefile overlay for boundaries
        # 3. Viridis color scale
        # 4. Minimal theme with no axis labels
        ggplot() +
            geom_raster(data = na.omit(df_raster), aes(x = x, y = y, fill = value)) +
            geom_sf(data = shp_proj , color = "black", linewidth = 1) +
            scale_fill_viridis(option = "turbo", limits = range(brks[[x]]), breaks = brksL[[x]]) +
            theme_minimal(base_size = 15) +
            labs(title = colnames(df)[x], fill = "Value") +
            coord_sf(xlim = c(shp_bbox["xmin"] - 2, shp_bbox["xmax"] + 2),
                     ylim = c(shp_bbox["ymin"] - 2, shp_bbox["ymax"] + 2)) +
            theme(plot.title = element_text(size = 30, face = "italic"),
                  axis.text = element_blank(),
                  axis.ticks = element_blank())           
    })

    return(result)
}