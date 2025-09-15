#' Create Cokriging Map List for Multiple CK Objects
#' 
#' @description Creates a list of levelplot objects from multiple cokriging results for compositional data
#' 
#' @param ck_list List of Cokriging objects containing the predicted values
#' @param orig Original compositional data matrix/dataframe
#' @param maf_result MAF transformation results (optional)
#' @param compositional_transformation Type of compositional transformation used ("ilr" or "clr")
#' @param shapefile Shapefile object for map boundaries
#' @param tolerance Tolerance value for merging CK objects (default: 0.5)
#' 
#' @return List of levelplot objects, one for each compositional component
#' 
#' @import raster
#' @import sp
#' @import rasterVis
#' @import latticeExtra
#' @import viridis
#' @import grid
#' @import terra
create_ck_maplist_from_list <- function(ck_list, 
                                        maf_result = NULL,
                                        compositional_transformation = "ilr", orig = orig,
                                        shapefile = shapefile,
                                        tolerance = 0.5) {
    # Input validation
    if (!compositional_transformation %in% c("ilr", "clr")) {
        stop("compositional_transformation must be either 'ilr' or 'clr'")
    }
    
    # Initialize empty dataframe and brick list
    df <- data.frame()
    CKbrk_list <- list()
    
    # Process each CK object
    for (i in seq_along(ck_list)) {
        ck <- ck_list[[i]]
        
        if (compositional_transformation == "ilr") {
            df0 <- as.matrix(ck@data[, seq(1, (ncol(orig) - 1) * 2, 2)])
        } else if (compositional_transformation == "clr") {
            df0 <- as.matrix(ck@data[, seq(1, ncol(orig) * 2, 2)])
        }
        
        # Back-transform if MAF was used
        if (!is.null(maf_result)) {
            scores <- df0
            loadings <- maf_result$invLoadings[1:ncol(scores), ]
            center <- maf_result$center
            df0 <- scores %*% unclass(loadings) + outer(rep(1, nrow(scores)), center)
        }
        
        # Inverse transform compositional data
        if (compositional_transformation == "ilr") {
            df0 <- as.data.frame(ilrInv(df0), orig = orig)
        } else if (compositional_transformation == "clr") {
            df0 <- as.data.frame(clrInv(df0))
        }
        
        # Convert to percentage
        df0 <- as.data.frame(df0 * 100)
        
        # Create brick and add to list
        CKbrk <- raster::brick(sp::SpatialPixelsDataFrame(as(ck, "SpatialPixels"), data = df0))
        CKbrk_list[[i]] <- CKbrk
        
        # Append data to main dataframe
        df <- rbind(df, df0)
    }
    
    # Set column names for main dataframe
    colnames(df) <- colnames(orig)
    
    # Set breaks for colorkey
    brks <- lapply(1:ncol(df), function(x) {
        seq(min(df[, x]), max(df[, x]), length.out = 40)
    })
    
    # Set breaks for labels
    brksL <- lapply(1:ncol(df), function(x) {
        aux <- round(seq(min(df[, x]), max(df[, x]), length.out = 5), 4)
        aux[1] <- aux[1] + 0.0001
        aux[length(aux)] <- aux[length(aux)] - 0.0001
        return(aux)
    })

    # Merge all bricks
    CKbrk <- do.call(terra::merge, c(CKbrk_list, list(tolerance = tolerance)))
    
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