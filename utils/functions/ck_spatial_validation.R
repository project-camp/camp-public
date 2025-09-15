#' Spatial Validation Analysis for Cokriging Results
#' 
#' @description Performs spatial validation analysis by comparing predicted values 
#' with observed values at specific locations. The function calculates accuracy,
#' precision, and goodness metrics for both multivariate and individual variable results.
#' 
#' @param spdf SpatialPointsDataFrame containing the ILR transformed data with observed values
#' @param ck SpatialPointsDataFrame containing the cokriging results with predicted values and variances
#' @param accuracy_plot logical indicating whether to plot the accuracy results (default = FALSE)
#' 
#' @return list with two components:
#'   \itemize{
#'     \item mv_results: data frame with multivariate accuracy, precision and goodness metrics
#'     \item var_results: data frame with per-variable metrics
#'   }
#' 
#' @details The function performs the following steps:
#'   1. Matches prediction points to nearest observation points
#'   2. Calculates overall multivariate accuracy metrics
#'   3. Calculates individual accuracy metrics for each variable
#'   
#' @note Requires the sp package for spatial operations
#' 
#' @author Abel Ruiz Giralt
#' @export
#' 
#' @examples
#' \dontrun{
#' # Perform validation with default settings
#' results <- ck_spatial_validation(spdf, ck_results)
#' 
#' # Show accuracy plot
#' results <- ck_spatial_validation(spdf, ck_results, accuracy_plot = TRUE)
#' 
#' # Access results
#' print(results$mv_results)  # Multivariate results
#' print(results$var_results) # Per-variable results
#' }
#' 
ck_spatial_validation <- function(spdf, ck, accuracy_plot = FALSE) {
    # Input validation
    if (!inherits(spdf, "SpatialPointsDataFrame") || !inherits(ck, "SpatialPointsDataFrame")) {
        stop("Both spdf and ck must be SpatialPointsDataFrame objects")
    }
    
    # Extract spatial points objects
    ck_coords <- as.matrix(ck@coords)
    spdf_coords <- as.matrix(spdf@coords)
    
    # Find nearest neighbors between observed and predicted points
    distances <- calc_distances(spdf_coords, ck_coords)
    closest_points_indices <- apply(distances, 1, which.min)
    
    # Create spatial object with matched prediction points
    closest_data <- ck@data[closest_points_indices, ]
    ck_at_spdf <- SpatialPointsDataFrame(
        coords = spdf@coords,
        data = closest_data,
        proj4string = spdf@proj4string
    )
    
    # Calculate multivariate accuracy metrics
    ck_acc <- gmGeostats::accuracy(ck_at_spdf@data, observed = spdf@data, 
                      method = "cokriging")
    ck_precision <- gmGeostats::precision(ck_acc)
    
    # Generate accuracy plot if requested
    if(accuracy_plot) {
        plot(ck_acc, main = "Cokriging multivariate accuracy plot")
    }

    # Store multivariate results
    mv_results <- data.frame(
                Accuracy = mean(ck_acc),
                Precision = round(as.numeric(ck_precision[1]), 2),
                Goodness = round(as.numeric(ck_precision[2]), 2)
            )
    
    # Initialize results dataframe for per-variable metrics
    variables <- colnames(spdf@data)
    var_results <- data.frame(
        Variable = character(),
        Accuracy = numeric(),
        Precision = numeric(),
        Goodness = numeric(),
        stringsAsFactors = FALSE
    )
    
    # Calculate metrics for each variable
    for (var in variables) {
        tryCatch({
            # Extract observed and predicted data for current variable
            observed_df <- data.frame(spdf@data[,var])
            predicted_df <- data.frame(
                ck_at_spdf@data[,paste0(var, ".pred")],
                ck_at_spdf@data[,paste0(var, ".var")]
            )
            
            # Set proper column names
            names(observed_df) <- var
            names(predicted_df) <- c(paste0(var, ".pred"), paste0(var, ".var"))
            predicted_df[[var]] <- observed_df[[var]]
            
            # Calculate variable-specific metrics
            ck_acc <- gmGeostats::accuracy(predicted_df, 
                             observed = observed_df, 
                             method = "cokriging",
                             ivar = var)
            ck_precision <- gmGeostats::precision(ck_acc)
            
            # Store variable results
            var_results <- rbind(var_results, data.frame(
                Variable = var,
                Accuracy = mean(ck_acc),
                Precision = round(as.numeric(ck_precision[1]), 2),
                Goodness = round(as.numeric(ck_precision[2]), 2)
            ))
        }, error = function(e) {
            warning(paste("Error processing variable", var, ":", e$message))
        })
    }
    
    return(list(
        mv_results = mv_results,
        var_results = var_results
    ))
}

#' Calculate Euclidean distances between two sets of coordinates
#' 
#' @description Internal function to compute distances between source and target coordinates
#' 
#' @param from_coords matrix of source coordinates (n x 2)
#' @param to_coords matrix of target coordinates (m x 2)
#' @return matrix of distances (n x m)
#' 
#' @keywords internal
calc_distances <- function(from_coords, to_coords) {
    # Input validation
    if (!is.matrix(from_coords) || !is.matrix(to_coords)) {
        stop("Coordinates must be provided as matrices")
    }
    
    n1 <- nrow(from_coords)
    n2 <- nrow(to_coords)
    distances <- matrix(0, nrow=n1, ncol=n2)
    
    # Calculate distances using sp package function
    for (i in 1:n1) {
        distances[i,] <- spDistsN1(to_coords, from_coords[i,], longlat=FALSE)
    }
    return(distances)
}