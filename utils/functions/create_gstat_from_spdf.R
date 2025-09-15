#' Create a gstat object from a SpatialPointsDataFrame
#'
#' @description
#' This function creates a gstat object from a SpatialPointsDataFrame by adding
#' all variables as separate models with intercept-only formulas or with coordinates.
#'
#' @param spdf A SpatialPointsDataFrame containing the variables to be modeled
#' @param method A character string specifying the method to use: "ordinary" or "universal"
#' @return A gstat object containing all variables from the input SPDF
#' @details
#' If `method` is "ordinary", the function creates models with intercept-only formulas.
#' If `method` is "universal", the function includes coordinates in the models.
#' @export
#'
#' @examples
#' \dontrun{
#' data(meuse)
#' coordinates(meuse) <- ~x+y
#' g <- create_gstat_from_spdf(meuse, method = "ordinary")
#' g <- create_gstat_from_spdf(meuse, method = "universal")
#' }
create_gstat_from_spdf <- function(spdf, method = method) {
    
    if (method == "ordinary") {
    # Create initial gstat object with first variable
    g <- gstat(id = names(spdf)[1],
               formula = as.formula(paste(names(spdf)[1], "~ 1")),
               data = spdf)
    
    # Add remaining variables if they exist
    if (length(names(spdf)) > 1) {
        for (i in 2:length(names(spdf))) {
            g <- gstat(g,
                      names(spdf)[i],
                      as.formula(paste(names(spdf)[i], "~ 1")),
                      spdf)
        }
    }
    } else if (method == "universal") {
    # Include coordinates in dataset
    spdf@data$x <- spdf@coords[, 1]
    spdf@data$y <- spdf@coords[, 2]
    # Create initial gstat object with first variable
    g <- gstat(id = names(spdf)[1],
               formula = as.formula(paste(names(spdf)[1], "~ x + y")),
               data = spdf)
    
    # Add remaining variables if they exist (except x and y))
    if (length(names(spdf)) > 1) {
        for (i in 2:(length(names(spdf)) - 2)) {
            g <- gstat(g,
                      names(spdf)[i],
                      as.formula(paste(names(spdf)[i], "~ x + y")),
                      spdf)
        }
    }
    } else {
        stop("Method not recognized. Please use 'ordinary' or 'universal'")
    }

  return(g)
}
