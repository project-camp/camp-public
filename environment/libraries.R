# libraries.R - Load all required packages

# Required packages (installed via conda or CRAN)
required_packages <- c(
  # Data manipulation
  "tidyverse",
  
  # Spatial packages  
  "sf", "sp", "raster", "stars", "gstat", "spdep", "automap", 
  "dbscan", "gridExtra", "lattice",

  # Visualization
  "colorspace", "viridis", "patchwork", "leaflet", "classInt", "knitr",
  
  # File I/O
  "readxl", "jsonlite", "htmlwidgets", "IRdisplay",
  
  # Statistics
  "car", "MASS", "vegan", "NbClust", "biotools",  # Removed MVN
  
  # Compositional data
  "compositions", "zCompositions", "gmGeostats"
)

# Function to check and load packages (no auto-installation)
load_packages <- function(packages) {
    missing_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
  
    if(length(missing_packages) > 0) {
        stop("Missing packages: ", paste(missing_packages, collapse = ", "), 
             "\nInstall missing packages via conda or CRAN script")
    }
  
    # Load all packages silently
    suppressPackageStartupMessages({
        invisible(lapply(packages, library, character.only = TRUE))
    })
    
    cat("All packages loaded successfully\n")
    return(TRUE)
}

# Load all packages
load_packages(required_packages)