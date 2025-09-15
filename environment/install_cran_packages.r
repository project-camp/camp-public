# install_cran_packages.R
cat("Installing specialized CRAN packages...\n")

# Environment check
if (!grepl("camp-public|camp_public", Sys.getenv("CONDA_DEFAULT_ENV", ""), ignore.case = TRUE)) {
  warning("Not running in expected conda environment. Current: ", 
          Sys.getenv("CONDA_DEFAULT_ENV", "none"))
}

# ONLY packages NOT available in conda-forge
cran_only_packages <- c(
  "gmGeostats",      # Geostatistics - essential for MAF analysis
  "zCompositions",   # Compositional data imputation - essential 
  "MVN",             # Multivariate normality tests - used in notebooks
  "biotools",        # Statistical tests - used in discriminant analysis
  "IRdisplay"        # Jupyter display - needed for notebooks
)

# Installation function
install_cran_packages <- function(packages) {
  missing_pkgs <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_pkgs) == 0) {
    cat("All packages already installed.\n")
    return(TRUE)
  }
  
  failed_packages <- character(0)
  
  for (pkg in missing_pkgs) {
    cat(sprintf("Installing %s...\n", pkg))
    result <- tryCatch({
      install.packages(pkg, dependencies = TRUE, repos = "https://cran.rstudio.com/")
      TRUE
    }, error = function(e) {
      cat(sprintf("Failed to install %s: %s\n", pkg, e$message))
      failed_packages <<- c(failed_packages, pkg)
      FALSE
    })
    
    if (result) cat(sprintf("SUCCESS: %s installed\n", pkg))
  }
  
  if (length(failed_packages) > 0) {
    cat(sprintf("\nFailed packages: %s\n", paste(failed_packages, collapse = ", ")))
    cat("Core analysis should still work despite these failures.\n")
  }
  
  return(length(failed_packages) == 0)
}

install_cran_packages(cran_only_packages)
cat("CRAN package installation complete!\n")