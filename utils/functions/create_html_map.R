#' Create interactive map of sample points and structures
#' 
#' @description
#' Creates an interactive web map displaying sample points and site layout/structures,
#' with optional outlier or imputed data highlighting.
#'
#' @param dataset data.frame or sf object. Must contain columns:
#'        Longitude, 
#'        Latitude, 
#'        Serial, 
#'        and classification columns if specific map types are requested
#' @param structures sf object. Site layout/structures to be displayed as polylines
#' @param is_outlier_map logical. Whether to create a map highlighting outliers
#' @param is_imputed_map logical. Whether to create a map highlighting imputed data
#' @param outlier_column character. Name of the column containing outlier information
#' @param imputed_column character. Name of the column containing imputed data information
#' @param output_path character. Directory where HTML file will be saved
#'
#' @return Invisibly returns a leaflet map object that can be further customized.
#'         Also saves an HTML file in the output_path directory with one of three names:
#'         - <name>_samples_map.html (regular map)
#'         - <name>_samples_outliers_map.html (outlier map)
#'         - <name>_samples_imputed_map.html (imputed data map)
#'
#' @details
#' The function creates three types of maps:
#' 1. Regular sample map (default):
#'    - Clustered markers for sample points
#'    - Blue polylines for structures
#' 2. Outlier map (is_outlier_map=TRUE):
#'    - Red circles for outliers, blue for normal samples
#' 3. Imputed map (is_imputed_map=TRUE):
#'    - Green circles for imputed data, blue for original samples
#'
#' @importFrom leaflet leaflet addProviderTiles addCircleMarkers addMarkers
#' @importFrom leaflet addPolylines addLayersControl setView addLegend
#' @importFrom leaflet markerClusterOptions layersControlOptions providerTileOptions
#' @importFrom htmlwidgets saveWidget
#' @importFrom sf st_transform
#' 
#' @author Antonios Koutroumpas, Abel Ruiz-Giralt
#'
#' @examples
#' \dontrun{
#' # Create regular sample map
#' m1 <- create_html_map(samples_df, structures_sf,
#'                       output_path = "maps")
#'
#' # Create outlier map
#' m2 <- create_html_map(samples_df, structures_sf, 
#'                       is_outlier_map = TRUE,
#'                       outlier_column = "is_outlier",
#'                       output_path = "maps/outliers")
#'
#' # Create imputed data map
#' m3 <- create_html_map(samples_df, structures_sf,
#'                       is_imputed_map = TRUE, 
#'                       imputed_column = "is_imputed",
#'                       output_path = "maps/imputed")
#'
#' # Customize returned map object
#' m1 %>% addMiniMap() %>% addMeasure()
#' }
#'
#' @export

create_html_map <- function(dataset, structures, 
                            is_outlier_map = FALSE,
                            is_imputed_map = FALSE, 
                            outlier_column = "is_outlier", 
                            imputed_column = "is_imputed", 
                            output_path = output_path) {
  # Creates an interactive web map displaying sample points and site layout/structures,
  # with optional outlier highlighting.

  # Input validation
  # Validate required columns in input dataset
  required_cols <- c("Longitude", "Latitude", "Serial")
  if (!all(required_cols %in% names(dataset))) {
    stop("dataset must contain columns: ", paste(required_cols, collapse = ", "))
  }
  # Check outlier column exists if making outlier map
  if (is_outlier_map && !outlier_column %in% names(dataset)) {
    stop("outlier_column '", outlier_column, "' not found in dataset")
  }
  # Verify structures is proper spatial object
  if (!inherits(structures, "sf")) {
    stop("structures must be an sf object")
  }

  # Initialize leaflet map with base configuration
  # Set zoom levels to accommodate high-resolution satellite imagery
  m <- leaflet(dataset, options = leafletOptions(
    minZoom = 1,
    maxZoom = 22  # High zoom for detailed view
  )) %>%
    # Add base map layers
    addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
    addProviderTiles("Esri.WorldImagery", 
                    options = providerTileOptions(maxZoom = 22),
                    group = "Satellite")

  # Create different visualizations based on map type
  if(is_outlier_map) {
    # For outlier maps: add circle markers with color coding
    m <- m %>% addCircleMarkers(
      lng = ~Longitude, 
      lat = ~Latitude,
      color = ~ifelse(get(outlier_column), "red", "blue"),  # Red for outliers, blue for normal
      radius = 6,
      fillOpacity = 0.7,
      stroke = FALSE,
      # Detailed popup with sample information
      popup = ~paste("Serial:", Serial, 
                    "<br>Type:", Type,
                    "<br>Outlier:", get(outlier_column)), 
      label = ~paste("Serial:", Serial, "- Type:", Type),
      group = "Sample Points"
    ) %>%
    # Add legend to explain color coding
    addLegend(
      position = "bottomright",
      colors = c("blue", "red"),
      labels = c("Normal", "Outlier"),
      title = "Sample Points"
    )
    } else if(is_imputed_map) {
    # For imputed maps: add circle markers with color coding
    m <- m %>% addCircleMarkers(
      lng = ~Longitude, 
      lat = ~Latitude,
      color = ~ifelse(get(imputed_column), "green", "blue"),  # Green for imputed, blue for normal
      radius = 6,
      fillOpacity = 0.7,
      stroke = FALSE,
      # Detailed popup with sample information
      popup = ~paste("Serial:", Serial, 
                    "<br>Type:", Type,
                    "<br>Imputed:", get(imputed_column)), 
      label = ~paste("Serial:", Serial, "- Type:", Type),
      group = "Sample Points"
    ) %>%
    # Add legend to explain color coding
    addLegend(
      position = "bottomright",
      colors = c("blue", "red"),
      labels = c("Normal", "Imputed"),
      title = "Sample Points"
    )
  } else {
    # For regular maps: add clustered markers
    m <- m %>% addMarkers(
      lng = ~Longitude, 
      lat = ~Latitude,
      popup = ~paste("Serial:", Serial, 
                    "<br>Type:", Type),
      group = "Sample Points",
      # Configure marker clustering for better visualization
      clusterOptions = markerClusterOptions(
        maxClusterRadius = 5,         # Small radius for detailed clustering
        spiderfyOnMaxZoom = TRUE,    # Enable point spread on max zoom
        showCoverageOnHover = TRUE,   # Show cluster coverage on hover
        zoomToBoundsOnClick = TRUE,   # Auto-zoom to cluster bounds
        singleMarkerMode = FALSE,
        animateAddingMarkers = TRUE,
        disableClusteringAtZoom = 22  # Stop clustering at max zoom
      ),
      label = ~paste("Serial:", Serial, "- Type:", Type)
    )
  }

  # Add site structures as polylines
  m <- m %>%
    addPolylines(
      data = st_transform(structures, 4326),  # Transform to WGS84
      color = "blue",
      weight = 2,
      opacity = 1,
      popup = ~Name,
      group = "Structures"
    ) %>%
    # Add layer controls for toggling map elements
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Satellite"),
      overlayGroups = c("Sample Points", "Structures"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    # Center view on sample points
    setView(
      lng = mean(dataset$Longitude), 
      lat = mean(dataset$Latitude), 
      zoom = 15
    )
  
  # Save map to HTML file
  if (is_outlier_map) {
    # Create temporary directory for libraries
    temp_dir <- normalizePath(file.path(output_path, "temp"), winslash = "/")
    if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, showWarnings = FALSE)
    }
    
    # Save outlier map
    saveWidget(m, 
              file.path(output_path, paste0(name, "_samples_outliers_map.html")), 
              selfcontained = TRUE, 
              libdir = temp_dir)
    
    # Clean up temporary directory
    unlink(temp_dir, recursive = TRUE)

  } else if (is_imputed_map) {
    # Create temporary directory for libraries
    temp_dir <- normalizePath(file.path(output_path, "temp"), winslash = "/")
    if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, showWarnings = FALSE)
    }
    
    # Save imputed map
    saveWidget(m, 
              file.path(output_path, paste0(name, "_samples_imputed_map.html")), 
              selfcontained = TRUE, 
              libdir = temp_dir)
    
    # Clean up temporary directory
    unlink(temp_dir, recursive = TRUE)

  } else {
    # Same process for regular map
    temp_dir <- normalizePath(file.path(output_path, "temp"), winslash = "/")
    if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, showWarnings = FALSE)
    }

    saveWidget(m, 
              file.path(output_path, paste0(name, "_samples_map.html")), 
              selfcontained = TRUE, 
              libdir = temp_dir)
    unlink(temp_dir, recursive = TRUE)
  }
  
  # Return map object invisibly for potential further modification
  invisible(m)
}
