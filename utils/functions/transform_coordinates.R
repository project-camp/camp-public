transform_coordinates <- function(data, epsg) {

    # Convert XY coordinates from UTM Zone 35S (EPSG:32735) to WGS84 
    # longitude/latitude (EPSG:4326)

    xy_points <- st_as_sf(data, coords = c("X", "Y"), crs = epsg)  
    longlat_points <- st_transform(xy_points, 4326)
    coords <- st_coordinates(longlat_points)
    
    data$Longitude <- coords[,1]
    data$Latitude <- coords[,2]
    return(data)
}