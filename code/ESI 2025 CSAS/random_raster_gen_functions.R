raster_to_polygon <- function(oc_rast) {
  # Get the non-NA values and convert to polygons
  valid_cells <- !is.na(values(oc_rast))  # Identifying non-NA areas
  valid_raster <- oc_rast
  values(valid_raster) <- ifelse(valid_cells, 1, NA)
  
  # Convert to a polygon using the non-NA area
  valid_polygon <- as.polygons(valid_raster)
  valid_polygon_sf <- st_as_sf(valid_polygon)
  
  return(valid_polygon_sf)
}

generate_random_oc_exact_area <- function(site, valid_polygon, oc_rast, n_sim = 100) {
  site_area <- as.numeric(st_area(site))  # Exact surface area of the site
  random_oc_values <- c()
  
  for (i in 1:n_sim) {
    # Generate a random point within the valid polygon (the non-NA raster area)
    random_point <- st_sample(valid_polygon, size = 1, type = "random") %>% st_as_sf()
    
    # Create an irregular polygon by buffering the point with a random distance
    buffer_distance <- runif(1, 10, 100)  # Random buffer distance between 10 and 100 meters
    random_polygon <- st_buffer(random_point, dist = buffer_distance)
    
    # Clip the polygon to the valid polygon (non-NA area of the raster)
    clipped_polygon <- st_intersection(random_polygon, valid_polygon)
    
    # Ensure the clipped polygon has the same area as the site
    clipped_area <- as.numeric(st_area(clipped_polygon))
    if (!is.na(clipped_area) && clipped_area > 0) {
      # Adjust the polygon's area to match the site's area
      scaling_factor <- sqrt(site_area / clipped_area)
      adjusted_polygon <- st_scale(clipped_polygon, scaling_factor, scaling_factor, random_point)
      
      # Calculate OC coverage for the adjusted polygon
      oc_value <- exactextractr::exact_extract(oc_rast, adjusted_polygon, fun = "sum", progress = FALSE)
      random_oc_values <- c(random_oc_values, oc_value)
    }
  }
  
  return(random_oc_values)
}
