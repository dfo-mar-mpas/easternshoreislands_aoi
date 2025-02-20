process_raster_intersection <- function(raster_path, bioregion, maritimes_network, species, can_proj = "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") {
  
  require(terra)
  require(tidyverse)
  require(sf)
  
  
  # Read the raster
  raster_data <- terra::rast(raster_path)
  raster_proj <- terra::crs(raster_data)
  
  # Transform input spatial objects to raster projection
  bioregion_transformed <- st_transform(bioregion, raster_proj)
  maritimes_transformed <- st_transform(maritimes_network, raster_proj)
  
  # Crop raster to bioregion extent
  raster_cropped <- terra::crop(raster_data, terra::vect(bioregion_transformed))
  
  # Create a mask for presence values (value == 1)
  if(sp != "Kelp"){presence_mask <- raster_cropped == 1}else{presence_mask <- !is.na(raster_cropped) & raster_cropped != 0} #this has more than one value - note that this is very specific so may not work if the values change
  
  # Apply mask to keep only presence values
  raster_presence <- raster_cropped * presence_mask
  
  # Convert raster to polygon and transform to sf, keeping only presence values
  raster_polygon <- terra::as.polygons(raster_presence, values=TRUE, dissolve=TRUE) %>%
    st_as_sf() %>%
    rename(layer=1)%>%
    filter(!is.na(layer) & layer == 1) %>%
    st_transform(can_proj)
  
  # Calculate total area of presence regions
  total_area <- raster_polygon %>%
    st_union() %>%
    st_as_sf() %>%
    st_union() %>%
    st_area() / 1000 / 1000 %>%
    as.numeric()
  
  # Perform intersection and calculate statistics
  intersection_result <- raster_polygon %>%
    st_union() %>%
    st_as_sf() %>%
    st_intersection(maritimes_network) %>%
    mutate(
      area = as.numeric(st_area(.)) / 1000 / 1000,
      prop_area = round(area / sum(area), 2),
      prop_region = as.numeric(round(area / total_area, 2)),
      total_regional_extent = as.numeric(total_area)
    ) %>%
    arrange(prop_region) %>%
    mutate(name = factor(name, levels = name),
           sp = species)%>%
    rename(geometry=x)%>%
    dplyr::select(sp,name,area,prop_area,prop_region,total_regional_extent,geometry)%>%
    arrange(-prop_region)
  
  return(intersection_result)
}