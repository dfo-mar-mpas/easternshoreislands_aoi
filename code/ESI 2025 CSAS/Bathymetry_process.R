#Analysis of bathymetric data ESI

#load libraries
library(terra)
library(tidyterra)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(MarConsNetData)
library(viridis)
library(ggspatial)
library(ggridges)

#Load projections
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utmkm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#Basemaps
bioregion <- data_planning_areas()%>%
  st_transform(CanProj)%>%
  st_make_valid()

#plotlimits
plotlims <- bioregion%>%
  st_transform(utmkm)%>%
  st_buffer(20)%>% #50km buffer
  st_transform(CanProj)%>%
  st_bbox()

#maritimes network -- note that this cannot be shared or made available online to the public
maritimes_network <- data_draft_areas()%>%
  st_transform(CanProj)%>%
  st_make_valid()%>%
  dplyr::select(Classification_E,SiteName_E)%>%
  rename(status=Classification_E,name=SiteName_E)%>%
  st_make_valid()%>%
  st_intersection(bioregion) #clean up the edges

esi_poly <- maritimes_network%>%
  filter(name == "Eastern Shore Islands")

esi_lims <- esi_poly%>%
  st_transform(utmkm)%>%
  st_buffer(10)%>%
  st_transform(CanProj)%>%
  st_bbox()

coastline_hr <- read_sf("data/Shapefiles/NS_coastline_project_Erase1.shp")%>%st_transform(CanProj)

#load the 10m resolution geotiff (from L. Teed and P. Lawton upscaled from 1m resolution)
esi_dem <- rast("data/Bathymetry/ESI_10m_bathy.tif") %>% project(CanProj)

esi_dem_gaps <- read_sf("data/Bathymetry/esi_bathy_gaps.shp") %>% st_transform(CanProj)

round(esi_dem_gaps%>%st_union()%>%st_area()/st_area(esi_poly),4)*100 #this is from the polylgons but below the number comes to be 7.0

p1 <- ggplot()+
  geom_spatraster(data=esi_dem)+
  geom_sf(data=coastline_hr)+
  geom_sf(data=esi_dem_gaps,col="black",fill=NA)+
  geom_sf(data=esi_poly,fill=NA)+
  scale_fill_viridis(na.value = "transparent")+
  theme_bw()+
  annotation_scale(location="bl")+
  coord_sf(expand=0,xlim=esi_lims[c(1,3)],ylim=esi_lims[c(2,4)])+
  labs(fill="Depth (m)")+
  theme(legend.position = "inside",
        legend.position.inside = c(0.85,0.2),
        legend.background = element_blank())

ggsave("output/ESI_2025_CSAS/esi_gaps_raster.png",p1,height=5,width=5,units="in",dpi=300)

#for presentation

ctd_stations <- read.csv("data/Oceanography/2021_EasternShore_sites.csv")%>%
                distinct(Site,.keep_all = TRUE)%>%
                st_as_sf(coords=c("long","lat"),crs=latlong)%>%
                st_transform(CanProj)%>%
                mutate(group=ifelse(grepl("ESI",Site),"ESI","River"),
                       group=ifelse(Site=="ESI-00","River",group))%>%
                filter(Site %in% c( "ESI-4","ESI-3","ESI-2","ESI-1"))
                            

contour_line <- as.contour(esi_dem, levels = -60)%>%
  st_as_sf()%>%
  st_transform(CanProj)%>%
  st_intersection(esi_poly)


p1_pres <- ggplot()+
  geom_spatraster(data=esi_dem)+
  geom_sf(data=coastline_hr)+
  geom_sf(data=esi_poly,fill=NA)+
  geom_sf(data=contour_line,linewidth=0.3,col="black")+
  geom_sf(data=ctd_stations,shape=21,fill="white",size=1.3)+
  scale_fill_viridis(na.value = "transparent",option="magma")+
  theme_bw()+
  coord_sf(expand=0,xlim=esi_lims[c(1,3)],ylim=esi_lims[c(2,4)])+
  labs(fill="Depth (m)")+
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank())

        
        ggsave("output/ESI_2025_CSAS/esi_map_pres.png",p1_pres,height=5,width=5,units="in",dpi=300,bg = "transparent")

#ridgeline plot
        
        # #is slow but seems to run slightly faster using non-dplyr
        bathymetry_values <- as.data.frame(values(esi_dem), col.names = "depth")

        # Remove NA values
        bathymetry_values <- na.omit(bathymetry_values)

        #clean up names
        names(bathymetry_values) <- "depth"
        
        bathymetry_df <- bathymetry_values%>%
                        rename(depth = 1)%>%
                        sample_n(sample_frac)%>%
                        mutate(depth_bin = floor(depth / 2) * 2) %>%
                        filter(depth<0)

        
        # save(bathymetry_values,file="data/bathymetry_values_cut_2m.RData")
        load("data/bathymetry_values_cut_2m.RData")
        
        sample_size <- ceiling(nrow(bathymetry_values) * 0.05)
        
        bathymetry_df <- bathymetry_values %>%
          sample_n(sample_size) %>%
          filter(depth < 0)
        
        # Add a constant column for the y-axis
        bathymetry_df$y <- "Depth Profile"
        
        # Create a single ridge plot
        p1_ridge <- ggplot(bathymetry_df, aes(x = depth, y = y, fill = ..x..)) +
          geom_density_ridges_gradient(scale = 10, rel_min_height = 0.01, justify = "right") + # Justify to right
          scale_fill_viridis_c(option = "magma", direction = 1) +
          coord_flip() +
          #scale_x_reverse() +  # Makes shallower depths appear at the top
          theme_ridges() +
          labs(y = "", x = "Depth (m)", fill = "Depth (m)") +
          theme(
            legend.position = "right",
            # Keep the y-axis text but make it smaller
            axis.text.y = element_text(size = 8),
            # Adjust plot margins to reduce whitespace
            plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
            # Adjust the panel margins
            panel.spacing = unit(0.5, "lines"),
            panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA)
          ) +
          # This expands the plot to fill available space
          scale_y_discrete(expand = c(0, 0))
        
        ggsave("output/ESI_2025_CSAS/esi_depth_ridges.png",p1_ridge,height=8,width=5,units="in",dpi=600,bg="transparent")
        
      
# sheet_harbour_nonna <- paste0("data/Bathymetry/NONNA/NONNA10_",c("4480N06260W","4480N06250W","4490N06260W","4490N06250W"),".tiff")
# 
# raster_list <- lapply(sheet_harbour_nonna, rast)
# sheet_harbour_rast <- do.call(merge, raster_list)%>%project(CanProj)
# 
# 
# sheet_harbour_rast_resampled <- resample(sheet_harbour_rast, esi_dem, method = "bilinear")
# 
# merged_rast <- merge(esi_dem, sheet_harbour_rast_resampled)
# 
# terra::writeRaster(merged_rast,"data/Bathymetry/esi_dem_with_sheetharbour.tiff")


#NONNA 10 data analysis 

# raster_files <- list.files("data/NONNA/Bathymetry/",pattern="NONNA10_",full.names=TRUE)
# raster_list <- lapply(raster_files, rast)
# combined_raster_10 <- do.call(merge, raster_list)
# 
# esi <- esi_poly%>%st_transform(st_crs(combined_raster_10))

# # First identify NA cells in high-res raster within the polygon
# multibeam <- combined_raster_10%>%crop(esi)
# multibeam_mask <- multibeam%>%mask(esi)
# 
# 
# total_nan <- sum(is.nan(values(multibeam_mask)))
# total_na <- sum(is.na(values(multibeam_mask))) - total_nan
# 
# nan_proportion <- round(total_nan/(ncell(multibeam_mask) - total_na),3)*100
# 
# nan_proportion
# 
# #to demonstrate the nan counting is working
# r <- app(multibeam_mask, fun = function(x) { ifelse(is.nan(x), 1000, x) }) 
# plot(r)

# # Identify NaN values (terra treats NaN as a separate value from NA)
# nan_raster <- app(multibeam_mask, function(x) ifelse(is.nan(x), 1, 0))
# 
# # Add a step to reclassify connected NaN regions
# nan_raster_clumped <- terra::clamp(terra::patches(nan_raster,zeroAsNA = TRUE), lower=1, upper=1)
# 
# save(nan_raster_clumped,file="data/esiBathy_nan_raster_clumped.RData") #save because it takse a while to make this 
# 
# # Convert to polygons, which should now have fewer, larger regions
# nan_polygons <- as.polygons(nan_raster_clumped, dissolve = TRUE)
# 
# nan_sf <- nan_polygons %>%
#   st_as_sf() %>%
#   st_make_valid() %>%
#   st_cast("POLYGON") %>%
#   mutate(area=as.numeric(st_area(nan_sf)) / 1e6 )%>%
#   filter(area>5) #only the blank zones smaller than 5km2
# 
# write_sf(nan_sf,dsn="data/Bathymetry/esi_bathy_gaps.shp")
