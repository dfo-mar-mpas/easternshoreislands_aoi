#Plot of the Relative Exposure Index (O'Brien et al. 2022 - https://figshare.com/collections/_/5433567)

#load libraries
library(terra)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(MarConsNetData)
library(tidyterra)
library(ggspatial)

#Load projections
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utmkm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#load the ESI REI layer
esi_rei <- rast("data/REI/rei_scaled.tif")

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

#Create basemap intersected with the bounding box. 
basemap_atlantic <- rbind(ne_states(country = "Canada",returnclass = "sf")%>%
                            dplyr::select(name_en,geometry)%>%
                            st_as_sf()%>%
                            st_union()%>%
                            st_transform(latlong)%>%
                            st_as_sf()%>%
                            mutate(country="Canada"),
                          ne_states(country = "United States of America",returnclass = "sf")%>%
                            dplyr::select(name_en,geometry)%>%
                            st_as_sf()%>%
                            st_union()%>%
                            st_transform(latlong)%>%
                            st_as_sf()%>%
                            mutate(country="USA"))%>%
  st_transform(CanProj)

coastline_hr <- read_sf("data/Shapefiles/NS_coastline_project_Erase1.shp")

coastline_esi <- coastline_hr%>%
  st_intersection(esi_lims%>%
                    st_as_sfc()%>%
                    st_transform(st_crs(coastline_hr))%>%
                    st_bbox()%>%
                    st_as_sfc())%>%
  st_transform(CanProj)%>%
  st_make_valid()


#Mask and the REI
rei_df <- esi_rei%>%
          terra::crop(.,esi_lims%>%st_as_sfc()%>%st_transform(st_crs(esi_rei)))%>%
          terra::mask(.,esi_poly%>%st_transform(st_crs(esi_rei)))

rei_mask <- !is.na(rei_df)

rei_df_cropped <- rei_df%>%
          terra::crop(.,rei_mask)%>%
          trim(.)%>%
          terra::project(CanProj) #takes a second

esi_rei_lims <- st_bbox(rei_df_cropped)


p_rei <- ggplot()+
  geom_spatraster(data=rei_df_cropped)+
  geom_sf(data=maritimes_network,fill=NA)+
  geom_sf(data=esi_poly,fill=NA)+
  geom_sf(data=coastline_esi)+
  coord_sf(xlim=esi_rei_lims[c(1,3)],ylim=esi_rei_lims[c(2,4)],expand=0)+
  theme_bw()+
  scale_fill_viridis_c(option = "viridis", direction = 1,na.value = NA)+
  annotation_scale(location="br")+
  labs(fill="REI")+
  theme(legend.position = "inside",
        legend.position.inside = c(0.05,0.85),
        legend.background = element_blank(),
        legend.title = element_text(face="bold"))

ggsave("output/ESI_2025_CSAS/esi_rei.png",p_rei,height=6,width=7,units="in",dpi=300)
  
