## make map of salmon acoustic detections

#load libaries
library(tidyverse)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(MarConsNetData)
library(scales)
library(patchwork)

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
                                   st_transform(st_crs(coastline_hr))%>%
                                   st_bbox()%>%
                                   st_as_sfc())%>%
                 st_transform(CanProj)%>%
                 st_make_valid()

#load the coordiantes
release_locations <- read.csv("data/Acoustic/ESINSSA_releaselocs.csv") %>% 
                     st_as_sf(coords=c("RELEASE_LONGITUDE","RELEASE_LATITUDE"),crs=latlong)%>%
                     st_transform(CanProj)

reciever_locations <- read.csv("data/Acoustic/ESIAOI_NSSA_allstations.csv")%>%
                      st_as_sf(coords=c("deploy_long","deploy_lat"),crs=latlong)%>%
                      st_transform(CanProj)

reciever_lims <- reciever_locations%>%
                 st_transform(utmkm)%>%
                 st_buffer(10)%>%
                 st_transform(CanProj)%>%
                 st_bbox()

release_bound <- release_locations%>%
                  st_transform(utmkm)%>%
                  st_buffer(5)%>%
                  st_transform(CanProj)%>%
                  st_bbox()

#zoomed in a bit
release_plot_lims<- release_locations%>%
                    st_transform(utmkm)%>%
                    st_buffer(0.8)%>%
                    st_transform(CanProj)%>%
                    st_bbox()

#estuary plot
estuary_lims <- reciever_locations%>%
                filter(!grepl("SR",station),!grepl("MR",station))%>%
                st_transform(utmkm)%>%
                st_buffer(0.25)%>%
                st_transform(CanProj)%>%
                st_bbox()

#crudgy way to do this but this just will make a map that is the right extent
combo_lims <- estuary_lims
combo_lims[1] <- min(c(estuary_lims[1],release_plot_lims[1]))
combo_lims[2] <- min(c(estuary_lims[2],release_plot_lims[2]))
combo_lims[3] <- max(c(estuary_lims[3],release_plot_lims[3]))
combo_lims[4] <- max(c(estuary_lims[4],release_plot_lims[4]))


  
          
#load and trim rivers db
river_df <- read_sf("data/Shapefiles/Nova Scotia Topographic DataBase - Water Features (Line Layer)/geo_export_03d8a2fe-2e4e-4497-a8b5-a6ed75aa8b3d.shp")

#filter down to the focal region around the release locations
river_trimmed <- river_df%>%
                 st_intersection(release_bound%>%st_as_sfc()%>%st_transform(st_crs(river_df)))%>%
                 st_transform(CanProj)

#assemble the west river as a polygon
west_river <- river_trimmed%>%
              filter(rivname_1 == "West River Sheet Harbour",grepl("right", feat_desc, ignore.case = TRUE))%>%
              st_combine()%>%
              st_line_merge()%>%
              st_cast("LINESTRING")%>%
              st_make_valid()

river_coords <- st_coordinates(west_river)
closed_line <- rbind(river_coords, river_coords[1,])
west_river_polygon <- st_polygon(list(closed_line[,1:2])) %>%
                      st_sfc(crs = st_crs(west_river))

#now make the islands in the river -- TBH I am not sure how this works but it does
island_combined <- river_trimmed%>%
                  filter(rivname_1 == "West River Sheet Harbour") %>%
                  st_combine()%>%
                  st_polygonize()

# Convert to sf object
island_polygon_sf <- st_sf(geometry = island_combined)

release_locations_combined <- release_locations %>%
  group_by(RELEASE_LOCATION) %>%
  summarize(species = paste(sort(unique(COMMON_NAME_E)), collapse=" & "))


#make the plots 

p1 <- ggplot()+ #ESI zoom out 
  geom_sf(data=coastline_esi,fill="grey")+
  geom_sf(data=release_locations_combined,aes(fill=species),shape=21,size=0)+
  geom_sf(data= river_polygon,fill="white")+
  geom_sf(data=esi_poly,fill=NA)+
  geom_sf(data=reciever_locations)+
  geom_sf(data=combo_lims%>%st_as_sfc(),fill=NA,lty=2,linewidth=0.5)+
  theme_bw()+
  coord_sf(xlim=reciever_lims[c(1,3)],ylim=reciever_lims[c(2,4)],expand=0)+
  annotation_scale()+
  theme(legend.position = "inside",
        legend.title = element_blank(),
        legend.position.inside = c(0.17,0.9),
        legend.background = element_blank())

p2 <- ggplot()+ #tag release location zoom in
  geom_sf(data=coastline_esi,fill="grey")+
  geom_sf(data= river_polygon,fill="white")+
  geom_sf(data=reciever_locations,size=0.4)+
  geom_sf(data= island_polygon_sf,fill="grey")+
  geom_sf(data=river_trimmed%>%filter(!rivname_1 %in% c("West River Sheet Harbour")),col="grey30",lwd=0.25)+
  geom_sf(data=release_locations_combined,aes(fill=species),shape=21,size=3)+
  theme_bw()+
  coord_sf(expand=0,xlim=combo_lims[c(1,3)],ylim=combo_lims[c(2,4)])+
  annotation_scale()+
  labs(fill="")+
  theme(
    # legend.title = element_blank(),
    # legend.box = element_blank(),
    # legend.background = element_rect(fill = scales::alpha("white", 0.5), color = NA),
    # legend.position = "inside",  # Removes extra padding inside the legend box
    # legend.position.inside = c(0.18,0.5),
    legend.position = "none",
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "pt")
  )

#now combine the plots 

combo_plot <- p1 + inset_element(p2, left = 0.405, bottom = -0.015, right = 0.97, top = 0.49)

ggsave("output/ESI_2025_CSAS/acoustic_combo.png",combo_plot,height=5,width=7,units="in",dpi=600)

