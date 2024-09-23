#load libraries
library(sf)
library(ggplot2)
library(dplyr)

#projections
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load the ESI polygon
esi <- read_sf("data/Shapefiles/EasternShoreIslands_networksite.shp")%>%st_transform(latlong)

#load stations coordinates for the beach seining study
bs_coords <- read.csv("data/Seining/2023_stations.csv")%>%
             st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)

bs_bounding <- bs_coords%>%
               st_bbox()%>%
               st_as_sfc()%>%
               st_as_sf()%>%
               st_transform(utm)%>%
               st_buffer(75)%>% # 75 km boundary
               st_bbox()%>%
               st_as_sfc()%>%
               st_as_sf()%>%
               st_transform(latlong)

old_bs_coords <- read.csv("data/Seining/2019_stations.csv") %>% mutate(Longitude2=Longitude*-1) %>% 
  sf::st_as_sf(coords=c("Longitude2","Latitude"),crs=latlong)

#load high resolution coastline
    # coast_hr <- read_sf("r:/Science/CESD/HES_MPAGroup/Data/Shapefiles/Coastline/NS_coastline_project_Erase1.shp")%>%
    #             st_transform(latlong)
    # 
    # coast_hr_zoom <- coast_hr%>%st_intersection(.,bs_bounding)
    # 
    # write_sf(coast_hr_zoom,"data/shapefiles/beach_seining_coast_hr.shp")
  coast_hr_zoom <- read_sf("data/shapefiles/NS_coastline_project_Erase1.shp")
  

p1 <- ggplot()+
  geom_sf(data=esi,fill=NA, linewidth=1.25)+
  geom_sf(data=coast_hr_zoom)+
  geom_sf(data=old_bs_coords, size=4, shape=24, color="black",fill="red")+
  geom_sf(data=bs_coords,size=5, shape=20)+
  coord_sf(xlim=c(-61, -64.5), ylim=c(44.2,45.3), expand=F)+
  theme_bw()
p1

ggsave("ESI_SeiningSites.png",plot = p1, device = "png", path = "output/2024CSAS",width = 8, height = 6, units = "in", dpi = 320, create.dir = TRUE)
