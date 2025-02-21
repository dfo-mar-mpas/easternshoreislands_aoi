## CTD Station Plot ----
## load libraries
library(sf)
library(tidyverse)
library(rnaturalearth)
library(MarConsNetData)
library(ggspatial)
library(terra)
library(tidyterra)
library(viridis)
library(scales)
library(patchwork)
library(units)
library(Mar.datawrangling)
library(taxize)
library(worrms)
library(terra)

#Load projections
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utmkm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#Basemaps
bioregion <- data_planning_areas()%>%
  st_transform(CanProj)%>%
  st_make_valid()

#High res coastal shapefile
coast_hr <- read_sf("data/Shapefiles/NS_coastline_project_Erase1.shp")%>%st_transform(CanProj)

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


plotlims_small <- maritimes_network%>%
  filter(name=="Eastern Shore Islands")%>%
  st_transform(utmkm)%>%
  st_buffer(2)%>% #2km buffer
  st_transform(CanProj)%>%
  st_bbox()

#load the stations for eDNA and CTDs 
ctd_stations <- read.csv("data/Oceanography/2021_EasternShore_sites.csv")%>%
                distinct(Site,.keep_all = TRUE)%>%
                st_as_sf(coords=c("long","lat"),crs=latlong)%>%
                st_transform(CanProj)%>%
                mutate(group=ifelse(grepl("ESI",Site),"ESI","River"),
                       group=ifelse(Site=="ESI-00","River",group))

esi_bathy <- terra::rast("data/Bathymetry/ESI_10m_bathy.tif")%>%
             terra::project(CanProj)

#load the bathymetry data

p1 <- ggplot()+
  geom_spatraster(data=esi_bathy)+
  geom_sf(data=coast_hr)+
  geom_sf(data=maritimes_network,fill=NA)+
  geom_sf(data=ctd_stations%>%filter(group=="ESI"),size=2,fill="white",pch=21)+
  geom_sf(data=ctd_stations%>%filter(group!="ESI"),size=2,fill="cornflowerblue",pch=21)+
  theme_bw()+
  coord_sf(expand=0,xlim = plotlims_small[c(1,3)],ylim=plotlims_small[c(2,4)])+
  annotation_scale(location="tl")+
  scale_fill_viridis(na.value = "transparent")+
  labs(fill="Depth (m)")+
  theme(legend.position = "inside",
        legend.position.inside = c(0.15,0.75),
        legend.background = element_blank())

ggsave("output/ESI_2025_CSAS/ctd_stations_esi.png",p1,height=5,width=6,units="in",dpi=600)

