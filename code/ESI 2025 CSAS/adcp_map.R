## eDNA survey map

#load libraries ----
library(dplyr)
library(sf)
library(tidyr)
library(tidyverse)
library(rnaturalearth)
library(marmap)
library(ggplot2)
library(ggspatial)
library(ggnewscale)
library(viridis)
library(raster)
library(stars)


s2_as_sf = FALSE

#projections ------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load adcp coodinates
adcp <- read.csv("data/Oceanography/adcp_metadata_2019.csv")%>%
        filter(year==2019,equipment=="adcp")%>% #just use the 2019 coordinates
        st_as_sf(coords=c("long","lat"),crs=latlong)%>%
        mutate(line = ifelse(grepl("liscomb",name),"Liscomb","Ship Harbour"))

ctd <- read.csv("data/Oceanography/HUD2018021_CTDs.csv")%>%
       st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)

#load shapefiles
esi_poly <- read_sf("data/Shapefiles/EasternShoreIslands_networksite.shp")%>%
  st_transform(latlong)

bounding_area <- esi_poly%>%
  st_transform(utm)%>%
  st_buffer(dist = 10)%>%
  st_bbox()%>%
  st_as_sfc()%>%
  st_as_sf()%>%
  st_transform(latlong)%>%
  st_bbox()%>%
  st_as_sfc()%>%
  st_as_sf()%>%
  suppressWarnings()%>%
  suppressMessages()

aoi_plot_lims <- esi_poly%>%
  st_bbox()%>%
  st_as_sfc()%>%
  st_as_sf()%>%
  st_transform(utm)%>%
  st_buffer(5)%>%
  st_transform(latlong)%>%
  st_bbox()

esi_coast <- st_read("data/Shapefiles/coast_hr_esi.shp")%>%st_transform(latlong)

#Bathymetry data extracted from 35m dem 
esi_bathy <- raster("data/esi_bath.tif")
values(esi_bathy) <- values(esi_bathy)*-1
names(esi_bathy) <- "Depth"

#generate plot with ESI locations for readme
esi_map <- ggplot()+
  geom_sf(data=esi_poly,fill=NA)+
  geom_stars(data=esi_bathy%>%st_as_stars())+
  scale_fill_viridis(option="C",na.value="white")+
  labs(fill="Depth (m)")+
  geom_sf(data=esi_poly,fill=NA,col="black")+
  geom_sf(data=esi_coast,fill="darkolivegreen3")+
  new_scale_fill()+
  geom_sf(data=adcp,aes(fill = line),shape=21,size=2.5,show.legend = FALSE)+
  geom_sf(data=ctd,size=1.4,col="black")+
  scale_fill_manual(values=c("white","cornflowerblue"))+
  theme_bw()+
  coord_sf(expand=0,xlim=aoi_plot_lims[c(1,3)],ylim=aoi_plot_lims[c(2,4)])+
  theme(axis.text=element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.9,0.2))+
  labs(x="",y="",col="",shape="",size="",fill="")+
  #annotation_scale(location="br")+
  annotation_north_arrow(location="tl")

ggsave("output/ESI 2025 CSAS/esi_map_adcp.png",esi_map,width=7,height=5,units="in",dpi=300)

