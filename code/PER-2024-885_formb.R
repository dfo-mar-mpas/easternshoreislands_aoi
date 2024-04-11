#Code to generate map for PER-2024-885 (April 2024) form b

#load libraries ----
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(ggplot2)
library(marmap)
library(patchwork)
library(viridis)
library(geosphere)

s2_as_sf = FALSE
#projections ------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load stations 
esi_stations <- read.csv("data/Acoustic/2023_Acoustic_stations.csv")%>%
                filter(!name %in% paste0("MR_0",1:3))%>%
                rename(lon=longitude,lat=latitude)%>%
                st_as_sf(coords=c("lon","lat"),crs=latlong,remove=FALSE)


sheet_harbour_port <- data.frame(lon=-62.504091,lat=44.903582,name="Port of Sheet Harbour")%>%
  st_as_sf(coords=c("lon","lat"),crs=latlong)

#load AOI polygon --- 
esi_poly <- read_sf("data/Shapefiles/EasternShoreIslands_networksite.shp")%>%
            st_transform(latlong)


#load coastline ------
coast_hr <- read_sf("data/Shapefiles/NS_coastline_project_Erase1.shp")%>%
            st_transform(latlong)


basemap_extent <- esi_stations%>%
  st_bbox()%>%
  st_as_sfc()%>%
  st_as_sf()%>%
  st_transform(utm)%>%
  st_buffer(2)%>%
  st_transform(st_crs(coast_hr))%>%
  st_bbox()%>%
  st_as_sfc()%>%
  st_as_sf()

aoi_extent <- esi_poly%>%
  st_bbox()%>%
  st_as_sfc()%>%
  st_as_sf()%>%
  st_transform(utm)%>%
  st_buffer(5)%>%
  st_transform(st_crs(coast_hr))%>%
  st_bbox()%>%
  st_as_sfc()%>%
  st_as_sf()

basemap_lims <- basemap_extent%>%st_transform(latlong)%>%st_bbox()
aoi_lims <- aoi_extent%>%st_transform(latlong)%>%st_bbox()

#crop shapefiles 
coast_hr_crop <- coast_hr%>%
                st_intersection(basemap_extent)%>%
                st_transform(latlong)

coast_hr_crop_esi <- coast_hr%>%
                      st_intersection(aoi_extent)%>%
                      st_transform(latlong)


#bathymetry -------
curdir <- getwd()
setwd("data/Bathymetry/")

noaabathy <- getNOAA.bathy(aoi_lims[1]-1,aoi_lims[3]+1,aoi_lims[2]+1,aoi_lims[4]-1,resolution = 0.25,keep=T)

isobath_df <- as.xyz(noaabathy)%>%
              rename(lon=1,lat=2,depth=3)

setwd(curdir)

#extract depth for the stations
esi_stations <- esi_stations%>%
                mutate(depth=get.depth(mat=noaabathy,x=esi_stations%>%data.frame()%>%select(lon,lat),locator=FALSE)%>%pull(depth))

#make plot ------
p1 <- ggplot()+
  geom_sf(data=basemap_lims%>%st_as_sfc(),fill=NA,lty=2,col="grey40",lwd=0.5)+
  geom_contour(data=isobath_df,aes(x=lon,y=lat,z=depth),breaks=-100,color = "grey80", size = 0.5)+
  geom_contour(data=isobath_df,aes(x=lon,y=lat,z=depth),breaks=-50,color = "grey80", size = 0.5,lty=3)+
  geom_sf(data=esi_poly,fill=NA)+
  geom_sf(data=coast_hr_crop_esi)+
  geom_sf(data=sheet_harbour_port,size=3)+
  geom_sf_label(data=sheet_harbour_port,aes(label=name),nudge_x = 0.02,nudge_y = 0.07)+
  geom_sf(data=esi_stations,pch=21,fill="cornflowerblue",col="black",size=1.5)+
  theme_bw()+
  coord_sf(expand=0,xlim = aoi_lims[c(1,3)],ylim=aoi_lims[c(2,4)])+
  labs(x="",y="",title = "ESIAOI Array")

p2 <- ggplot()+
  geom_sf(data=esi_poly,fill=NA)+
  geom_contour(data=isobath_df,aes(x=lon,y=lat,z=depth),breaks=-100,color = "grey80", size = 0.5)+
  geom_contour(data=isobath_df,aes(x=lon,y=lat,z=depth),breaks=-50,color = "grey80", size = 0.5,lty=3)+
  geom_sf(data=coast_hr_crop_esi)+
  geom_sf(data=sheet_harbour_port,size=3)+
  geom_sf_label(data=sheet_harbour_port,aes(label=name),nudge_x = 0.02,nudge_y = 0.02)+
  geom_sf(data=esi_stations,pch=21,aes(fill=depth),col="black",size=2)+
  scale_fill_viridis()+
  theme_bw()+
  theme(axis.text = element_blank())+
  coord_sf(expand=0,xlim = basemap_lims[c(1,3)],ylim=basemap_lims[c(2,4)])+
  labs(x="",y="",fill="Depth (m)")

p3 <- p1 + p2 + plot_layout(ncol=2)

ggsave("output/PER-2024-885.png",p3,width=10,height=7,units="in",dpi=300)

#Missing station
missing_station <- data.frame(lon=-62.0442,lat=44.77725,name="MR_20")%>% # coordinates from the Perley Triangulation exercise
                  st_as_sf(coords=c("lon","lat"),crs=latlong,remove=FALSE)

missing_buffer <- missing_station%>%
                  st_transform(utm)%>%
                  st_buffer(173.205/1000) #buffer (radius) of a cone in m based on 200m of rope and 100 m depth
                  st_transform(latlong)
                  
missing_range <- missing_station%>%
                  st_transform(utm)%>%
                  st_buffer(1.5)%>% #buffer (radius) of a cone in m based on 200m of rope and 100 m depth
                  st_transform(latlong)%>%
                  st_bbox()

inset_range <- rbind(sheet_harbour_port,esi_stations%>%dplyr::select(names(sheet_harbour_port)))%>%
               st_bbox()%>%
               st_as_sfc()%>%
               st_transform(utm)%>%
               st_buffer(5)%>%
               st_transform(latlong)%>%
               st_bbox()
                  
p4 <- ggplot()+
  geom_sf(data=missing_range%>%st_as_sfc(),fill=NA,lty=2,col="grey40",lwd=0.5)+
  geom_contour(data=isobath_df,aes(x=lon,y=lat,z=depth),breaks=-100,color = "grey80", size = 0.5)+
  geom_contour(data=isobath_df,aes(x=lon,y=lat,z=depth),breaks=-50,color = "grey80", size = 0.5,lty=3)+
  #geom_sf(data=esi_poly,fill=NA)+
  geom_sf(data=coast_hr_crop_esi)+
  geom_sf(data=esi_stations,pch=21,fill="cornflowerblue",col="black",size=1.5)+
  geom_sf(data=missing_station,pch=21,fill="red",size=2)+
  theme_bw()+
  coord_sf(expand=0,xlim = inset_range[c(1,3)],ylim=inset_range[c(2,4)])+
  labs(x="",y="",title = "ESIAOI Array")

p5 <- ggplot()+
  geom_sf(data=esi_poly,fill=NA)+
  geom_contour(data=isobath_df,aes(x=lon,y=lat,z=depth),breaks=-100,color = "grey80", size = 0.5)+
  geom_contour(data=isobath_df,aes(x=lon,y=lat,z=depth),breaks=-50,color = "grey80", size = 0.5,lty=3)+
  geom_sf(data=esi_stations%>%filter(name != "MR_20"),pch=3,col="black",size=2)+
  geom_sf(data=missing_station,pch=21,fill="red",size=2)+
  geom_sf(data=missing_buffer,lty=2,fill=NA)+
  theme_bw()+
  theme(axis.text = element_blank())+
  coord_sf(expand=0,xlim = missing_range[c(1,3)],ylim=missing_range[c(2,4)])+
  labs(x="",y="",fill="Depth (m)")

#total distance of the array 
esi_stations%>%
  filter(name %in% c("MR_04","MR_26"))%>%
  st_distance()%>%
  matrix()%>%
  max()/1000*0.539957

esi_stations%>%
  filter(name %in% c("MR_26","MR_04"))%>%
  mutate(name = factor(name,levels=c("MR_26","MR_04")))%>%
  arrange(name)%>%
  as_Spatial()%>%
  bearing()

esi_stations%>%
  filter(name %in% c("MR_04","MR_26"))%>%
  as_Spatial()%>%
  bearing()