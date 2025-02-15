#Figure 1 for the Acoustic Report

#load libaries
library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(MarConsNetData)
library(scales)
library(patchwork)
library(ggnewscale)
library(knitr)
library(stars)
library(viridis)

#Load projections
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utmkm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#qualified detections
qdet_df <- read.csv("data/Acoustic/ESI_qdets_est_rellocs.csv")%>%
  st_as_sf(coords=c("Rel.longitude","Rel.latitude"),crs=latlong,remove=FALSE)%>%
  st_transform(CanProj)

large_lim <- qdet_df%>%
  st_transform(latlong)%>%
  st_buffer(3)%>%
  st_bbox()

large_lim[1] <- -81 #sets the extent of our region 
large_lim[2] <- 23
large_lim[3] <- -54
large_lim[4] <- 50


#grab the Canadian closures. 

Atlantic <- data_bioregion("Atlantic")%>%
            st_make_valid()%>%
            st_union()%>%
            st_as_sf()%>%
            st_make_valid()%>%
            st_transform(4326)

# closures <- data_CPCAD_areas(Atlantic)%>%
#             st_transform(CanProj)%>%
#             st_as_sf()
# 
# write_sf(closures,dsn="data/Shapefiles/atlantic_closures.shp")

closures <- read_sf("data/Shapefiles/atlantic_closures.shp")
west_river <- read_sf("data/Shapefiles/west_river.shp")
west_river_islands <- read_sf("data/Shapefiles/west_river_islands.shp")
noaa_closures <- read_sf("r:/Science/CESD/HES_MPAGroup/Data/Shapefiles/NOAA MPAs/NOAA_MPAs.shp")
noaa_closures <- noaa_closures%>%
                 st_intersection(large_lim%>%st_as_sfc()%>%st_transform(st_crs(noaa_closures)))

#reciever locations
reciever_locations <- read.csv("data/Acoustic/ESIAOI_NSSA_allstations.csv")%>%
  st_as_sf(coords=c("deploy_long","deploy_lat"),crs=latlong)%>%
  st_transform(CanProj)

#Query OTN for reciever locations
proj_long_upp <- -40.00 #encompassing the regional network
proj_long_low <- -70.00
proj_lat_upp <- 60.00
proj_lat_low <- 40.00

geoserver_receivers <- readr::read_csv('https://members.oceantrack.org/geoserver/otn/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=otn:stations_receivers&outputFormat=csv', guess_max = 13579)

otn_stations <- geoserver_receivers %>%
  filter(!is.na(deploy_date)) %>% # Remove rows with missing deploy_date values
  filter(stn_lat >= proj_lat_low & stn_lat <= proj_lat_upp &# Filter stations based on latitude and longitude bounds
           stn_long >= proj_long_low & stn_long <= proj_long_upp)%>%
  st_as_sf(coords=c("stn_long","stn_lat"),crs=latlong)%>%
  st_transform(CanProj)%>%
  mutate(marine = is.na(as.numeric(st_intersects(., basemap_atlantic))),
         halifax_line = ifelse(grepl("HFX",station_name),"Halifax line","OTN"),
         year=year(as.POSIXct(deploy_date)))%>%
  filter(marine,
         year>2018)


#load the ESI dem
esi_dem <- rast("data/Bathymetry/esi_dem_with_sheetharbour.tiff")

#load coastline and make basemap ------
coast_hr <- read_sf("data/shapefiles/NS_coastline_project_Erase1.shp")%>%st_transform(CanProj)



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

#get global basemap 
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

land <- ne_countries(scale = "medium", returnclass = "sf")

#make rings
esi_centre <- esi_poly%>%
            st_centroid()

esi_centre_terra <- esi_centre%>%vect()

rings <- NULL

for(i in c(100,500,1500,3000)){rings <- rbind(rings,esi_centre_terra%>%buffer(i*1000)%>%st_as_sf())}

# large_lim <- large_lim%>%
#              st_as_sfc()%>%
#               st_transform(CanProj)%>%
#               st_bbox()
# 
# ggplot()+
#   geom_sf(data=bioregion,fill=NA)+
#   geom_sf(data=maritimes_network%>%filter(name != "Eastern Shore Islands"),fill="grey",alpha=0.1)+
#   geom_sf(data=maritimes_network%>%filter(name == "Eastern Shore Islands"),fill="coral2")+
#   geom_sf(data=land%>%st_transform(CanProj))+
#   geom_sf(data=basemap_atlantic)+
#   geom_sf(data=qdet_df)+
#   coord_sf(expand=0,xlim=large_lim[c(1,3)],ylim=large_lim[c(2,4)])+
#   theme_bw()

#looks better in this projection 
p1 <- ggplot()+
  geom_sf(data=bioregion%>%st_transform(latlong),fill=NA)+
  geom_sf(data=rings,fill=NA,lty=2)+
  geom_sf(data=maritimes_network%>%filter(name != "Eastern Shore Islands")%>%st_transform(latlong),fill="cornflowerblue",alpha=0.2)+
  geom_sf(data=maritimes_network%>%filter(name == "Eastern Shore Islands")%>%st_transform(latlong),fill="coral2")+
  geom_sf(data=closures%>%st_transform(latlong),fill=NA)+
  geom_sf(data=noaa_closures%>%filter(AreaMar>100)%>%st_transform(latlong),fill=NA)+
  geom_sf(data=basemap_atlantic%>%st_transform(latlong))+
  geom_sf(data=land%>%st_transform(latlong))+
  geom_sf(data=qdet_df%>%st_transform(latlong),aes(fill=Species),pch=21,size=3.5)+
  coord_sf(expand=0,xlim=large_lim[c(1,3)],ylim=large_lim[c(2,4)])+
  theme_bw()+
  scale_fill_viridis(discrete=T)+
  scale_y_continuous(
    position = "right",  # Move y-axis to the right side
    sec.axis = sec_axis(~., name = "")
  )+
  annotation_scale(location="br")+
  annotation_north_arrow(location = "tl")+
  theme(legend.position = "none",
        axis.title = element_blank())

#smaller scale just the NW Atlantic 

med_lim <- large_lim

med_lim[1] <- -72
med_lim[2] <- 39.5
# large_lim[3] <- -54
# large_lim[4] <- 50

p2 <- ggplot()+
      geom_sf(data=bioregion%>%st_transform(latlong),fill=NA)+
      geom_sf(data=maritimes_network%>%filter(name != "Eastern Shore Islands")%>%st_transform(latlong),fill="cornflowerblue",,alpha=0.1)+
      geom_sf(data=maritimes_network%>%filter(name == "Eastern Shore Islands")%>%st_transform(latlong),fill="coral2")+
      geom_sf(data=basemap_atlantic%>%st_transform(latlong))+
      geom_sf(data=land%>%st_transform(latlong))+
      geom_sf(data=qdet_df%>%st_transform(latlong),aes(fill=Species),pch=21,size=3)+
      coord_sf(expand=0,xlim=med_lim[c(1,3)],ylim=med_lim[c(2,4)])+
      theme_bw()+
      scale_fill_viridis(discrete=T)+
      annotation_scale(location="br")+
      theme(legend.position = "none",
            axis.text = element_blank(),  
            axis.ticks.y = element_line(),
            axis.title = element_blank())

p2_legend <- ggplot()+
             geom_sf(data=qdet_df%>%st_transform(latlong),aes(fill=Species),pch=21,size=3)+
             theme_bw()+
             labs(fill="")+
             scale_fill_viridis(discrete=T)

ggsave("output/ESI_2025_CSAS/qdetections_large.png",p1,height=7.5,width=6,units="in",dpi=600)
ggsave("output/ESI_2025_CSAS/qdetections_med.png",p2,height=5,width=5,units="in",dpi=600)
ggsave("output/ESI_2025_CSAS/qdetections_legend.png",p2_legend,height=5,width=5,units="in",dpi=600)


## load salmon detections
salmon_qdet <- read.csv("data/Acoustic/esisalmon_match_sum_both_nozero.csv")%>%
               st_as_sf(coords=c("mean_lon","mean_lat"),crs=latlong,remove=FALSE)%>%
               st_transform(CanProj)

salmon_lims <- salmon_qdet%>%
  st_transform(utmkm)%>%
  st_buffer(200)%>%
  st_transform(CanProj)%>%
  st_bbox()

#download bathymetry for a contour line
lims_bbox <- bioregion%>%
             st_transform(utmkm)%>%
             st_buffer(1000)%>%
             st_transform(4326)%>%
             st_bbox()
          
  
#bathymetry
 # bathy <- rast("R:/Science/CESD/HES_MPAGroup/Data/Bathymetry/GEBCO/gebco_2019_Canada.tif")
 # 
 # lims_bbox <- bioregion%>%
 #   st_transform(utmkm)%>%
 #   st_buffer(1000)%>%
 #   st_transform(st_crs(bathy))%>%
 #   st_bbox()
 # 
 # ext <- ext(lims_bbox[1], lims_bbox[3], lims_bbox[2], lims_bbox[4])
 # 
 # bathy_crop <- crop(bathy, ext)
 # 
 # bathy_contour <- as.contour(bathy_crop, levels = -250) %>%
 #   st_as_sf()
 # write_sf(bathy_contour,dsn = "data/Shapefiles/contour_250.shp")
 # rm(bathy,bathy_crop)

bathy_sf <- read_sf("data/Shapefiles/contour_250.shp")


esi_lims <- reciever_locations%>%
            st_transform(utmkm)%>%
            st_buffer(5)%>%
            st_transform(CanProj)%>%
            st_bbox()

#zoomed in scale
p3 <- ggplot()+ #ESI zoom out 
  geom_sf(data=coast_hr)+
  geom_sf(data= west_river_polygon,fill="white")+
  geom_sf(data=esi_poly,fill=NA)+
  geom_sf(data=reciever_locations%>%filter(!grepl("SR",station),!grepl("MR",station)),size=0.2)+
  geom_sf(data=reciever_locations%>%filter(grepl("SR",station)|grepl("MR",station)),size=1)+
  geom_sf(data=salmon_qdet,shape=21,size=2,fill="salmon")+ #note that the location's don't exactly match the recievers. 
  theme_bw()+
  coord_sf(xlim=esi_lims[c(1,3)],ylim=esi_lims[c(2,4)],expand=0)

#board scale
p4 <- ggplot()+
      geom_sf(data=basemap_atlantic)+
      geom_sf(data=bathy_sf,linewidth=0.25,col="grey") +
      geom_sf(data=closures,fill="grey95",col="black",linewidth=0.1)+
      geom_sf(data=otn_stations,size=0.05,pch=20,col="grey45")+
      geom_sf(data=salmon_qdet,pch=21,size=3,fill="salmon")+
      theme_bw()+
      coord_sf(expand=0,xlim=salmon_lims[c(1,3)],ylim=salmon_lims[c(2,4)])+
      annotation_scale(location="br")

ggsave("output/ESI_2025_CSAS/salmon_detections_outside_array.png",p4,height=6,width=6,units="in",dpi=600)
      
  






