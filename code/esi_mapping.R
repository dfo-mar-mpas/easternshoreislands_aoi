## eDNA survey map

#load libraries ----
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(rnaturalearthdata)
library(ggplot2)
library(raster)
library(stars)
library(geosphere)
library(ggspatial)
library(viridis)

s2_as_sf = FALSE

#projections ------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load bathymetry ------ ## note this is too large to be tracked by github 


#load stations
esi_acoustic <- read.csv("data/Acoustic/2023_Acoustic_stations.csv")%>%
                dplyr::select(latitude,longitude,name,type)%>%
                st_as_sf(coords=c("longitude","latitude"),crs=latlong,remove=FALSE)%>%
                st_transform(proj4string(bathy))%>%
                mutate(depth=raster::extract(bathy,as_Spatial(.)),
                       type="Acoustic")%>%
                st_transform(latlong)

esi_eDNA<- read.csv("data/SamplingDesign_65by20km.csv")%>%
            st_as_sf(coords=c("lon","lat"),crs=latlong)%>%
            mutate(id=c(16,12,8,4,15,11,7,3,14,10,6,2,13,9,5,1))%>%#reorder because they were done along shore not perpendicular
            arrange(id)%>%
            mutate(name=paste0("ESI_",1:16),
                   transect = rep(paste0("Transect_",1:4),each=4),
                   type="eDNA")

stations <- rbind(esi_acoustic%>%dplyr::select(name,type,geometry),
                  esi_eDNA%>%dplyr::select(name,type,geometry))

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

#create bounded bathymetry and coastlines 
    # coast_hr <- read_sf("r:/Science/CESD/HES_MPAGroup/Data/Shapefiles/Coastline/NS_coastline_project_Erase1.shp")
    # 
    # coast_hr_esi <- coast_hr%>%
    #                 st_intersection(bounding_area%>%st_transform(st_crs(coast_hr)))%>%
    #                 st_transform(latlong)%>%
    #                 suppressWarnings()
    # 
    # st_write(coast_hr_esi,"data/shapefiles/coast_hr_esi.shp")

    # bathy <- raster("r:/Science/CESD/HES_MPAGroup/Data/Bathymetry/dem35c_5c.tif")
    # 
    # bathy_crop <- bathy%>%
    #               crop(.,as_Spatial(bounding_area%>%st_transform(proj4string(bathy))))%>%
    #               projectRaster(.,crs=latlong)
    # 
    # writeRaster(bathy_crop,filename="data/esi_bath.tif",overwrite=TRUE)

esi_coast <- st_read("data/Shapefiles/coast_hr_esi.shp")%>%st_transform(latlong)
esi_bathy <- raster("data/esi_bath.tif")
values(esi_bathy) <- values(esi_bathy)*-1
names(esi_bathy) <- "Depth"

#generate plot with ESI locations for readme
esi_map <- ggplot()+
          geom_sf(data=esi_poly,fill=NA)+
          geom_stars(data=esi_bathy%>%st_as_stars())+
          scale_fill_viridis(option="C",na.value="white")+
          geom_sf(data=esi_poly,fill=NA,col="black")+
          geom_sf(data=esi_coast,fill="darkolivegreen3")+
          geom_sf(data=stations%>%filter(type=="Acoustic"),aes(shape=type,size=type),fill="red")+
          geom_sf(data=stations%>%filter(type=="eDNA"),aes(shape=type,size=type),fill="cornflowerblue")+
          scale_shape_manual(values=c(21,23))+
          scale_size_manual(values=c(1.25,2))+
          theme_bw()+
          coord_sf(expand=0,xlim=aoi_plot_lims[c(1,3)],ylim=aoi_plot_lims[c(2,4)])+
          theme(axis.text=element_blank())+
          labs(x="",y="",col="",shape="",size="",fill="Depth (m)")+
          annotation_scale(location="br")+
          annotation_north_arrow(location="tl")

ggsave("output/esi_map.png",esi_map,width=7,height=5,units="in",dpi=300)
