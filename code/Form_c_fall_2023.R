## Fall sampling mission in the Eastern Shore Islands Area of Interest (AOI)

#load libraries ----
  library(dplyr)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthhires)
  library(rnaturalearthdata)
  library(ggplot2)
  library(raster)
  library(patchwork)

  s2_as_sf = FALSE

#projections ------
  latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  utm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load bathymetry ------
  dem_esi <- raster("data/Bathymetry/esi_dem.tif") # ESI based on the Greenlaw 35 de
  
#load the Eastern Shore Islands Area of Interest
  esi <- read_sf("data/Shapefiles/EasternShoreIslands_networksite.shp")%>%
         st_transform(latlong)
  
  bs_bounding <- esi%>% #makes things faster
                  st_bbox()%>%
                  st_as_sfc()%>%
                  st_as_sf()%>%
                  st_transform(utm)%>%
                  st_buffer(75)%>% # 75 km boundary
                  st_bbox()%>%
                  st_as_sfc()%>%
                  st_as_sf()%>%
                  st_transform(latlong)
  
  plot_lims <- esi%>%
                st_bbox()%>%
                st_as_sfc()%>%
                st_as_sf()%>%
                st_transform(utm)%>%
                st_buffer(5)%>%
                st_transform(latlong)%>%
                st_bbox()
              
#load coastline and make basemap ------
    coast_hr <- read_sf("data/shapefiles/NS_coastline_project_Erase1.shp")%>%
                st_transform(latlong)%>%
                st_intersection(.,bs_bounding)
    
#load edna Stations
    edna_esi <- read.csv("data/SamplingDesign_65by20km.csv")%>%
                mutate(lon = lon *-1,
                       deg_lon = floor(lon),
                       deg_lat = floor(lat),
                       dm_lon = (lon-deg_lon)*60,
                       dm_lat = (lat-deg_lat)*60,
                       deg_lon = deg_lon*-1,
                       activity="eDNA",
                       location="ESI",
                       id=c(16,12,8,4,15,11,7,3,14,10,6,2,13,9,5,1))%>%#reorder because they were done along shore not perpendicular
                arrange(id)%>%
                mutate(station=paste0("ESI_",1:16),
              transect = rep(paste0("Transect_",1:4),each=4),
              Zone=" ")
    
    esi_sf <- edna_esi%>%
              mutate(lon=lon*-1)%>%
              st_as_sf(coords=c("lon","lat"),crs=latlong)%>%
              rename(name=station)%>%
              dplyr::select(name,geometry)%>%
              mutate(group = case_when(name %in% paste0("ESI_",1:8) ~ "Day 1",
                               name %in% paste0("ESI_",9:16) ~ "Day 2"),
                     priority = case_when(name %in% paste0("ESI_",5:8) ~ "Primary",
                                  TRUE ~ "Secondary"))
#map waypoints
    
    waypoints <- data.frame(name=c("WP1","WP2","WP3"),
                            lon=c(-62.483136,-62.497729,-62.475491),
                            lat=c(44.891778,44.835977,44.803352))%>%
      st_as_sf(coords=c("lon","lat"),crs=latlong)
    
    sheet_harbour_port <- data.frame(lon=-62.504091,lat=44.903582,name="Port of Sheet Harbour")%>%
      st_as_sf(coords=c("lon","lat"),crs=latlong)
    
    waypoints_esi <- rbind(sheet_harbour_port,waypoints)
    
    ## plot it out
    esi_plot <- ggplot()+
                geom_sf(data=coast_hr,col="black")+
                geom_sf(data=esi,fill=NA)+
                geom_sf(data=esi_sf,pch=21,col="black",fill="white",size=3)+
                geom_sf(data=esi_sf%>%filter(priority == "Primary"),pch=21,col="black",fill="red",size=3)+
                geom_sf(data=sheet_harbour_port)+
                geom_sf_label(data=sheet_harbour_port,aes(label=name),nudge_x = 0.02,nudge_y = 0.06)+
                coord_sf(expand=0,xlim=plot_lims[c(1,3)],ylim=plot_lims[c(2,4)])+
                theme_bw()+
                labs(fill="",x="",y="")+
                theme(legend.position = c(0.9,0.1),
                      legend.title = element_blank())+
                annotation_scale()
    
    ggsave("output/per_2023_fall_esi.png",esi_plot,width=7,height=5,units="in",dpi=300)
    