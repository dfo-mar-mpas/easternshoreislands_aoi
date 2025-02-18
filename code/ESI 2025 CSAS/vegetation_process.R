### Eastern Shore Islands Vegetation data prep ---

#load libraries
library(tidyverse)
library(sf)
library(terra)
library(MarConsNetData)

#Load projections
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utmkm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

source("code/raster_extraction.R")

#Basemaps
bioregion <- data_planning_areas()%>%
  st_transform(CanProj)%>%
  st_make_valid()

#maritimes network -- note that this cannot be shared or made available online to the public
maritimes_network <- data_draft_areas()%>%
  st_transform(CanProj)%>%
  st_make_valid()%>%
  dplyr::select(Classification_E,SiteName_E)%>%
  rename(status=Classification_E,name=SiteName_E)%>%
  st_make_valid()

#coastal sites in the network (names)
coastal_veg_sites <- read.csv("data/coastal_vegetation_site_list.csv")

##Eelgrass -----------

#ingest eelgrass polygons
eg_path <- "E:/Vegetation/National_Eelgrass_Dataset_Canada_NETForce_Scotian_Shelf.gdb/"

layers <- st_layers(eg_path) #view layers

#Presence/Absence
eelgrass_data_pa <- st_read(eg_path, 
                            layer = "DFO_Maritimes_Eelgrass_Ensemble_Species_Distribution_Model_Binary_PA_2010_2021")

#projection
eelgrass_proj <- st_crs(eelgrass_data_pa) #faster to not transform this large shapefile

maritimes_network_eg <- maritimes_network%>%
                        filter(name %in% coastal_veg_sites$name)%>%
                        st_transform(eelgrass_proj)%>%
                        dplyr::select(name)

eelgrass_total_area <- eelgrass_data_pa%>%
                      filter(Eelgrass_Occurrence_PA == "Presence")%>%
                      st_union()%>%st_area()/1000/1000%>%as.numeric()

eelgrass_pa_intersection <- eelgrass_data_pa%>%
                            filter(Eelgrass_Occurrence_PA == "Presence")%>%
                            dplyr::select(Shape)%>%
                            st_intersection(maritimes_network_eg)%>%
                            mutate(area=as.numeric(st_area(.))/1000/1000,
                                   prop_area = round(area/sum(area),2),
                                   prop_region = as.numeric(round(area/eelgrass_total_area,2)),
                                   total_regional_extent = eelgrass_total_area)%>%
                            arrange(prop_region)%>%
                            mutate(name=factor(name,levels=name))%>%
                            st_transform(CanProj)

save(eelgrass_pa_intersection,file="data/Vegetation/eelgrass_nework_ensemble_intersect.RData")

rm(eeelgrass_data_pa,eelgrass_proj,maritimes_network_eg,eelgrass_total_area,eelgrass_pa_intersection)#clean up workspace


#Balbar/Metaxas kelp -----------------------

kelp_balbar <- read_sf("E:/Vegetation/Kelp_SDM.shp")%>%
               st_transform(CanProj)

kelp_total_area <- kelp_balbar%>%
                    st_union()%>%
                    st_as_sf()%>%
                    st_union()%>%
                    st_area()/1000/1000%>%
                    as.numeric()

kelp_pa_intersection <- kelp_balbar%>%
                        st_union()%>%
                        st_as_sf()%>%
                        st_intersection(maritimes_network%>% filter(name %in% coastal_veg_sites$name))%>%
                        mutate(area=as.numeric(st_area(.))/1000/1000,
                               prop_area = round(area/sum(area),2),
                               prop_region = as.numeric(round(area/kelp_total_area,2)),
                               total_regional_extent = kelp_total_area)%>%
                        arrange(prop_region)%>%
                        mutate(name=factor(name,levels=name))

save(kelp_pa_intersection,file="data/Vegetation/mb_kelp_sdm_intersect.RData")

rm(kelp_balbar,kelp_pa_intersection,kelp_total_area)#clean up workspace


#Rockweed -----------------

rockweed_df <- read_sf("E:/Vegetation/MARboundary_rockweed_presence_validated_2021-04-07_NDVIstats_substrate.shp")

rockweed_total_area <- rockweed_df%>%
  st_union()%>%
  st_as_sf()%>%
  st_union()%>%
  st_area()/1000/1000%>%
  as.numeric()

rockweed_pa_intersection <- rockweed_df%>% #takes a while
                            st_union()%>%
                            st_as_sf()%>%
                            st_intersection(maritimes_network%>% filter(name %in% coastal_veg_sites$name)%>%st_transform(st_crs(rockweed_df)))%>%
                            mutate(area=as.numeric(st_area(.))/1000/1000,
                                   prop_area = round(area/sum(area),2),
                                   prop_region = as.numeric(round(area/rockweed_total_area,2)),
                                   total_regional_extent = rockweed_total_area)%>%
                            arrange(prop_region)%>%
                            mutate(name=factor(name,levels=name))%>%
                            st_transform(CanProj)

save(rockweed_pa_intersection,file="data/Vegetation/rockweed_intersect.RData")

rm(rockweed_df,rockweed_total_area,rockweed_pa_intersection) #clean up workspace


#Krumhansl sdms ------

#data downloaded from one drive and placed on external - https://086gc-my.sharepoint.com/:f:/g/personal/kira_krumhansl_dfo-mpo_gc_ca/EtQ-kmY5X6pPuDttnyxM_j4Bk5etLA3-DMhTJkGiAgLr5Q?email=Ryan.Stanley%40dfo-mpo.gc.ca&e=Jdttq8

sdm_rasters <- paste0("E:/Vegetation/MCT_SDM_AIS_GROUP/",
                      c("Antithamnion sparsum/Antithamnion_sparsum_Bathy_rm2_20240304_avg_Binary.tif",
                        "Bonnemaisonia hamifera/Bonnemaisonia_hamifera_Bathy_rm2MinusYRMin_20240304_avg_Binary.tif",
                        "Codium fragile/Codium_fragile_Bathy_rm3_20240304_avg_Binary.tif",
                        "Dasysiphonia japonica/Dasysiphonia_japonica_Bathy_rm3MinusYRMax_20240304_avg_Binary.tif",
                        "Fucus serratus/Fucus_serratus_Bathy_rm3_20240227_avg_Binary.tif",
                        "Laminaria digitata/Laminaria_digitata_Bathy_rm4_20240223_avg_Binary.tif",
                        "Saccharina latissima/Saccharina_latissima_Bathy_rm2MinusRugosityAndProfile_20240223_avg_Binary.tif",
                        "Combined_Kelp/LaminariaSaccharina_Bathy_Addition_20240226.tif"))


sdm_list <- list()

for(i in sdm_rasters){
  
  sp <- strsplit(i, "/")[[1]][4]
  
  sp <- ifelse(sp == "Combined_Kelp","Kelp",sp)
  
  message(paste0("Working on ",sp," ",which(i == sdm_rasters)," of ",length(sdm_rasters)))
  
  sdm_list[[sp]] <- process_raster_intersection(i,
                                                bioregion = bioregion, 
                                                maritimes_network = maritimes_network%>%filter(name %in% coastal_veg_sites$name),
                                                species=sp)
}

save(sdm_list,file="data/Vegetation/sdm_raster_extract_list.RData")

##OUTDATED DATA -----
# kelp_path <- "data/Vegetation/kelp_v2.gdb/"
# 
# layers <- st_layers(kelp_path) #view layers
# 
# #Saccharina 
# saccharina_df <- st_read(kelp_path, 
#                             layer = "Saccharina_304")%>%
#                  filter(gridcode == 1)%>%
#                  st_make_valid()%>%
#                  st_union()%>%
#                  st_as_sf()%>%
#                  st_transform(CanProj)
# 
# saccharina_total_area <- saccharina_df%>%
#                           st_area()/1000/1000%>%
#                           as.numeric()
# 
# saccharina_pa_intersection <- saccharina_df%>% 
#                               st_intersection(maritimes_network%>% filter(name %in% coastal_veg_sites$name))%>%
#                               mutate(area=as.numeric(st_area(.))/1000/1000,
#                                      prop_area = round(area/sum(area),2),
#                                      prop_region = as.numeric(round(area/saccharina_total_area ,2)),
#                                      total_regional_extent = saccharina_total_area )%>%
#                               arrange(prop_region)%>%
#                               mutate(name=factor(name,levels=name))
# 
# save(saccharina_pa_intersection,file="data/Vegetation/saccharina_intersect.RData")
# 
# rm(saccharina_df,saccharina_pa_intersection,saccharina_total_area) #clean workspace
# 
# #Laminaria
# laminaria_df <- st_read(kelp_path, 
#                          layer = "Laminaria_467")%>%
#                 filter(gridcode == 1)%>%
#                 st_make_valid()%>%
#                 st_union()%>%
#                 st_as_sf()%>%
#                 st_transform(CanProj)
# 
# laminaria_total_area <- saccharina_df%>%
#                         st_area()/1000/1000%>%
#                         as.numeric()
# 
# laminaria_pa_intersection <- laminaria_df%>% 
#                               st_intersection(maritimes_network%>% filter(name %in% coastal_veg_sites$name))%>%
#                               mutate(area=as.numeric(st_area(.))/1000/1000,
#                                      prop_area = round(area/sum(area),2),
#                                      prop_region = as.numeric(round(area/laminaria_total_area ,2)),
#                                      total_regional_extent = laminaria_total_area )%>%
#                               arrange(prop_region)%>%
#                               mutate(name=factor(name,levels=name))
# 
# save(laminaria_pa_intersection,file="data/Vegetation/laminaria_intersect.RData")
# 
# rm(laminaria_df,laminaria_pa_intersection,laminaria_total_area,kelp_path,layers) #clean workspace
                

