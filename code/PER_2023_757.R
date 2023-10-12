## Script for processing 2023 eDNA samples

#load libraries
library(dplyr)
library(sf)
library(ggplot2)
library(lubridate)
library(devtools)

#source functions
source_url("https://raw.githubusercontent.com/rystanley/mct_r/main/code/smithroot_process.R")

#projections
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load shapefiles 
esi_poly <- read_sf("data/Shapefiles/EasternShoreIslands_networksite.shp")%>%
            st_transform(latlong)

            aoi_plot_lims <- esi_poly%>%
                              st_bbox()%>%
                              st_as_sfc()%>%
                              st_as_sf()%>%
                              st_transform(utm)%>%
                              st_buffer(5)%>%
                              st_transform(latlong)%>%
                              st_bbox()

esi_coast <- st_read("data/Shapefiles/coast_hr_esi.shp")%>%st_transform(latlong)

esi_bathy <- raster("data/esi_bath.tif")
  values(esi_bathy) <- values(esi_bathy)*-1
  names(esi_bathy) <- "Depth"

#get files names for the smithroot samples to get associated water volumes for each sample. 
sr_files <- dir("data/eDNA/PER-2023-757/SmithRoot/",full.names = TRUE)

#get the summary extractions
PER_2023_757_SR <- do.call("rbind", lapply(sr_files, function(x) smithroot_process(x,longform = FALSE)))%>%
                   mutate(month=month(date),
                          year=year(date),
                          volume=as.numeric(volume))%>%
                   filter(month == 4, year==2023,volume>0)%>% #may 2023
                   st_as_sf(coords=c("longitude","latitude"),crs=latlong)%>%
                   arrange(date)%>%
                   mutate(time_step = difftime(date,lag(date,1)),
                          id=1:n())
                   
#now key out breaks in the time series based on the 3 biggest steps
step_inds <- PER_2023_757_SR%>%
             data.frame()%>%
             arrange(-time_step)%>%
             dplyr::select(file,id,time_step)%>%
             slice(1:3)%>%
             arrange(file)

PER_2023_757_metadata <- read.csv("data/eDNA/PER-2023-757/ESI_Sampling_2023.csv")%>%
                         dplyr::select(-notes)

station_ord <- unique(PER_2023_757_metadata$station) #this will keep the order sampled. 

#now assign the stations based on the breaks in samplng when boat was steaming. 
PER_2023_757_SR <- PER_2023_757_SR%>%mutate(station = case_when(id %in% 1:(step_inds[1,"id"]-1) ~ station_ord[1],
                                                                id %in% step_inds[1,"id"]:(step_inds[2,"id"]-1) ~ station_ord[2],
                                                                id %in% step_inds[2,"id"]:(step_inds[3,"id"]-1) ~ station_ord[3],
                                                                TRUE ~ station_ord[4]))

##matching - this is done manually because the datasheet volumes are likely inaccurate but 
#assume it goes Bottom 1-3 then surface 4-6
#this was added back to the metadata sheet as 'file ids' as best matched. Note there were sr_match_notes to describe the logic. 
PER_2023_757_SR%>%data.frame()%>%filter(station=="ESI_08")%>%dplyr::select(file,volume)
PER_2023_757_SR%>%data.frame()%>%filter(station=="ESI_07")%>%dplyr::select(file,volume)
PER_2023_757_SR%>%data.frame()%>%filter(station=="ESI_06")%>%dplyr::select(file,volume)
PER_2023_757_SR%>%data.frame()%>%filter(station=="ESI_05")%>%dplyr::select(file,volume)


#Data manipulations
PER_2023_757_SR_fixed <- PER_2023_757_SR

#add the first two together -- equalling 3 litres ESI_08 Bottom, replicate 1
PER_2023_757_SR_fixed[PER_2023_757_SR$file == "data/eDNA/PER-2023-757/SmithRoot/00007.csv","volume"] = 
  PER_2023_757_SR%>%filter(file %in% c("data/eDNA/PER-2023-757/SmithRoot/00007.csv","data/eDNA/PER-2023-757/SmithRoot/00010.csv"))%>%pull(volume)%>%sum()

#add 00039 and 00038 to equal 3.1 litres matching closer the volume reported. 
PER_2023_757_SR_fixed[PER_2023_757_SR$file == "data/eDNA/PER-2023-757/SmithRoot/00038.csv","volume"] = 
  PER_2023_757_SR%>%filter(file %in% c("data/eDNA/PER-2023-757/SmithRoot/00038.csv","data/eDNA/PER-2023-757/SmithRoot/00039.csv"))%>%pull(volume)%>%sum()



ggplot()+
  geom_sf(data=esi_poly)+
  geom_sf(data=esi_coast)+
  geom_sf(data=PER_2023_757_SR)

ggplot()+
  geom_point(data=PER_2023_757_SR,aes(x=date,y=1,fill=station),pch=21,size=2)+
  geom_vline(data=PER_2023_757_SR%>%filter(id %in% step_inds$id),aes(xintercept = date))+
  theme_bw()
  

  

