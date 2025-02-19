## eDNA comparison - ESI

#load libraries ----------
library(sf)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthhires)
library(lubridate)
library(MarConsNetData)
library(vegan)
library(ggnewscale)
library(robis)

#Load projections
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utmkm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#source taxonmic processing scripts
source("code/venn_bar.R")
source("code/species_pool_estimate.R")

#load edna species lists
edna_df <- read.csv("data/eDNA/2023Seining_vs_eDNA_FishList.csv")

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

#load the esi polygon
maritimes_network <- data_draft_areas()%>%
  st_transform(CanProj)%>%
  st_make_valid()%>%
  dplyr::select(Classification_E,SiteName_E)%>%
  rename(status=Classification_E,name=SiteName_E)%>%
  st_make_valid()%>%
  st_intersection(bioregion) #clean up the edges

esi_poly <- maritimes_network%>%
  filter(name == "Eastern Shore Islands")


##download observations from OBIS

polygon <- esi_poly%>%
           st_transform(utmkm)%>%
           st_buffer(10)%>% # 10 km buffer -- this will grab RV sets
           st_transform(crs = 4326)%>% # Convert to WGS84 (EPSG:4326) if needed, as OBIS uses this CRS
           st_bbox()%>%
           st_as_sfc()%>%
           st_geometry()%>%
           st_as_text()


# Download OBIS records within the bounding box
obis_data <- occurrence(geometry = polygon)

save(obis_data,file="data/robis_esi_10km_buffer.RData")  

# Optionally, filter the data to retain only points strictly within the polygon
obis_sf <- st_as_sf(obis_data, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
obis_filtered <- obis_sf[st_within(obis_sf, polygon, sparse = FALSE), ]

save(obis_filtered,file="data/robis_esi.RData")
