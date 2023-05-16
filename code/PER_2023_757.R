## Script for processing 2023 eDNA samples

#load libraries
library(dplyr)
library(sf)
library(ggplot2)
library(lubridate)

#source functions
source("code/smithroot_process.R")

#projections
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load shapefiles


#get files names for the smithroot samples to get associated water volumes for each sample. 
sr_files <- dir("data/eDNA/PER-2023-757/SmithRoot/",full.names = TRUE)

#get the summary extractions
PER_2023_757_SR <- do.call("rbind", lapply(sr_files, function(x) smithroot_process(x,longform = FALSE)))%>%
                   mutate(month=month(date),
                          year=year(date))%>%
                   filter(month == 4, year==2023)%>% #may 2023
                   st_as_sf(coords=c("longitude","latitude"),crs=latlong)

