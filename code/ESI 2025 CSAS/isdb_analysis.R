## ISDB analysis

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

#load isdb data
get_data('isdb', data.dir = "c:/Users/stanleyr/Documents/Github/WesternEmerald_CSAS_2025/data/isdb")
ISTRIPTYPECODES <- ISTRIPTYPECODES%>%filter(TRIP_TYPE == "4VSW SENTINEL PROGRAM")
self_filter()

ISSETPROFILE_WIDE2 <- ISSETPROFILE_WIDE %>% dplyr::select(FISHSET_ID, SET_NO, LAT1, LONG1, YEAR, DEP1, DATE_TIME1)
ISFISHSETS2 <- ISFISHSETS %>% dplyr::select(FISHSET_ID, TRIP_ID, HAULCCD_ID, SETCD_ID, GEAR_ID, STATION, STRATUM_ID, NAFAREA_ID, LEN_LONGLINE, NUM_HOOK_HAUL)
ISCATCHES2 <- ISCATCHES %>% dplyr::select(FISHSET_ID, CATCH_ID, SPECCD_ID, EST_NUM_CAUGHT, EST_COMBINED_WT, EST_KEPT_WT, EST_DISCARD_WT, EST_REDUCTION_WT, SPECSCD_ID)
ISTRIPS2 <- ISTRIPS %>% dplyr::select(TRIP_ID, TRIPCD_ID, OBSCD_ID, COMMENTS)

temp_merge <- merge(ISSETPROFILE_WIDE2, ISFISHSETS2, by = "FISHSET_ID")
temp_merge2 <- merge(x = temp_merge, y = ISCATCHES2, by = "FISHSET_ID", all = F)
temp_merge2 <- merge(temp_merge2, ISGEARS[, c("GEAR_ID", "GEARCD_ID", "HOOKCD_ID", "HOOKSIZE")], by = "GEAR_ID")
temp_merge2 <- merge(temp_merge2, ISTRIPS[, c("TRIP_ID", "TRIP", "TRIPCD_ID", "OBSCD_ID")])
temp_merge2 <- merge(temp_merge2, ISSPECIESCODES[, c("SPECCD_ID", "COMMON", "SCIENTIFIC")], by = "SPECCD_ID", all.x = T)
temp_merge2$QUARTER_YEAR <- quarter(x = temp_merge2$DATE_TIME1)

bycat_dat <- temp_merge2 %>% dplyr::select(FISHSET_ID, YEAR, DATE_TIME1, QUARTER_YEAR, TRIP_ID, TRIP,
                                           OBSCD_ID, TRIPCD_ID, SET_NO, LAT1, LONG1, DEP1, STATION, STRATUM_ID,
                                           NAFAREA_ID, SETCD_ID, HAULCCD_ID, GEARCD_ID, LEN_LONGLINE, NUM_HOOK_HAUL,
                                           HOOKSIZE, HOOKCD_ID, CATCH_ID, SPECCD_ID, COMMON, EST_COMBINED_WT, EST_NUM_CAUGHT,
                                           EST_KEPT_WT, EST_DISCARD_WT, EST_REDUCTION_WT, SCIENTIFIC)

save(bycat_dat,file="data/ISDB/bycat_dat.RData")

## isdb df

isdb_sample_df <- bycat_dat%>%
                  filter(!is.na(LONG1) & LONG1!=0 & LAT1!=0)%>%
                  mutate(id = paste(TRIP_ID,SET_NO,sep="-"))%>%
                  distinct(id,.keep_all=TRUE)%>%
                  st_as_sf(coords = c("LONG1","LAT1"),crs=latlong)%>%
                  st_transform(CanProj)%>%
                  st_join(.,maritimes_network)

sample_year_esi <- isdb_sample_df%>%
                   data.frame()%>%
                   filter(name == "Eastern Shore Islands")%>%
                   group_by(YEAR)%>%
                   summarise(count=n())%>%
                   ungroup()

species_isdb <-  bycat_dat%>%
                filter(!is.na(LONG1) & LONG1!=0 & LAT1!=0)%>%
                mutate(id = paste(TRIP_ID,SET_NO,sep="-"))%>%
                st_as_sf(coords = c("LONG1","LAT1"),crs=latlong)%>%
                st_transform(CanProj)%>%
                st_join(.,maritimes_network)%>%
                data.frame()%>%
                filter(name == "Eastern Shore Islands")%>%
                mutate(species = gsub("F.","",SCIENTIFIC,fixed=T),
                       species = gsub("S.","",species,fixed=T),
                       species = gsub("SP.","",species,fixed=T),
                       species = gsub("S.C.","",species,fixed=T),
                       species = gsub("C.","",species,fixed=T),
                       species = gsub("\\.","",species,fixed=T),
                       species = trimws(species),
                       species = tolower(species))%>%
                arrange(species)%>%
                distinct(SCIENTIFIC,.keep_all=TRUE)%>%
                dplyr::select(SCIENTIFIC,species)

#extract classification
PhyloNames <- c("Kingdom","Phylum","Class","Order","Family","Genus","Species")

trad_species <- species_isdb$species

tax_list_trad <- data.frame()

for(i in 1:length(trad_species)){
  
  sp <- trad_species[i]
  
  message(paste0("Working on ",sp," ",i," of ",length(trad_species)))
  
  temp <- classification(sp,db="worms") #run classification
  
  temp2 <- temp[[1]]%>% #unpack classification
    data.frame()
  
  if(nrow(temp2[1])>1){
    
    temp2 <- temp2%>%
      select(rank,name)%>%
      spread(rank,name)%>%
      mutate(aphiaID = temp[[1]]%>%data.frame()%>%slice(n())%>%pull(id))
    
    temp3 <- temp2%>% #trim classification
      select(all_of(c(names(temp2)[names(temp2)%in%PhyloNames],"aphiaID")))
    
    #if data is missing or a certain taxonomic level isn't identified.
    missing_cols <- setdiff(c(PhyloNames,"aphiaID"),names(temp3))
    
    if(length(missing_cols)>0){
      
      temp3[,missing_cols] <- NA
      
      temp3 <- temp3[,c(PhyloNames,"aphiaID")]
      
    }
  }else{temp3 <- data.frame(matrix(NA, nrow = 1, ncol = length(PhyloNames)))
  names(temp3) = PhyloNames
  temp3$aphiaID = NA}
  
  temp3$species_filter <- sp # this is for linking back in the original code
  
  tax_list_trad <- rbind(tax_list_trad,temp3)
  
}                     


isdb_formatted <- tax_list_trad%>%
                  rename(species = species_filter)%>%
                  left_join(species_isdb)%>%
                  dplyr::select(-species)%>%
                  dplyr::select(all_of(PhyloNames),aphiaID,SCIENTIFIC)%>%
                  arrange(Phylum,Class,Order)

save(isdb_formatted,file="data/isdb/isdb_taxonomy.RData")

common_sp <- bycat_dat%>%
            filter(!is.na(LONG1) & LONG1!=0 & LAT1!=0)%>%
            mutate(id = paste(TRIP_ID,SET_NO,sep="-"))%>%
            st_as_sf(coords = c("LONG1","LAT1"),crs=latlong)%>%
            st_transform(CanProj)%>%
            st_join(.,maritimes_network)%>%
            data.frame()%>%
            filter(name == "Eastern Shore Islands")%>%
            mutate(id = paste(TRIP_ID,SET_NO,sep="-"))%>%
            group_by(SCIENTIFIC,YEAR)%>%
            summarise(total_catch = sum(EST_NUM_CAUGHT,na.rm=T),
                      cpue_catch = total_catch/length(unique(id)),
                      total_weight = sum(EST_COMBINED_WT,na.rm=T),
                      cpue_weight = total_weight/length(unique(id)))%>%
            ungroup()

common_sp_year <- common_sp%>%
                  filter(YEAR>2004)%>% #drop happened
                  group_by(SCIENTIFIC)%>%
                  summarise(mean_catch = mean(total_catch), #average among years
                            mean_cpue_catch = mean(cpue_catch),
                            mean_weight = mean(total_weight),
                            mean_cpue_weight = mean(cpue_weight))%>%
                   ungroup()%>%
                   left_join(.,tax_list_trad%>%
                               rename(species = species_filter)%>%
                               left_join(species_isdb)%>%
                               rename(species_filter=species)%>%
                               dplyr::select(all_of(PhyloNames),aphiaID,SCIENTIFIC,species_filter))%>%
                   left_join(.,bycat_dat%>%
                               distinct(SCIENTIFIC,.keep_all=TRUE)%>%
                               dplyr::select(SCIENTIFIC,COMMON))%>%
                   arrange(-mean_cpue_catch)

common_sp_year%>%dplyr::select(species_filter,COMMON,mean_cpue_catch)%>%data.frame()
common_sp_year%>%arrange(-mean_cpue_weight)%>%dplyr::select(species_filter,COMMON,mean_cpue_weight)%>%data.frame()

sample_year_esi%>%filter(YEAR>2004)%>%summarise(mean=mean(count),sd=sd(count))
                                
ggplot(sample_year_esi,aes(x=YEAR,y=count))+
  geom_line()+
  geom_point(size=2,pch=21,fill="white")+
  theme_bw()
