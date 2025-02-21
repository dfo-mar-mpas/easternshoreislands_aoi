## biodiversity comparison analsysis

library(sf)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthhires)
library(lubridate)
library(MarConsNetData)
library(vegan)
library(ggnewscale)
library(robis)
library(worrms)
library(taxize)

source("code/venn_bar.R")

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

#load data -----

PhyloNames <- c("Kingdom","Phylum","Class","Order","Family","Genus","Species")

#obis (from obis_pull.R)
load("data/robis_esi.RData")

obis_data <- obis_filtered%>%
             data.frame()

obis_species <- obis_data%>%
                distinct(aphiaID,.keep_all=TRUE)%>%
                dplyr::select(all_of(tolower(PhyloNames)),aphiaID)%>%
                rename_all(~tools::toTitleCase(.))%>%
                mutate(method="obis")

#load isdb data 
load("data/ISDB/isdb_taxonomy.RData")

#load the eDNA and acoustic species list 

load("data/Acoustic/acoustic_taxonomy.RData") #code to create this at the end. 
load("data/eDNA/edna_taxonomy.RData")

#load the siening data

load("data/Seining/seining_taxonomy.RData")

total_esi_taxonomy <- rbind(obis_species%>%dplyr::select(all_of(c(PhyloNames,"aphiaID","method"))),
                            acoustic_formatted%>%dplyr::select(all_of(c(PhyloNames,"aphiaID","method"))),
                            edna_formatted%>%dplyr::select(all_of(c(PhyloNames,"aphiaID","method"))),
                            isdb_formatted%>%mutate(method="isdb")%>%dplyr::select(all_of(c(PhyloNames,"aphiaID","method"))),
                            seining_formatted%>%dplyr::select(all_of(c(PhyloNames,"aphiaID","method")))
                            )%>%
                      mutate(meth=method,
                             method=ifelse(method!="eDNA","other","edna"),
                             Phylum = case_when(Class == "Florideophyceae"~"Rhodophyta", #plants have (Phylum(Division)) so the code above doesn't keep the right bits
                                                Class == "Mamiellophyceae"~"Chlorophyta",
                                                TRUE ~ Phylum))

total_esi_taxonomy%>%distinct(aphiaID,.keep_all=TRUE)%>%pull(aphiaID)%>%length()
total_esi_taxonomy%>%distinct(aphiaID,.keep_all=TRUE)%>%filter(!is.na(Species))%>%pull(aphiaID)%>%length()

#code borrowed from the WEBMR code. 
bar_direct <- total_esi_taxonomy%>%
              filter(!is.na(Species))%>% #just keep the species level 
              group_by(aphiaID)%>%
              mutate(Status = case_when(
                sum(method == "other") == 1 & sum(method == "edna") == 0 ~ "Traditional Only",
                sum(method == "other") == 0 & sum(method == "edna") == 1 ~ "eDNA Only",
                TRUE ~ "Shared"
              ))%>%
              ungroup()%>%
              distinct(aphiaID,.keep_all=TRUE)%>%
              group_by(Phylum,Status)%>%
              summarise(Count=n())%>%
              ungroup()%>%
              mutate(Status = factor(Status,levels=c("Shared","Traditional Only","eDNA Only")))%>%
              data.frame()


bar_plot_direct <- venn_bar(bar_direct)

ggsave("output/ESI_2025_CSAS/eDNA_vs_obis_other.png",bar_plot_direct,width=6,height=6,units="in",dpi=600)


bar_direct2 <- total_esi_taxonomy%>%
              filter(!is.na(Species), #just keep the species level 
                      meth!="obis")%>%
              group_by(aphiaID)%>%
              mutate(Status = case_when(
                sum(method == "other") == 1 & sum(method == "edna") == 0 ~ "Traditional Only",
                sum(method == "other") == 0 & sum(method == "edna") == 1 ~ "eDNA Only",
                TRUE ~ "Shared"
              ))%>%
              ungroup()%>%
              distinct(aphiaID,.keep_all=TRUE)%>%
              group_by(Phylum,Status)%>%
              summarise(Count=n())%>%
              ungroup()%>%
              mutate(Status = factor(Status,levels=c("Shared","Traditional Only","eDNA Only")))%>%
              data.frame()


bar_plot_direct2 <- venn_bar(bar_direct2)

ggsave("output/ESI_2025_CSAS/eDNA_vs_other.png",bar_plot_direct2,width=6,height=6,units="in",dpi=600)



### taxonomic processes
# edna_df <- read.csv("data/Species_method_inventories.csv")%>%
#                 filter(method=="eDNA")%>%
#                 mutate(type=case_when(location %in% c("Moosehead","Sheet Harbour Estuary","Taylor Head Provincial Park") ~ "Coastal",
#                                       TRUE ~ location))
# 
# edna_species <- edna_df%>%
#                 filter(type!="Offshore")%>%
#                 mutate(type="Coastal")%>%
#                 rbind(.,
#                       edna_df%>%
#                         filter(type!="Coastal")%>%
#                         mutate(type="Offshore"))%>%
#                filter(!latin %in% c("Ameiurus nebulosus", #Brown Bullhead - freshwater
#                                     "Fundulus hetroclitus", #Mummichog - estuarine
#                                     "Euchlanis dilatata" #rotifer - non-marine
#                                     ))%>%
#                mutate(latin = case_when(latin == "Callithamnion corybosum" ~ "Callithamnion corymbosum", #spelling error
#                                         latin == "Boonea bisaturalis" ~ "Boonea bisuturalis",#spelling error
#                                         latin == "Ilyanassa trivttata" ~ "Ilyanassa trivittata", #spelling error
#                                         latin == "Juxtacribilina mutabilis" ~ "Juxtacribrilina mutabilis", #spelling error
#                                         latin == "Ophiopholis aculeatus" ~ "Ophiopholis aculeata", #Spelling error
#                                         latin == "Delphinus delphinus" ~ "Delphinus delphis", #spelling error
#                                         TRUE ~ latin))
# 
# acoustic_species <- read.csv("data/Species_method_inventories.csv")%>%
#                     filter(method=="Acoustic")
# 
# 
# #do the taxonomy
# trad_species <- unique(edna_species$latin,acoustic_species$latin) 
# 
# tax_list_trad <- data.frame()
# 
# for(i in 1:length(trad_species)){
#   
#   sp <- trad_species[i]
#   
#   message(paste0("Working on ",sp," ",i," of ",length(trad_species)))
#   
#   temp <- classification(sp,db="worms") #run classification
#   
#   temp2 <- temp[[1]]%>% #unpack classification
#     data.frame()
#   
#   if(nrow(temp2[1])>1){
#     
#     temp2 <- temp2%>%
#       select(rank,name)%>%
#       spread(rank,name)%>%
#       mutate(aphiaID = temp[[1]]%>%data.frame()%>%slice(n())%>%pull(id))
#     
#     temp3 <- temp2%>% #trim classification
#       select(all_of(c(names(temp2)[names(temp2)%in%PhyloNames],"aphiaID")))
#     
#     #if data is missing or a certain taxonomic level isn't identified.
#     missing_cols <- setdiff(c(PhyloNames,"aphiaID"),names(temp3))
#     
#     if(length(missing_cols)>0){
#       
#       temp3[,missing_cols] <- NA
#       
#       temp3 <- temp3[,c(PhyloNames,"aphiaID")]
#       
#     }
#   }else{temp3 <- data.frame(matrix(NA, nrow = 1, ncol = length(PhyloNames)))
#   names(temp3) = PhyloNames
#   temp3$aphiaID = NA}
#   
#   temp3$species_filter <- sp # this is for linking back in the original code
#   
#   tax_list_trad <- rbind(tax_list_trad,temp3)
#   
# }      
# 
# save(tax_list_trad,file="data/edna_acoustic_taxonomy_worms.RData")
# 
# edna_formatted <- edna_species%>%
#                   left_join(.,tax_list_trad%>%
#                               distinct(aphiaID,.keep_all=TRUE)%>%
#                               rename(latin=species_filter))
# 
# save(edna_formatted ,file="data/eDNA/edna_taxonomy.RData")
# 
# acoustic_formatted <- acoustic_species%>%
#                       left_join(.,tax_list_trad%>%
#                                   distinct(aphiaID,.keep_all=TRUE)%>%
#                                   rename(latin=species_filter))
# 
# save(acoustic_formatted,file="data/Acoustic/acoustic_taxonomy.RData")

#load siening data

# data_format <- read.csv("data/Seining/Seining_FishMeasurements_2023.csv")%>%
#                 dplyr::select(SpeciesName,Species)%>%
#                 distinct(Species,.keep_all=TRUE)%>%
#                 mutate(year=2023)%>%
#                 rbind(.,
#                 read.csv("data/Seining/Seining_FishMeasurements_2024.csv")%>%
#                 dplyr::select(SpeciesName,Species)%>%
#                 distinct(Species,.keep_all=TRUE)%>%
#                 mutate(year=2024))
# 
# write.csv(data_format,file="data/Seining/data_clean.csv",row.names=FALSE)

# seining_df <- read.csv("data/Seining/species_list_seining.csv") #pulled together from the different morpho csvs. 
# 
# trad_species <- unique(seining_df$latin)
# 
# tax_list_trad <- data.frame()
# 
# for(i in 1:length(trad_species)){
# 
#   sp <- trad_species[i]
# 
#   message(paste0("Working on ",sp," ",i," of ",length(trad_species)))
# 
#   temp <- classification(sp,db="worms") #run classification
# 
#   temp2 <- temp[[1]]%>% #unpack classification
#     data.frame()
# 
#   if(nrow(temp2[1])>1){
# 
#     temp2 <- temp2%>%
#       select(rank,name)%>%
#       spread(rank,name)%>%
#       mutate(aphiaID = temp[[1]]%>%data.frame()%>%slice(n())%>%pull(id))
# 
#     temp3 <- temp2%>% #trim classification
#       select(all_of(c(names(temp2)[names(temp2)%in%PhyloNames],"aphiaID")))
# 
#     #if data is missing or a certain taxonomic level isn't identified.
#     missing_cols <- setdiff(c(PhyloNames,"aphiaID"),names(temp3))
# 
#     if(length(missing_cols)>0){
# 
#       temp3[,missing_cols] <- NA
# 
#       temp3 <- temp3[,c(PhyloNames,"aphiaID")]
# 
#     }
#   }else{temp3 <- data.frame(matrix(NA, nrow = 1, ncol = length(PhyloNames)))
#   names(temp3) = PhyloNames
#   temp3$aphiaID = NA}
# 
#   temp3$species_filter <- sp # this is for linking back in the original code
# 
#   tax_list_trad <- rbind(tax_list_trad,temp3)
# 
# }
# 
# seining_formatted <- tax_list_trad%>%
#                      mutate(method="seining")
# 
# save(seining_formatted,file="data/Seining/seining_taxonomy.RData")

