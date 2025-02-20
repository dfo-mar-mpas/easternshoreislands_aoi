#Make a map of eDNA sites in the ESI including our Perley work,and Kira's kelp sites
library(sf)
library(raster)
library(ggplot2)
library(dplyr)

latlong <- "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
utm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"


# MEQ Contaminants Sites --------------------------------------------------
meq <- read.csv("~/GitHub/easternshoreislands_aoi/data/MEQ_Eastern Shore Contaminants Project Sampling Sites.csv", header = T) %>% glimpse()

colnames(meq) <- c("Site","Latitude","Longitude","Sample_Type")


novsco <-read_sf("../Courtney-Trask-Honours/data/Shapefiles/NS_coastline_project_Erase1.shp") %>% 
  st_transform(latlong) %>%
  mutate(name="Nova Scotia")%>%
  dplyr::select(name, geometry)

esi <-read_sf("~/GitHub/Courtney-Trask-Honours/data/Shapefiles/EasternShoreIslands_networksite.shp")%>%
  st_transform(latlong)%>%
  mutate(name="ESI")%>%
  dplyr::select(name,geometry)


focal_bound <- esi%>%
  st_bbox()%>%
  st_as_sfc()%>%
  st_transform(utm)%>%
  st_buffer(0)%>% 
  st_transform(latlong)%>%
  st_bbox()

ggplot()+
  geom_sf(data=novsco, fill="grey")+
  geom_sf(data=esi, fill=NA, linewidth=0.75, colour="black")+
  coord_sf(expand=0, xlim=focal_bound[c(1,3)], ylim=c(44.64, 44.97730))+
  xlab(label = "Longitude")+
  ylab(label="Latitude")+
  geom_point(data=meq, aes(x=Longitude, y=Latitude,  fill=factor(Sample_Type)),shape=21,size=4)+
  scale_fill_manual(values=c("#E69F00","#009E73"))+
  theme_bw()+
  theme(axis.text.y = element_text(colour = "black", size = 14, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 14),
        text=element_text(size=20))+
  guides(fill=guide_legend(title="Sample Type"))

ggsave(filename = "MEQ_ESIContamination_Sites.png",plot = last_plot(), width = 12, height=8, dpi=300, path = "figures/2024CSAS/")
