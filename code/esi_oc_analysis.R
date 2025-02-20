## Blue Carbon View

library(sf)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthhires)
library(MarConsNetData)
library(ggspatial)
library(terra)
library(tidyterra)
library(rasterVis)
library(basemaps)
library(viridis)
library(scales)
library(patchwork)

#Load projections
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utmkm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

source("code/ESI 2025 CSAS/random_raster_gen_functions.R")

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

coast_hr <- read_sf("data/Shapefiles/NS_coastline_project_Erase1.shp")%>%st_transform(CanProj)


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
                    st_buffer(10)%>% #50km buffer
                    st_transform(CanProj)%>%
                    st_bbox()

#organic carbon raster - data resides in Western Emerald
oc_rast <- rast("c:/Users/stanleyr/Documents/Github/WesternEmerald_CSAS_2025/data/OCDEN_Maritimes.tif")%>%
           project(CanProj)

oc_rast_mask <- mask(oc_rast,vect(bioregion%>%st_transform(st_crs(oc_rast))))

if (!st_crs(maritimes_network) == crs(oc_rast)) {
  maritimes_network <- st_transform(maritimes_network, crs(oc_rast))
}


#Create basemap intersected with the bounding box. 
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

#make map

rast_min <- min(values(oc_rast_mask), na.rm=TRUE)
rast_max <- max(values(oc_rast_mask), na.rm=TRUE)

p1_small <- ggplot()+
  geom_spatraster(data=oc_rast_mask)+
  geom_sf(data=maritimes_network%>%filter(name!="Eastern Shore Islands"),fill=NA,linewidth=0.5)+
  geom_sf(data=maritimes_network%>%filter(name=="Eastern Shore Islands"),fill=NA,col="grey75",linewidth=0.5)+
  geom_sf(data=coast_hr)+
  #geom_sf(data=bc_areas,fill="red")+
  theme_bw()+
  coord_sf(expand=0,xlim=plotlims_small[c(1,3)],ylim=plotlims_small[c(2,4)])+
  scale_fill_viridis_c(
    na.value = "transparent",
    n.breaks = 10,  # Creates 10 color breaks
    breaks = scales::breaks_extended(n = 4)(values(oc_rast_mask)),  # Shows 4 labels
    labels = function(x) round(x, 1)  # Rounds labels to 1 decimal
  )+
  annotation_scale()+
  labs(fill=expression("OC Density (kg/m"^3*")"))+
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.98, 0.03),  # Position (0,0 is bottom left, 1,1 is top right)
    legend.justification = c(1, 0),    # Anchor point of legend box
    legend.text = element_text(size = rel(0.7)),  # Reduce text size by 30%
    legend.title = element_text(size = rel(0.7)),# Also reduce title size to match
    legend.background = element_blank(),
    legend.title.align = 0.5
  )

ggsave("output/ESI_2025_CSAS/esi_OC.png",p1_small,width=6,height=6,units="in",dpi=300)

p1_small_box <- plotlims_small%>%st_as_sfc()

p1_large <- ggplot()+
  geom_sf(data=bioregion,fill=NA)+
  geom_sf(data=basemap_atlantic)+
  geom_spatraster(data=oc_rast_mask)+
  geom_sf(data=maritimes_network,fill=NA,linewidth=0.5)+
  geom_sf(data=maritimes_network%>%filter(name=="Eastern Shore Islands"),linewidth=1.2,fill=NA,col="grey75")+
  geom_sf(data=p1_small_box,fill=NA,lty=2,linewidth=0.5)+
  theme_bw()+
  coord_sf(expand=0,xlim=plotlims[c(1,3)],ylim=plotlims[c(2,4)])+
  scale_fill_viridis_c(
    na.value = "transparent",
    n.breaks = 10,  # Creates 10 color breaks
    breaks = scales::breaks_extended(n = 4)(values(oc_rast_mask)),  # Shows 4 labels
    labels = function(x) round(x, 1)  # Rounds labels to 1 decimal
  )+
  annotation_scale()+
  labs(fill=expression("OC Density (kg/m"^3*")"))+
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.98, 0.03),  # Position (0,0 is bottom left, 1,1 is top right)
    legend.justification = c(1, 0),    # Anchor point of legend box
    legend.text = element_text(size = rel(0.7)),  # Reduce text size by 30%
    legend.title = element_text(size = rel(0.7)),# Also reduce title size to match
    legend.background = element_blank(),
    legend.title.align = 0.5
  )



#now combine the plots 

p1_small_format <- p1+theme(
  legend.position = "none",
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  plot.margin = margin(0, 0, 0, 0, "pt"),
  plot.background = element_blank()
)

#without dem
combo_plot <- p1_large + inset_element(p1_small_format, left = 0.405, bottom = -0.015, right = 0.97, top = 0.49)

combo_plot <- (p1_large+theme(legend.position="none")) + p1_small + plot_layout(ncol=2)

ggsave("output/ESI_2025_CSAS/oc_combo.png",combo_plot,height=6,width=10,units="in",dpi=600)

#coverage analysis
masked_oc <- mask(oc_rast_mask,vect(maritimes_network%>%st_transform(st_crs(oc_rast_mask))))

total_oc_conservation <- sum(values(masked_oc),na.rm=T)
total_oc_region <- sum(values(oc_rast_mask),na.rm=T)

total_oc_conservation / total_oc_region


maritimes_oc <- NULL

for (i in 1:nrow(maritimes_network)) {
  
  message("Working on ",maritimes_network[i,]%>%pull(name)," site ",i," of ",nrow(maritimes_network))
  
  # Extract individual site as a subset
  site <- maritimes_network[i, ]
  
  # Mask raster with the individual site
  site_masked_raster <- mask(oc_rast, vect(site))
  
  # Calculate total OC density for the site
  total_oc_site <- sum(values(site_masked_raster),na.rm = TRUE)
  
  # Calculate proportion of OC density for this site
  proportion_site <- total_oc_site / total_oc_region
  
  # Store results
  
  out <- data.frame(site = maritimes_network[i,]%>%pull(name),
                    total_oc = total_oc_site,
                    prop_oc = proportion_site)
  
  maritimes_oc <- rbind(maritimes_oc,out)
  
  
}

plot_oc <- maritimes_oc%>%
           arrange(prop_oc)%>%
           mutate(name_oc = factor(site,levels=site))

p2 <- ggplot(plot_oc%>%filter(prop_oc>0.005),aes(x=prop_oc,y=name_oc,fill=prop_oc))+
      geom_bar(stat="identity",col="black")+
      geom_bar(data=plot_oc%>%filter(site=="Eastern Shore Islands"),stat="identity",col="black",linewidth=1.5)+
      geom_text(aes(label = scales::percent(prop_oc, accuracy = 0.1)), 
                hjust = 1.1,  # Position text slightly outside the bar
                size = 3.5) +
          theme_bw()+
          scale_x_continuous(labels = percent, expand = c(0, 0.001))+ 
          #scale_fill_viridis_b(labels = percent,n.breaks=5,option="D")+
      scale_fill_distiller(palette = "Blues", direction = 1,labels=percent) +  # Blue gradient
  
      labs(y="",x="Proportional coverage of total regional OC",fill="")+
      theme(legend.position = "inside",
            legend.position.inside = c(0.9,0.2),
            legend.background = element_blank());p2

ggsave("output/ESI_2025_CSAS/OC_analysis.png",p2,width=6,height=6,units="in",dpi=300)



  
