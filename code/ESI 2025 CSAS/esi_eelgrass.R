# Eastern Shore Island Eelgrass Modelling

## load libraries
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
library(units)
library(ggforce)

source("code/ESI 2025 CSAS/random_raster_gen_functions.R")

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

#coastal sites in the network (names)
coastal_veg_sites <- read.csv("data/coastal_vegetation_site_list.csv")

coastal_veg_sites$abbreviation <- sapply(strsplit(coastal_veg_sites$name, " "), function(x) {
  paste(toupper(substr(x, 1, 1)), collapse = "")
})

coastal_veg_sites <- coastal_veg_sites%>%
                     mutate(abbreviation = case_when(abbreviation == "P" ~ "PEM",
                                                     abbreviation == "C" ~ "CHE",
                                                     abbreviation == "I" ~ "ING",
                                                     abbreviation == "SM(RAE" ~ "SMR",
                                                     name == "Cobequid Bay" ~ "CoB",
                                                     name == "Bird Islands" ~ "BrI",
                                                     name == "MacNamaras Islands" ~ "MacI",
                                                     TRUE ~ abbreviation))

#plotlimits
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

coastal_veg_table <- maritimes_network%>%
                     filter(name %in% coastal_veg_sites$name)%>%
                     mutate(area=round(as.numeric(st_area(.))/1000/1000,2))%>%
                     left_join(.,coastal_veg_sites)%>%
                     data.frame()%>%
                     dplyr::select(name,abbreviation,area)%>%
                     arrange(-area)

write.csv(coastal_veg_table,"output/ESI_2025_CSAS/coastal_site_table.csv",row.names=FALSE)

#load vegetation extractions from 'vegetation_process.R'

load("data/Vegetation/eelgrass_nework_ensemble_intersect.RData")
load("data/Vegetation/rockweed_intersect.RData")
load("data/Vegetation/mb_kelp_sdm_intersect.RData")
load("data/Vegetation/sdm_raster_extract_list.RData")

sdm_df <- do.call("rbind",sdm_list)


df_names <- c("sp","name","area","prop_area","prop_region","total_regional_extent")


## AIS vegetative species 

nkelp_df <- sdm_df %>%
  data.frame() %>%
  filter(sp %in% c("Antithamnion sparsum","Bonnemaisonia hamifera",
                   "Codium fragile","Dasysiphonia japonica","Fucus serratus")) %>%
  dplyr::select(df_names) %>%
  # rbind(.,rockweed_pa_intersection %>%
  #         data.frame() %>%
  #         mutate(sp="Rockweed") %>%
  #         dplyr::select(df_names)) %>%
  left_join(.,coastal_veg_sites) %>%
  # Group by species and get top 5 by prop_region
  group_by(sp) %>%
  slice_max(order_by = prop_region, n = 5, with_ties = FALSE) %>%
  ungroup()

#add in the composite AIS layer
ais_rast <- rast("e:/Vegetation/AIS indices/AIS_Index_Bathy_Continuous_20240306.tif")

rast_proj <- terra::crs(ais_rast)

raster_trim <- maritimes_network%>%
  filter(name=="Eastern Shore Islands")%>%
  st_transform(utmkm)%>%
  st_buffer(10)%>% #2km buffer
  st_transform(rast_proj)%>%
  st_bbox()%>%
  st_as_sfc()%>%
  terra::vect()

esi_ais_rast <- ais_rast%>%
  terra::crop(.,raster_trim)%>%
  terra::project(CanProj)

# ggplot()+
#   geom_spatraster(data=esi_ais_rast)+
#   geom_sf(data=maritimes_network%>%filter(name == "Eastern Shore Islands"),fill=NA)+
#   geom_sf(data=coast_hr)+
#   coord_sf(expand=0,xlim=plotlims_small[c(1,3)],ylim=plotlims_small[c(2,4)])+
#   theme_bw()+
#   annotation_scale(location="tl")+
#   scale_fill_viridis(na.value = "transparent")

# Create a modified dataframe with proper ordering within facets
nkelp_df_ordered <- nkelp_df %>%
  group_by(sp) %>%
  arrange(prop_region, .by_group = TRUE) %>%
  mutate(
    # Create a unique ordering factor for each species-abbreviation combination
    abbrev_ordered = factor(interaction(sp, abbreviation, drop = TRUE), 
                            levels = interaction(sp, abbreviation, drop = TRUE))
  ) %>%
  ungroup()

# Create summary dataframe for total extent
total_extent_df <- nkelp_df_ordered %>% 
  group_by(sp) %>% 
  summarise(total_regional_extent = first(total_regional_extent)) %>%
  ungroup()

# Create the plot using the modified ordering
nkelp_plot <- ggplot(nkelp_df_ordered) +
  geom_bar(aes(x = prop_region, 
               y = abbrev_ordered,
               fill = prop_region),
           stat = "identity", 
           col = "black") +
  geom_bar(data=nkelp_df_ordered%>%filter(name=="Eastern Shore Islands"),
           aes(x = prop_region, 
               y = abbrev_ordered),
           stat = "identity", 
           col = "black",
           lwd=1.25) +
  geom_text(data=nkelp_df_ordered%>%filter(abbreviation == "ESI"),aes(x = prop_region - 0.005, 
                y = abbrev_ordered,
                label = paste0(round(area, 1), " km²")),
            hjust = 1, 
            vjust = 1.2, 
            color = "black", 
            fontface = "bold",
            size = 3) +  # Reduced text size for area labels
  geom_text(data = total_extent_df,
            aes(x = 0.95 * max(nkelp_df_ordered$prop_region),
                y = 1,
                label = sprintf("Total Network extent:\n%.1f km²", total_regional_extent)),
            hjust = 1,
            size = 3) +
  facet_wrap(~sp, scales = "free_y") +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_gradient(high = "darkgreen", low = "lightgreen") +
  scale_y_discrete(labels = function(x) sub("^.*\\.", "", x)) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold")
  ) +
  labs(x = "% bioregional coverage bioregion",
       y = "")

ggsave("output/ESI_2025_CSAS/ais_bars.png",nkelp_plot,width=10,height=6,units="in",dpi=100)


nkelp_map_df <- sdm_df %>%
  filter(name=="Eastern Shore Islands",sp %in% c("Antithamnion sparsum","Bonnemaisonia hamifera",
                   "Codium fragile","Dasysiphonia japonica","Fucus serratus"))


  ais_map_plot <- ggplot()+
                  geom_sf(data=maritimes_network%>%
                            filter(name=="Eastern Shore Islands")%>%
                            st_transform(CanProj),fill=NA)+
                  geom_sf(data=nkelp_map_df,fill="forestgreen",col=NA)+
                  geom_sf(data=coast_hr%>%st_transform(CanProj))+
                  theme_bw()+
                  facet_wrap(~sp,ncol=3)+
                  geom_text(data = nkelp_map_df,  # Ensure area is associated correctly
                              aes(label = paste0("Total extent: ", round(area,1), " km", "\u00B2")),
                              x = Inf, y = -Inf,  # Positioning in bottom-right
                              hjust = 1.1, vjust = -0.5,  # Adjust text alignment
                              inherit.aes = FALSE)+
                  coord_sf(expand=0,xlim=plotlims_small[c(1,3)],ylim = plotlims_small[c(2,4)])+
                  annotation_scale(location="tl")+
                  theme(strip.background = element_rect(fill="white"))
  
  ggsave("output/ESI_2025_CSAS/AIS_map_plot.png",ais_map_plot,height=6,width=10,units="in",dpi=600)
 

### Eelgrass bar -------

eelgrass_pa_intersection%>%pull(area)%>%sum()/unique((eelgrass_pa_intersection$total_regional_extent)) #total coverage in the network

eelgrass_plot_df <- data.frame(eelgrass_pa_intersection)%>%
                    filter(prop_area>0.02)%>%
                    left_join(.,coastal_veg_sites)%>%
                    arrange(prop_region)%>%
                    mutate(name=factor(name,levels=name),
                           abbreviation = factor(abbreviation,levels=abbreviation))




eelgrass_bar <- ggplot(data=eelgrass_plot_df)+
                geom_bar(aes(x=prop_region,y=abbreviation,fill=prop_region),stat="identity",col="black")+
                geom_bar(data=eelgrass_plot_df%>%filter(name=="Eastern Shore Islands"),aes(x=prop_region,y=abbreviation),fill=NA,stat="identity",col="black",lwd=2)+
                geom_text(aes(x = prop_region - 0.005, y = abbreviation, label = paste0(round(area,1), " km²")), 
                           hjust = 1, vjust = 1.2, color = "black", fontface = "bold",size=2)+
                geom_text(data =eelgrass_plot_df,
                          aes(x = 0.95 * max(prop_region),
                              y = 1,
                              label = sprintf("Total Network extent:\n%.1f km²", total_regional_extent)),
                          hjust = 1,
                          size = 4)+
                theme_bw()+
                scale_x_continuous(labels = scales::percent)+
                scale_fill_gradient(high = "darkgreen", low = "lightgreen")+
                theme(legend.position="none")+
                labs(x="% coverage suitable habitat",y="")


eelgrass_map <- ggplot()+
                geom_sf(data=coast_hr%>%st_transform(CanProj))+
                geom_sf(data=maritimes_network%>%
                             filter(name=="Eastern Shore Islands")%>%
                             st_transform(CanProj),fill=NA)+
                geom_sf(data=eelgrass_pa_intersection%>%filter(name=="Eastern Shore Islands")%>%st_transform(CanProj),fill="forestgreen",col=NA)+
                theme_bw()+
                coord_sf(expand=0,xlim=plotlims_small[c(1,3)],ylim = plotlims_small[c(2,4)])+
                annotation_scale(location="tl")

# Combine with the map, scaling the inset proportionally
combo_eelgrass <- eelgrass_map + eelgrass_bar + plot_layout(ncol=2)

ggsave("output/ESI_2025_CSAS/eelgrass_map_combo.png",combo_eelgrass,height=6,width=10,units="in",dpi=600)

## kelp (combo SDM from KK group)
kelp_plot_df <- sdm_df%>%
           filter(sp =="Kelp")%>%
           data.frame()%>%
           left_join(.,coastal_veg_sites)%>%
           arrange(prop_region)%>%
           mutate(name=factor(name,levels=name),
                  abbreviation = factor(abbreviation,levels=abbreviation))%>%
           arrange(-prop_region)%>%
           filter(prop_area>0.04)

kelp_bar <- ggplot(data=kelp_plot_df)+
  geom_bar(aes(x=prop_region,y=abbreviation,fill=prop_region),stat="identity",col="black")+
  geom_bar(data=kelp_plot_df%>%filter(name=="Eastern Shore Islands"),aes(x=prop_region,y=abbreviation),fill=NA,stat="identity",col="black",lwd=2)+
  geom_text(aes(x = prop_region - 0.005, y = abbreviation, label = paste0(round(area,1), " km²")), 
            hjust = 1, vjust = 1.2, color = "black", fontface = "bold",size=3)+
  geom_text(data = kelp_plot_df,
            aes(x = 0.95 * max(prop_region),
                y = 1,
                label = sprintf("Total Network extent:\n%.1f km²", total_regional_extent)),
            hjust = 1,
            size = 4)+
  theme_bw()+
  scale_x_continuous(labels = scales::percent)+
  scale_fill_gradient(high = "darkgreen", low = "lightgreen")+
  theme(legend.position="none")+
  labs(x="% coverage suitable habitat",y="")


kelp_map <- ggplot()+
            geom_sf(data=coast_hr%>%st_transform(CanProj))+
            geom_sf(data=maritimes_network%>%
                      filter(name=="Eastern Shore Islands")%>%
                      st_transform(CanProj),fill=NA)+
            geom_sf(data=sdm_df%>%
                         filter(sp =="Kelp",name=="Eastern Shore Islands")%>%st_transform(CanProj),fill="forestgreen",col=NA)+
            theme_bw()+
            coord_sf(expand=0,xlim=plotlims_small[c(1,3)],ylim = plotlims_small[c(2,4)])+
            annotation_scale(location="tl")

combo_kelp <- kelp_map + kelp_bar + plot_layout(ncol=2)

ggsave("output/ESI_2025_CSAS/kelp_map_combo.png",combo_kelp,height=6,width=10,units="in",dpi=600)


##Laminaria digitata
LD_plot_df <- sdm_df%>%
  filter(sp=="Laminaria digitata")%>%
  data.frame()%>%
  left_join(.,coastal_veg_sites)%>%
  arrange(prop_region)%>%
  mutate(name=factor(name,levels=name),
         abbreviation = factor(abbreviation,levels=abbreviation))%>%
  arrange(-prop_region)%>%
  filter(prop_area>0.04)

LD_bar <- ggplot(data=LD_plot_df)+
  geom_bar(aes(x=prop_region,y=abbreviation,fill=prop_region),stat="identity",col="black")+
  geom_bar(data=LD_plot_df%>%filter(name=="Eastern Shore Islands"),aes(x=prop_region,y=abbreviation),fill=NA,stat="identity",col="black",lwd=2)+
  geom_text(aes(x = prop_region - 0.005, y = abbreviation, label = paste0(round(area,1), " km²")), 
            hjust = 1, vjust = 1.2, color = "black", fontface = "bold",size=3)+
  geom_text(data = LD_plot_df,
            aes(x = 0.95 * max(prop_region),
                y = 1,
                label = sprintf("Total Network extent:\n%.1f km²", total_regional_extent)),
            hjust = 1,
            size = 4)+
  theme_bw()+
  scale_x_continuous(labels = scales::percent)+
  scale_fill_gradient(high = "darkgreen", low = "lightgreen")+
  theme(legend.position="none")+
  labs(x="% coverage suitable habitat",y="")

LD_map <- ggplot()+
  geom_sf(data=maritimes_network%>%
            filter(name=="Eastern Shore Islands")%>%
            st_transform(CanProj),fill=NA)+
  geom_sf(data=sdm_df%>%
            filter(sp =="Laminaria digitata",name=="Eastern Shore Islands")%>%st_transform(CanProj),fill="forestgreen",col=NA)+
  geom_sf(data=coast_hr%>%st_transform(CanProj))+
  theme_bw()+
  coord_sf(expand=0,xlim=plotlims_small[c(1,3)],ylim = plotlims_small[c(2,4)])+
  annotation_scale(location="tl")

##Saccharina latissima
SL_plot_df <- sdm_df%>%
  filter(sp=="Saccharina latissima")%>%
  data.frame()%>%
  left_join(.,coastal_veg_sites)%>%
  arrange(prop_region)%>%
  mutate(name=factor(name,levels=name),
         abbreviation = factor(abbreviation,levels=abbreviation))%>%
  arrange(-prop_region)%>%
  filter(prop_area>0.04)

SL_bar <- ggplot(data=SL_plot_df)+
  geom_bar(aes(x=prop_region,y=abbreviation,fill=prop_region),stat="identity",col="black")+
  geom_bar(data=SL_plot_df%>%filter(name=="Eastern Shore Islands"),aes(x=prop_region,y=abbreviation),fill=NA,stat="identity",col="black",lwd=2)+
  geom_text(aes(x = prop_region - 0.005, y = abbreviation, label = paste0(round(area,1), " km²")), 
            hjust = 1, vjust = 1.2, color = "black", fontface = "bold",size=3)+
  geom_text(data = SL_plot_df,
            aes(x = 0.95 * max(prop_region),
                y = 1,
                label = sprintf("Total Network extent:\n%.1f km²", total_regional_extent)),
            hjust = 1,
            size = 4)+
  theme_bw()+
  scale_x_continuous(labels = scales::percent)+
  scale_fill_gradient(high = "darkgreen", low = "lightgreen")+
  theme(legend.position="none")+
  labs(x="% coverage suitable habitat",y="")


SL_map <- ggplot()+
  geom_sf(data=maritimes_network%>%
            filter(name=="Eastern Shore Islands")%>%
            st_transform(CanProj),fill=NA)+
  geom_sf(data=sdm_df%>%
            filter(sp =="Saccharina latissima",name=="Eastern Shore Islands")%>%st_transform(CanProj),fill="forestgreen",col=NA)+
  geom_sf(data=coast_hr%>%st_transform(CanProj))+
  theme_bw()+
  coord_sf(expand=0,xlim=plotlims_small[c(1,3)],ylim = plotlims_small[c(2,4)])+
  annotation_scale(location="tl")



combo_kelp_sp <- ((LD_map + LD_bar + plot_layout(ncol=2)) + labs(title="Laminaria digitata"))/((SL_map + SL_bar + plot_layout(ncol=2))+ labs(title="Saccharina latissima"))

ggsave("output/ESI_2025_CSAS/kelp_sp_map_combo.png",combo_kelp_sp,height=12,width=10,units="in",dpi=600)

#species plots

##Kelp (Balbar/Metaxas model) ---------

load("data/Vegetation/mb_kelp_sdm_intersect.RData")

mb_plot_df <- data.frame(kelp_pa_intersection)%>%
                dplyr::select(-x)%>%
                filter(prop_region>0.01)%>%
                left_join(.,coastal_veg_sites)%>%
                arrange(prop_region)%>%
                mutate(name=factor(name,levels=name),
                       abbreviation = factor(abbreviation,levels=abbreviation))%>%
                arrange(-prop_region)

mb_bar <- ggplot(data=mb_plot_df)+
          geom_bar(aes(x=prop_region,y=abbreviation,fill=prop_region),stat="identity",col="black")+
          geom_bar(data=mb_plot_df%>%filter(name=="Eastern Shore Islands"),aes(x=prop_region,y=abbreviation),fill=NA,stat="identity",col="black",lwd=2)+
          geom_text(aes(x = prop_region - 0.005, y = abbreviation, label = paste0(round(area,1), " km²")), 
                    hjust = 1, vjust = 1.2, color = "black", fontface = "bold",size=3)+
          geom_text(data = mb_plot_df,
                    aes(x = 0.95 * max(prop_region),
                        y = 1,
                        label = sprintf("Total Network extent:\n%.1f km²", total_regional_extent)),
                    hjust = 1,
                    size = 4)+
          theme_bw()+
          scale_x_continuous(labels = scales::percent)+
          scale_fill_gradient(high = "darkgreen", low = "lightgreen")+
          theme(legend.position="none")+
          labs(x="% coverage suitable habitat",y="")


mb_map <- ggplot()+
          geom_sf(data=coast_hr%>%st_transform(CanProj))+
          geom_sf(data=maritimes_network%>%
                    filter(name=="Eastern Shore Islands")%>%
                    st_transform(CanProj),fill=NA)+
          geom_sf(data=kelp_pa_intersection%>%
                    filter(name=="Eastern Shore Islands")%>%st_transform(CanProj),fill="forestgreen",col=NA)+
          theme_bw()+
          coord_sf(expand=0,xlim=plotlims_small[c(1,3)],ylim = plotlims_small[c(2,4)])+
          annotation_scale(location="tl")

combo_mb_kelp <- mb_map + mb_bar + plot_layout(ncol=2)

ggsave("output/ESI_2025_CSAS/kelp_map_combo_MetBal.png",combo_mb_kelp,height=6,width=10,units="in",dpi=600)

#ROckweed ------------------
load("data/Vegetation/rockweed_intersect.RData")


rockweed_plot_df <- data.frame(rockweed_pa_intersection)%>%
                    dplyr::select(-x)%>%
                    filter(prop_region>0.01)%>%
                    left_join(.,coastal_veg_sites)%>%
                    arrange(prop_region)%>%
                    mutate(name=factor(name,levels=name),
                           abbreviation = factor(abbreviation,levels=abbreviation))%>%
                    arrange(-prop_region)

rockweed_bar <- ggplot(data=rockweed_plot_df)+
                geom_bar(aes(x=prop_region,y=abbreviation,fill=prop_region),stat="identity",col="black")+
                geom_bar(data=rockweed_plot_df%>%filter(name=="Eastern Shore Islands"),aes(x=prop_region,y=abbreviation),fill=NA,stat="identity",col="black",lwd=2)+
                geom_text(aes(x = prop_region - 0.005, y = abbreviation, label = paste0(round(area,1), " km²")), 
                          hjust = 1, vjust = 1.2, color = "black", fontface = "bold",size=3)+
                geom_text(data = rockweed_plot_df,
                          aes(x = 0.95 * max(prop_region),
                              y = 1,
                              label = sprintf("Total Network extent:\n%.1f km²", total_regional_extent)),
                          hjust = 1,
                          size = 4)+
                theme_bw()+
                scale_x_continuous(labels = scales::percent)+
                scale_fill_gradient(high = "darkgreen", low = "lightgreen")+
                theme(legend.position="none")+
                labs(x="% coverage suitable habitat",y="")


rockweed_map <- ggplot()+
                geom_sf(data=coast_hr%>%st_transform(CanProj))+
                geom_sf(data=maritimes_network%>%
                          filter(name=="Eastern Shore Islands")%>%
                          st_transform(CanProj),fill=NA)+
                geom_sf(data=rockweed_pa_intersection%>%
                          filter(name=="Eastern Shore Islands")%>%st_transform(CanProj),fill="forestgreen",col=NA)+
                theme_bw()+
                coord_sf(expand=0,xlim=plotlims_small[c(1,3)],ylim = plotlims_small[c(2,4)])+
                annotation_scale(location="tl")

combo_rockweed <- rockweed_map + rockweed_bar + plot_layout(ncol=2)

ggsave("output/ESI_2025_CSAS/rockweed_map_combo.png",combo_rockweed,height=6,width=10,units="in",dpi=600)





ggplot()+
  geom_sf(data=coast_hr)+
  geom_sf(data=eelgrass_data_pa)
  geom_sf(data=maritimes_eg_network%>%filter(name !="Eastern Shore Islands"),fill=NA,lwd=0.5)+
  geom_sf(data=maritimes_eg_network%>%filter(name =="Eastern Shore Islands"),fill=NA,lwd=1)+
  


#set up data analysis
esi_boundary <- maritimes_network%>%
                filter(name=="Eastern Shore Islands")%>%
                st_transform(eelgrass_proj)

st_write(esi_boundary%>%dplyr::select(name),dsn="data/Shapefiles/esi_poly.shp")

coast_esi <- st_intersection(coast_hr%>%st_transform(eelgrass_proj), esi_boundary)
esi_analysis <- analyze_coastal_eelgrass(coast_esi, eelgrass_data_pa)

coastalbox <- data.frame(lon = c(-66.248810,-65.609340,-60.740732,-61.349226)
                         lat = c(43.820056,43.203809, 45.264270, 45.572595))

cb_coords <- matrix(c(
  -60.892767,  45.303558,
  -61.012789,  45.341341,
  -61.565648,  45.341884,
  -62.150630,  46.186854,
  -60.526314,  47.25402,
  -59.176279,  46.015770,
  -60.892767,  45.303558  # Closing the polygon
), ncol = 2, byrow = TRUE)

cb_box <- st_sfc(st_polygon(list(cb_coords)), crs = latlong) %>%
  st_sf()%>%
  st_transform(eelgrass_proj)

eg_test_range <- eelgrass_data_pa%>%st_difference(cb_box)

#Manually create boxes for the coast line of ~ 81 km (the northeast - southwest principle axis distance of the ESI)
east_box1 <- matrix(c(
  -61.953544,  44.903582,
  -62.085148,  45.057922,
  -61.110342,  45.334906,
  -61.015991,  45.226096,
  -61.953544,  44.903582),ncol = 2,byrow = TRUE)

eastbox1 <- st_sfc(st_polygon(list(east_box1)), crs = latlong) %>%
  st_sf()%>%
  mutate(name="East box 1")%>%
  st_transform(eelgrass_proj)%>%
  dplyr::select(name,geometry)

esi_box <- matrix(c(
          -63.077923,   44.796922,
          -63.053827,   44.551107,
          -61.953544,  44.903582,
          -62.085148,  45.057922,
          -63.077923,   44.796922
        ),ncol = 2,byrow = TRUE)

esibox <- st_sfc(st_polygon(list(esi_box)), crs = latlong) %>%
  st_sf()%>%
  mutate(name="ESI box")%>%
  st_transform(eelgrass_proj)%>%
  dplyr::select(name,geometry)
  

west_box1 <- matrix(c(
  -64.863668,   44.029506,
  -64.656783,   43.872106,
  -65.617287,  43.321044,
  -65.821620,  43.495718,
  -64.863668,   44.029506
  ),ncol = 2,byrow = TRUE)

westbox1 <- st_sfc(st_polygon(list(west_box1)), crs = latlong) %>%
  st_sf()%>%
  mutate(name="West box 1")%>%
  st_transform(eelgrass_proj)%>%
  dplyr::select(name,geometry)

west_box2 <- matrix(c(
  -64.863668,   44.029506,
  -64.656783,   43.872106,
  -63.87054,    44.353487,
  -64.276217,   44.690956,
  -64.863668,   44.029506
),ncol = 2,byrow = TRUE)

westbox2 <- st_sfc(st_polygon(list(west_box2)), crs = latlong) %>%
  st_sf()%>%
  mutate(name="West box 2")%>%
  st_transform(eelgrass_proj)%>%
  dplyr::select(name,geometry)

west_box3 <- matrix(c(
  -63.87054,    44.353487,
  -64.276217,   44.690956,
  -63.077923,   44.796922,
  -63.053827,   44.551107,
  -63.87054,    44.353487
),ncol = 2,byrow = TRUE)

westbox3 <- st_sfc(st_polygon(list(west_box3)), crs = latlong) %>%
  st_sf()%>%
  mutate(name="West box 3")%>%
  st_transform(eelgrass_proj)%>%
  dplyr::select(name,geometry)

boxes <- rbind(eastbox1,esibox,westbox3,westbox2,westbox1)

ggplot()+
  geom_sf(data=boxes,aes(fill=name),alpha=0.5)+
  theme_bw()

eelgrass_df <- eelgrass_data_pa%>%
              st_intersection(boxes[i])%>%
              mutate(area=as.numeric(st_area(.))/1000/1000)


  
  temp_df <- eelgrass_data_pa%>%
             dplyr::select(Eelgrass_Occurrence_PA)%>%
             st_intersection(boxes)%>%
             mutate(area=as.numeric(st_area(.))/1000/1000)%>%
             data.frame()%>%
             dplyr::select(-Shape)
  
  temp_df_ratio <- temp_df %>%
    group_by(name) %>%
    summarise(ratio = sum(area[Eelgrass_Occurrence_PA == "Presence"]) / 
                sum(area[Eelgrass_Occurrence_PA == "Absence"])) %>%
    ungroup()

  
  temp_df <- eelgrass_data_prop%>%
    dplyr::select(Level_Uncertain)%>%
    st_intersection(boxes)%>%
    mutate(area=as.numeric(st_area(.))/1000/1000)%>%
    data.frame()%>%
    dplyr::select(-Shape)
  
  temp_df_ratio <- temp_df %>%
    group_by(name) %>%
    summarise(ratio = sum(area[Level_Uncertain == "High"]) / 
                sum(area[Level_Uncertain == "Low"])) %>%
    ungroup()
  

  #get the coastal sites for analysis 
  # coastal_planning <- read_sf("R:/Science/CESD/HES_MPAGroup/Data/Shapefiles/coastal planning area/coastal_planning_area.shp")%>%
  #   st_transform(CanProj)
  # 
  # eg_sites <- maritimes_network%>%
  #   st_make_valid()%>%
  #   dplyr::select(name)%>%
  #   st_intersection(coastal_planning)%>% #filter to just the coastal sites
  #   filter(!name %in% c("St. Anns Bank Marine Protected Area ","Bay of Fundy Horse Mussel Aggregations",
  #                       "Southwest Bank"))%>%
  #   data.frame()%>%
  #   pull(name)
  # 
  # write.csv(data.frame(name=eg_sites),file="data/coastal_vegetation_site_list.csv",row.names=FALSE)