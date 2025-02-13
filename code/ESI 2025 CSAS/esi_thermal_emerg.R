#load libraries
library(tidyverse)
library(sf)
library(raster)
library(MarConsNetData)
library(rnaturalearth)
library(scales)
library(patchwork)
library(ggspatial)
library(viridis)
library(ggimage)
library(rphylopic)
library(R.matlab)
library(zoo)

sf_use_s2(FALSE)

#projections ------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#store root directory
root_dir <- getwd()

maritimes_network <- data_draft_areas()%>%
  st_transform(CanProj)%>%
  st_make_valid()%>%
  dplyr::select(Classification_E,SiteName_E)%>%
  rename(status=Classification_E,name=SiteName_E)%>%
  st_make_valid()

esi_poly <- maritimes_network%>%
  filter(name == "Eastern Shore Islands")

#species habitat loss ----- 
#focal species

#read in the species metadata 

sp_meta <- read.csv("c:/Users/stanleyr/Documents/Github/MAR_thermal_emerg/data/species_grouping.csv")%>%
           rename(common_name = ComName,
                  species = SciName)%>%
           mutate(common_name = case_when(common_name == "(ESS/WSS) Atlantic cod" ~ "Atlantic cod",
                                          common_name == "(ESS/WSS)pollock"~ "Pollock",
                                          common_name == "(ESS/WSS)haddock"~ "Haddock",
                                          TRUE ~ common_name))%>%
           distinct(species,.keep_all=TRUE)

focal_sp <- c("Homarus americanus","Gadus morhua","Hippoglossus hippoglossus","Pollachius virens")

#load the time of emergence summaries
load("c:/Users/stanleyr/Documents/Github/MAR_thermal_emerg/output/toe_summaries/all_toe_summaries.RData")

esi_df <- toe_summaries%>%
          filter(NAME == "Eastern Shore Islands",
                 mod == "Ensemble")%>%
          left_join(.,sp_meta%>%
                      dplyr::select(common_name,species,functional_group,importance))

esi_sp <- esi_df%>%pull(species)%>%unique()

habitat_loss_df <- NULL

for(i in unique(esi_df$species)){
  for(j in unique(esi_df$climate_proj)){
    
    temp <- esi_df%>%
            filter(species == i,
                   climate_proj == j)
    
    
    
  }
  
}

toe_dat <- toe_summaries%>%
  filter(mod != "GFDL",
         #species %in% focal_sp,
         NAME == "Eastern Shore Islands")%>%
  mutate(climate_proj = gsub("\\.","-",climate_proj))# the . was left in from the ensemble calculation


#calculate the total area for each species in the network
agg_network_area <- toe_dat%>%
  group_by(climate_proj,mod,species)%>% #slightly different for each mod and projection 
  summarise(total_area=sum(cell_area))%>%
  ungroup()%>%
  data.frame()%>%
  arrange(species)

#so now we will add together grid cells that emerged in the same year
agg_annual_toe <- toe_dat%>%
  group_by(climate_proj,mod,species,ToE)%>%
  summarise(area_lost=sum(cell_area))%>%
  ungroup()%>%
  data.frame()


#extract the summaries of habitat loss (e.g., when a cell becomes too warm) -

species <- unique(agg_annual_toe$species)
years <- 2015:2100
mods <- unique(agg_annual_toe$mod)
projs <- unique(agg_annual_toe$climate_proj)

#Big 'for loop' == this could probably be done using 'do' in dplyr but this doesn't take long and is easy to follow. 
habitat_loss <- NULL #will grow each loop
for(i in species){
  message(paste0("Working on ",i))
  for(p in projs){
    for(m in mods){
      
      temp <- agg_annual_toe%>%
        filter(species==i,climate_proj == p,mod==m,!is.na(ToE))%>%
        rbind(.,data.frame(climate_proj=p,mod=m,species=i,ToE=setdiff(years,.$ToE),area_lost=0))%>% #add in the years that are missing as 0 loss years
        arrange(ToE)%>% #sort them
        mutate(cum_lost = cumsum(area_lost))%>%
        left_join(.,agg_network_area)%>%# add in the total area for a given mod, projection and species
        mutate(prop_lost = cum_lost/total_area)%>%
        data.frame()%>%suppressMessages()
      
      habitat_loss <- rbind(habitat_loss,temp)
      
    } #end of 'm' mods loop
  } #end of 'p' climate_proj loop
} #end of 'i' species loop

## Species by site 'emergence' based on a threshold of habitat loss summary data

#total area in each site that is occupied by each species
agg_site_area <- toe_dat%>%
  group_by(climate_proj,mod,species,NAME)%>% #slightly different for each mod and projection 
  summarise(total_area=sum(cell_area))%>%
  ungroup()%>%
  data.frame()

habitat_loss_site <- toe_dat%>%
  mutate(ToE = ifelse(is.na(ToE),2500,ToE))%>% #2500 is a placeholder for 'NA' or 'not emerged'
  group_by(climate_proj,mod,species,NAME,ToE)%>%
  summarise(area_lost=sum(cell_area))%>%
  ungroup()%>%
  left_join(agg_site_area)%>% # add in the total area within each site. 
  mutate(prop_area=area_lost/total_area)%>%
  arrange(climate_proj,mod,species,NAME,ToE)%>% #make sure everything is ordered so that ToE's are sequential
  group_by(climate_proj,mod,species,NAME)%>%
  mutate(cum_sum=cumsum(prop_area))%>%
  ungroup()%>%
  data.frame()%>%
  mutate(climate_proj_fact=factor(ifelse(climate_proj == "2-6","RCP 2.6","RCP 8.5")),
         mod = factor(mod, levels=c("AWI","IPSL","HAD","Ensemble")))


#construct the plot
esi_loss <- ggplot(habitat_loss_site%>%filter(species %in% focal_sp), aes(x = ToE, y = cum_sum, color = mod,linetype = mod)) +
  geom_line(linewidth = 1,show.legend = FALSE) +
  geom_point(shape=21,aes(fill=mod),col="black",size=2.1, alpha = 0.6) +
  facet_grid(species~climate_proj_fact) +
  scale_linetype_manual(values = c("AWI" = "dashed", "IPSL" = "dashed", "HAD" = "dashed", "Ensemble" = "solid")) +
  scale_color_manual(values = c("AWI" = "coral2", "IPSL" = "aquamarine4", "HAD" = "cornflowerblue", "Ensemble" = "black")) +
  scale_fill_manual(values = c("AWI" = "coral2", "IPSL" = "aquamarine4", "HAD" = "cornflowerblue", "Ensemble" = "black")) +
  theme_bw() +
  labs(
    x = "Time of Emergence (ToE)",
    y = "% Habitat extent lost",
    fill = "Climate model"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor.y = element_blank(),
    strip.background = element_rect(fill="white"),
    strip.text.y = element_text(size = rel(0.7)),
    panel.grid.minor.x = element_line(color = "lightgrey", linetype = "dashed")
  ) +
  scale_size_continuous(range = c(1, 5)) +
  scale_x_continuous(
    limits = c(2015, 2100),
    breaks = seq(2020, 2100, by = 10)
  ) +
  scale_y_continuous(labels=percent)+
  # Add horizontal line at 1 to show complete habitat loss
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.5)+
  guides(
    color = guide_legend(override.aes = list(size = 5, shape = 21, stroke = 1)),
    linetype = "none",  # Hide linetype legend
    fill = guide_legend(override.aes = list(size = 5, shape = 21, stroke = 1))
  )

ggsave("output/ESI_2025_CSAS/esi_focal_habitat_loss.png",esi_loss,height=7.1,width=8,units="in",dpi=600)


#habitat loss by species 
benchmark_years <- c(2025,2050,2075,2100)


assign_decade <- function(year) {
  if (year >= 2500) return(2100)  # Handle the 2500 case
  return(floor(year/10) * 10)
}

habitat_loss_processed <- habitat_loss_site %>%
  #filter(ToE != 2500)%>%
  # Add decade column
  mutate(decade = sapply(ToE, assign_decade)) %>%
  # Group by relevant variables and get max cumulative sum for each decade
  group_by(climate_proj, mod, species, decade) %>%
  summarise(max_cum_sum = max(cum_sum), .groups = 'drop') %>%
  # Complete the dataset with missing decades
  complete(
    climate_proj, mod, species,
    decade = seq(2030, 2100, by = 10),
    fill = list(max_cum_sum = 0)
  ) %>%
  # Forward fill the cumulative sums within each group
  group_by(climate_proj, mod, species) %>%
  arrange(decade) %>%
  fill(max_cum_sum) %>%
  # Calculate the difference between decades to get the incremental loss
  mutate(
    incremental_loss = max_cum_sum - lag(max_cum_sum, default = 0)
  ) %>%
  ungroup()

# First prepare the data as you have done
plot_df <- habitat_loss_processed %>%
            filter(decade > 2020,
                   decade < 2100,
                   mod == "Ensemble",
                   !species %in% c("Sebastes mentella", "Calanus glacialis","Eubalaena glacialis","Calanus finmarchicus","Chionoecetes opilio"))%>%
            left_join(.,sp_meta%>%
                         dplyr::select(common_name,species,functional_group))

species_order_df <- plot_df %>%
  filter(climate_proj == "8-5") %>%
  group_by(functional_group,species) %>%
  summarize(max_loss = max(max_cum_sum), .groups = 'drop') %>%
  arrange(max_loss)%>%
  left_join(plot_df%>%distinct(species,.keep_all=TRUE)%>%dplyr::select(species,common_name))


species_order_df <- plot_df %>%
  group_by(species, climate_proj) %>%
  summarize(max_loss = max(max_cum_sum), .groups = 'drop') %>%
  # Filter to just one climate projection for ordering
  filter(climate_proj == "8-5") %>%
  arrange(max_loss)%>%
  left_join(plot_df%>%distinct(species,.keep_all=TRUE)%>%dplyr::select(species,common_name))

species_order <- species_order_df%>%pull(species)
common_order <- species_order_df%>%pull(common_name)

# Update the plot_df with new species ordering
plot_df <- plot_df %>%
  mutate(
    # Order species by maximum loss
    species = factor(species, levels = species_order),
    common_name = factor(common_name,levels=common_order),
    # Keep decades in reverse chronological order
    decades = paste0(decade, "s"),
    decades = factor(decades, levels = paste0(sort(unique(decade), decreasing = TRUE), "s")),
    climate_proj = ifelse(climate_proj == "8-5","RCP 8.5","RCP 2.6"),
    climate_proj = factor(climate_proj,levels=c("RCP 8.5","RCP 2.6"))
    )

# Create the plot with custom colors
custom_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628")

esi_toe_hab <- ggplot(plot_df %>% arrange(desc(decade)),
       aes(x = max_cum_sum,
           y = common_name, 
           fill = decades)) +
  geom_vline(xintercept = 0.5,lty=2)+
  geom_col(position = "identity",
           width = 0.7,col="black",linewidth=0.25) +
  scale_x_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  
  scale_fill_manual(values = custom_colors) +
  facet_wrap(~climate_proj, ncol = 1) +
  labs(
    x = "Proportion of habitat reaching ToE",
    y = NULL
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 8),
    legend.position = "right",
    strip.text = element_text(size = 10),
    strip.background = element_rect(fill="white"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank()
  )

ggsave("output/ESI_2025_CSAS/habtiat_toe_species.png",esi_toe_hab,height=7,width=6,units="in",dpi=600)
#Temperature timeseries ------

setwd("c:/Users/stanleyr/Documents/Github/MAR_thermal_emerg/")

fls<-c(list.files("data/climate_projections/2.6/", full.names=T),list.files("data/climate_projections/8.5/",full.names=T)) ##climate projections for RCP 2.6; run again for the 4.5 folder
fls <- fls[!grepl("CNRM",fls)] #remove the CNRM model from the analysis
fls <- fls[!grepl("GFDL",fls)] #remove the GFDL model from the analysis

data <- readMat(fls[1]) #only do this for one projection because they are the same extent
names(data) <- "datout" #this will update the ones with 'outt'

bdata<-brick(data$datout,xmn=-83,xmx=-41,ymn=38,ymx=85,crs=latlong)
cmip_proj <- bdata@crs #projection of the CMIP

network_sp <- esi_poly%>%
              st_transform(st_crs(bdata))%>%
              as_Spatial()

network_extent <- extent(network_sp)

network_raster_mask <- rasterize(network_sp,crop(bdata[[1]],network_extent,snap="out"),getCover=TRUE) 
network_raster_mask[network_raster_mask == 0] <- NA

out_masks <- network_raster_mask
out_networks <- network_sp

emmission_scenarios <- c("2.6","8.5")

#loop over the modeled scenarios
for(i in emmission_scenarios){
  
  cmip_files <- fls[grep(i,fls)]
  
  message(paste0("Loading data for RCP ",i))
  
  #read in data and create brick (month (12) x years (86))
  for(j in 1:3){
    
    temp <- readMat(cmip_files[j])
    names(temp) <- "datout"
    temp <- brick(temp$datout,xmn=-83,xmx=-41,ymn=38,ymx=85,crs=latlong)
    assign(paste0("emmisiondata_",j),temp)
    rm(temp)
    
  }
  
  #get the emsemble for each month/year combination
  
  message("Conducting model averaging to generate an ensemble.")
  
  ensemble.list <- list()
  for(s in 1:dim(emmisiondata_1)[3]){
    
    temp <- stack(emmisiondata_1[[s]],emmisiondata_2[[s]],emmisiondata_3[[s]])
    ensemble <- calc(temp,fun=mean,na.rm=T) # note that 'AWI' doesn't apply a land mask, so this will give us more data for coastal MPAs that are otherwise missed by the HAD/ISPL because of that 0.25 degree land mask
    ensemble.list[[s]] <- ensemble
    
  }
  
  
  #convert to a raster brick
  bdata <- ensemble.list%>%stack()%>%brick(.,xmx=-41,ymn=38,ymx=85,crs=latlong)
  cmip_proj <- proj4string(bdata)
  
  ## do the extracts on the ensembles ---
  
  message(paste0("Working on the climate extractions for RCP ",i))
  
  #loop through the network to extract the data. 
  cmip_extracts<-list()
  cmip_extracts_shapes <- list()
  climate_proj <- i
  mod <- "Ensemble"
  
    #load the depth-adjusted network (sf)
    network_sf <- esi_poly%>%
      mutate(area=st_area(.))%>%
      st_transform(st_crs(bdata))
    
    network_raster_mask <- projectRaster(network_raster_mask,crs=cmip_proj)
    
    #extent of the network
    network_extent <- extent(network_raster_mask)
    
    #apply the mask to the entire raster stack using raster  
    bdata_processed <- bdata%>%
      crop(.,network_extent,snap="out")%>%
      raster::mask(.,network_raster_mask)
    
    spec = "all"
    
    #now use sf and stars to extract the data
    data_extract <- bdata_processed%>% #crop the raster to the extent of the PA
      st_as_stars()%>% #convert to stars raster brick
      st_as_sf()%>% #convert to sf dataframe
      st_transform(utm)%>% #convert to planar coordinates for a more appropriate overlay
      st_intersection(.,network_sf%>%st_transform(utm)%>%dplyr::rename(site_area=area))%>%
      st_transform(st_crs(network_raster_mask))%>% #transform back to the raster brick projection
      mutate(cell_area=as.vector(st_area(.)/1000/1000))%>% #calculate the area of the raster cells that are overlaid 
      gather(.,"layer","temp",starts_with("layer."))%>% #convert to the long-form    
      mutate(month=rep(rep(1:12,each=length(layer)/86/12),86),
             year=rep(2015:2100,each=length(layer)/86),
             species=spec,
             mod=mod,
             climate_proj=climate_proj)%>%
      dplyr::select(mod,climate_proj,species,year,month,name,site_area,cell_area,temp,geometry)%>%
      suppressWarnings() #"attribute variables are assumed to be spatially constant throughout all geometries" - will clutter the output so it is suppressed
    
    #save the geometry information which is duplicated for each iteration (month x year)
    data_extract_shape <- data_extract%>%
      filter(month==1,year==2015)
    
    #output only the extracted information
    data_extract_output <- data_extract%>%
      data.frame()%>%
      dplyr::select(-geometry)
    
    #save the output
    
    save(data_extract_output,file=paste0(root_dir,"/output/ESI_2025_CSAS/cmip_extracts/cmip_rcp_",gsub("\\.","-",i),"_extract.RData"))
    save(data_extract_shape,file=paste0(root_dir,"/output/ESI_2025_CSAS/cmip_extracts/cmip_rcp_",gsub("\\.","-",i),"_shape.RData"))
} #end of emmission loop


#load the outputs
setwd(root_dir)

load("output/ESI_2025_CSAS/cmip_extracts/cmip_rcp_2-6_extract.RData")
cmip_26 <- data_extract_output
rm(data_extract_output)

load("output/ESI_2025_CSAS/cmip_extracts/cmip_rcp_8-5_extract.RData")
cmip_85 <- data_extract_output
rm(data_extract_output)

cmip_df <- rbind(cmip_26,cmip_85)%>%
  group_by(climate_proj,year,month)%>%
  summarise(month_mean=weighted.mean(temp,cell_area))%>%
  ungroup()%>%
  group_by(climate_proj,year)%>%
  summarise(mean=mean(month_mean),
            se=sd(month_mean)/sqrt(12),
            sd=sd(month_mean))%>%
  ungroup()%>%
  data.frame()%>%
  mutate(climate_proj_fact = ifelse(climate_proj == 2.6,"RCP 2.6","RCP 8.5"))

cmip_df_smoothed <- cmip_df %>%
  group_by(climate_proj_fact) %>%
  arrange(year) %>%
  mutate(
    rolling_mean = rollmean(mean, k = 5, fill = NA, align = "center")
  ) %>%
  ungroup()

esi_timeseries <- ggplot(cmip_df_smoothed, aes(x = year, y = mean, color = climate_proj_fact, fill = climate_proj_fact)) +
  annotate("rect", # Add baseline period shading
           xmin = 2015,
           xmax = 2025,
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.5,
           fill = "gray75") +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = rolling_mean), linewidth = 1) +
  theme_bw() +
  facet_wrap(~climate_proj_fact, ncol = 2) +
  labs(
    y = "Temperature (°C) ± se",
    x = ""
  ) +
  theme(
    panel.background = element_rect(fill = "white", colour = "black"),
    panel.grid.major = element_line(color = "lightgray", linetype = "dotted"),
    strip.background = element_rect(fill="white"),
    legend.position = "none"
  )+
  scale_fill_manual(values=c("cornflowerblue","coral3"))+
  scale_color_manual(values=c("cornflowerblue","coral3"))

ggsave("output/ESI_2025_CSAS/esi_temp_timeseries.png",esi_timeseries,height=6,width=6,units="in",dpi=600)
