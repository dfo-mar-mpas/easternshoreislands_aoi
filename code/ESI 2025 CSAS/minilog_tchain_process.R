## analysis of t-chain data

#load libaries
library(tidyverse)
library(purrr)
library(lubridate)
library(viridis)
library(stringr)
library(oce)

#load process funciton
source("code/ESI 2025 CSAS/minilog_process_function.R")

##functions for interpolating depth for the raster plot

depth_approx=function(x,nout=30){ #using the approxfun 
  data.frame(approx(x$depth,x$temp,xout = seq(min(x$depth),max(x$depth),length.out=nout)))%>%
    rename(depth=x,temp=y)%>%return()
}

depth_linear=function(x,nout=30){ #using an explict linear modl
  
  mod <- lm(temp~depth,data=x)
  data.frame(depth =seq(min(x$depth),max(x$depth),length.out=nout))%>%
    mutate(temp=as.vector(predict(mod,newdata=.)))%>%return()
}

#matlibplot like colour scale
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

#identify the data and assign depths
# tchain_files <- data.frame(file=dir("data/Oceanography/2019 Temperature chain/",full.names = TRUE))%>%
#                 mutate(depth = gsub("_ship-harbour-shallow_M2085_dec18-nov19.csv","",file),
#                        device = gsub("data/Oceanography/2019 Temperature chain/PER2019051_","",depth),
#                        device = stringr::str_extract(device,"[^_]*_[^_]*"),
#                        device = gsub("II-T_","II-T-",device), # to get it to match the processed extraction format
#                        depth = gsub("data/Oceanography/2019 Temperature chain/PER2019051_Minilog-II-T_","",depth),
#                        depth = sub(".*_", "", depth),
#                        depth = as.numeric(sub("p",".",depth)))
# 
# #process the data 
# tchain_df <- map(tchain_files%>%pull(file), minilog_process)%>%
#               do.call("rbind",.)%>%
#               left_join(tchain_files%>%dplyr::select(depth,device))
# 
# save("data/tchain.RData")

  load("data/Oceanography/2019 Temperature chain/tchain.RData")


#set boundaries for the start and end point of the time-series
#these were approximated using the start of the drop or increase in temperature along the full timeseries
start_time <-  as.POSIXct("2018-12-08 07:05:00 AST", format="%Y-%m-%d %H:%M:%S")
end_time <- as.POSIXct("2019-11-25 10:55:00 AST", format="%Y-%m-%d %H:%M:%S")

#hourly tchain data
hourly_data <- tchain_df %>%
              filter(datetime > start_time,datetime<end_time)%>%
              group_by(datetime = floor_date(datetime, "hour"), depth) %>%
              summarise(temp = mean(temp, na.rm = TRUE)) %>%
              ungroup()%>%
              group_by(datetime)%>%
              do(depth_approx(.,nout=100))%>%
              ungroup()%>%
              mutate(month=month(datetime),
                     year=year(datetime),
                     season=case_when(month %in% 1:3 ~"Winter",
                                      month %in% 4:6 ~ "Spring",
                                      month %in% 7:9 ~ "Summer",
                                      month %in% 10:12 ~ "Fall"),
                     season=factor(season,levels=c("Winter","Spring","Summer","Fall")))

#tchain data
depths_data <- data.frame(
  datetime = as.POSIXct("2019-01-7"),  # Fixed date for vertical bar
  depth = unique(tchain_df$depth),
  season = rep(c("Winter","Spring","Summer","Fall"),each=length(unique(tchain_df$depth))))%>%
  mutate(season = factor(season,levels=c("Winter","Spring","Summer","Fall")))

season_breaks <- data.frame(datetime=c("2019-04-01","2019-07-01","2019-10-01"))%>%
                 mutate(datetime = as.POSIXct(datetime))

 
#set the plotting range    
temp_range <- hourly_data%>%pull(temp)%>%range()
temp_range[1] <- round(temp_range[1],1)
temp_range[2] <- ceiling(temp_range[2])

season_plot <- ggplot(data = hourly_data%>%filter(year==2019), aes(x = datetime, y = depth)) +
                geom_tile(aes(fill=temp)) +  
                scale_fill_gradientn(colors = jet.colors(100),  
                                     name = expression("Temp (" * degree * "C)"),limits=temp_range)+
                scale_y_reverse(expand=c(0,0)) +  # Reverse y-axis for depth
                scale_x_datetime(expand=c(0,0))+
                labs(x = "", y = "Depth (m)") +
                theme_bw()+   
                geom_vline(xintercept = unique(depths_data$datetime))+
                geom_vline(xintercept = season_breaks$datetime,lty=2,col="black",lwd=0.75)+
                geom_point(data = depths_data%>%filter(season=="Winter"), aes(x = datetime, y = depth),
                fill="white",color = "black", size = 3,pch=21)+
                #facet_wrap(~season, scales = "free_x", strip.position = "top",nrow=1) + ## creates some issues with the spacing (fall is shorter)
                theme(
                  panel.spacing = unit(0, "lines"),             # Removes space between panels
                  strip.background = element_blank(),           # Removes strip background to look continuous
                  strip.placement = "outside",                  # Places labels outside the panel grid
                  strip.text = element_text(size = 10, face = "bold"),  # Adjusts strip text size and style
                  panel.border = element_blank(),               # Removes panel border
                  axis.ticks.x = element_blank()                # Removes axis ticks for seamless look
                )
                # geom_text(data = hourly_data %>% #doesn't look as good. 
                #             filter(year==2019)%>%
                #             group_by(season) %>%
                #             summarise(datetime = mean(datetime), depth = min(depth) - 1),  # Position label slightly above tiles
                #           aes(label = season),
                #           vjust = 1.5, size = 5, fontface = "bold")

ggsave("output/ESI 2025 CSAS/tchain-plot.png",season_plot,width=8,height=4,units="in",dpi=300)


##Clark Richards -- OCE way of doing it. 

# library(oce)
# load("tchain.RData")
# device <- unique(tchain_df$device)
# temp <- split(tchain_df, tchain_df$device)
# trange <- range(tchain_df$datetime)
# ti <- seq(trange[1], trange[2], by = 600)
# Ti <- Di <- array(NA, dim = c(length(ti), length(temp)))
# for (i in seq_along(temp)) {
#   Ti[, i] <- with(temp[[i]], approx(datetime, temp, ti)$y)
#   Di[, i] <- with(temp[[i]], approx(datetime, depth, ti)$y)
# }
# o <- order(apply(Di, 2, mean, na.rm = TRUE))
# Ti <- Ti[, o]
# Di <- Di[, o]
# meanD <- apply(Di, 2, mean, na.rm = TRUE)
# imagep(ti, meanD, Ti, flipy = TRUE, zlim = c(-2, 20), decimate = FALSE, col = oceColorsJet)
# imagep(ti, meanD, Ti,
#        flipy = TRUE, zlim = c(-2, 20), decimate = FALSE, col = oceColorsJet,
#        xlim = as.POSIXct(c("2019-07-01", "2019-07-31"))
# )
