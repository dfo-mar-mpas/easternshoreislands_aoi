# Plotting temperature data from the ESI Hobo Tidbit loggers


# Load libraries ----------------------------------------------------------
library(lubridate)
library(dplyr)
library(ggplot2)
library(patchwork)

# Tidbit Recovered December 16 2024 ---------------------------------------
# Read in data - had to change column names in the file as R didn't like the degrees symbol 

esi_temp <- read.csv("data/temperatureLogs/ESI_Bird_Islands_2024_21644666 2024-12-16 13_13_05 AST.csv", header = T) %>% glimpse()

# filter data to remove time on land when the tidbit was logging at the start and end
# need to start at event 10 and end on 26940
esi_temp <-esi_temp[10:26940,]


#plot as a line plot
p1 <- ggplot(data = esi_temp, aes(x=mdy_hm(Date), y=Temperature, group=1))+
  geom_line(linewidth=1, color="blue")+
  #geom_point()+
  ylim(-1, 20)+
  xlab("Date")+
  ylab("Temperature (°C)")+
  theme_bw(base_size = 20);p1

ggsave("output/BirdIslands_2024_Tidbit_TemperaturePlot.png",plot = last_plot(), width = 12, height=6, dpi = 300,bg = "white")



# Tidbit recovered July 29 2025 --------------------------------------------------------

esi_temp2 <- read.csv("data/temperatureLogs/ESI_Bird_Islands_L31-21724381 2025-07-29 14_25_20 ADT_Processed.csv", header = T) %>% glimpse()

## already removed data from on land before and after deployment 

p2 <- ggplot(data = esi_temp2, aes(x=mdy_hm(Date.Time_AST_ADT), y=Temperature, group=1))+
  geom_line(linewidth=1, color="blue")+
  #geom_point()+
  ylim(-1,20)+
  xlab("Date")+
  scale_x_discrete(breaks=c("Jan","Feb","Mar","Apr","May","Jun","Jul"))+
  theme_bw(base_size = 20)+
  theme(axis.title.y = element_blank());p2


p1 | p2

ggsave("output/bird_islands_2024-2025_Tidbit_Temperatureplot.png", plot=last_plot(), width=12, height = 8, dpi=300, bg="white")
