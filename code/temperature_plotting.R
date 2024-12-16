# Plotting temperature data from the ESI Hobo Tidbit loggers


# Load libraries ----------------------------------------------------------
library(lubridate)
library(dplyr)
library(ggplot2)

# Tidbit Recovered December 16 2024 ---------------------------------------
# Read in data - had to change column names in the file as R didn't like the degrees symbol 

esi_temp <- read.csv("data/ESI_Bird_Islands_2024_21644666 2024-12-16 13_13_05 AST.csv", header = T) %>% glimpse()

# filter data to remove time on land when the tidbit was logging at the start and end
# need to start at event 10 and end on 26940
esi_temp <-esi_temp[10:26940,]


#plot as a line plot
ggplot(data = esi_temp, aes(x=mdy_hm(Date), y=Temperature, group=1))+
  geom_line(linewidth=1, color="blue")+
  #geom_point()+
  xlab("Date")+
  ylab("Temperature (°C)")+
  theme_bw(base_size = 16)

ggsave("output/BirdIslands_2024_Tidbit_TemperaturePlot.png",plot = last_plot(), width = 12, height=10, dpi = 300,bg = "white")
