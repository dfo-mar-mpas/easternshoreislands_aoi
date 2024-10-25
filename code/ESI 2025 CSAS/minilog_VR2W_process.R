## analysis of temperature - logger data. Note that these were not t-chains but actually were
## loggers affixed to VR2W recievers deployed in 2029


#load libaries
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(purrr)
library(lubridate)
library(viridis)

#load processing function 
source("code/ESI 2025 CSAS/minilog_process_function.R")

#get the file paths for the minilog datasets
minilog_files <- dir('data/Oceanography/areciever_tloggers/',full.names = TRUE)%>%
                .[grepl(".csv",.)]%>%
                .[!grepl("lnk",.)]%>%
                .[!grepl("start",.)]

#extract and format the data
minilog_df <- map(minilog_files, minilog_process)%>%
  do.call("rbind",.)

### Dynamic selection -----
    # #get the hourly data (time-step is every 5 minutes which makes rendering the plots difficult)
    # df_hourly <- minilog_df%>%rename(temperature = temp,device_id = device)%>%
    #              mutate(datetime_hourly = floor_date(datetime, "hour")) %>%
    #              group_by(device_id, datetime_hourly) %>%
    #              summarize(temperature = first(temperature), .groups = 'drop')
    # 
    # #Extract the start and end for each time series using shiny and plotly. Here, you use the sliders to identify the
    # #likely start and end in year, month, day, hour format and then record those so that you can trim the data. 
    # #this is subjective and not totally repeatable, but I found it better than trying to automate a start/end selection. 
    # 
    # 
    # ui <- fluidPage(
    #   titlePanel("Temperature Time Series"),
    #   sidebarLayout(
    #     sidebarPanel(
    #       selectInput("device", "Select Device:", choices = unique(df_hourly$device_id)),
    #       verbatimTextOutput("selected_range")  # Output for displaying the selected range
    #     ),
    #     mainPanel(
    #       plotlyOutput("timeSeriesPlot")
    #     )
    #   )
    # )
    # 
    # # Server
    # server <- function(input, output) {
    #   filtered_data <- reactive({
    #     df_hourly %>% filter(device_id == input$device)
    #   })
    #   
    #   output$timeSeriesPlot <- renderPlotly({
    #     req(filtered_data())
    #     
    #     p <- plot_ly(data = filtered_data(), x = ~datetime_hourly, y = ~temperature, type = 'scatter', mode = 'lines') %>%
    #       layout(title = paste("Temperature Time Series for Device", input$device),
    #              xaxis = list(title = "Datetime"),
    #              yaxis = list(title = "Temperature (°C"),
    #              dragmode = "zoom")
    #     
    #     p
    #   })
    #   
    #   # Track the selected range
    #   output$selected_range <- renderText({
    #     range <- event_data("plotly_relayout")
    #     print(range)  # For debugging - will show in R console
    #     
    #     if (!is.null(range) && !is.null(range[["xaxis.range[0]"]])) {
    #       start_time <- as.POSIXct(range[["xaxis.range[0]"]])
    #       end_time <- as.POSIXct(range[["xaxis.range[1]"]])
    #       
    #       paste("Selected Start Time:", start_time, "\n",
    #             "Selected End Time:", end_time)
    #     } else {
    #       "No time range selected."
    #     }
    #   })
    # }
    # 
    # # Run the app
    # shinyApp(ui = ui, server = server)


##Outputs from this manual start/end selection is available 
start_end_df <- read.csv("data/Oceanography/areciever_tloggers/start_end_dates_tlog_vr2w.csv")%>%
                mutate(start=as.POSIXct(trimws(start), format="%Y-%m-%d %H:%M:%S"),
                       stop=as.POSIXct(trimws(stop), format="%Y-%m-%d %H:%M:%S"))


minilog_df_filtered <- minilog_df%>%
  rename(rec_start=start,rec_stop=stop)%>%
  left_join(.,start_end_df)%>%
  group_by(device)%>%
  filter(datetime >= unique(start) & datetime <= unique(stop))%>%
  ungroup()

#3 hour smoother

minilog_smoothed <- minilog_df_filtered %>%
                  mutate(datetime = floor_date(datetime, "6 hours")) %>%
                  group_by(device, datetime) %>%
                  summarise(temperature = mean(temp, na.rm = TRUE),
                            sd =sd(temp,na.rm=T)) %>%
                  ungroup()%>%
                  data.frame()%>%
                  mutate(year=year(datetime))

logger_order <- minilog_smoothed%>%
                mutate(month=month(datetime),
                       day=day(datetime))%>%
                filter(month==6,day==15)%>%
                arrange(-temperature)%>%
                pull(device)


p1 <- ggplot(minilog_smoothed%>%filter(year>2018), aes(x = datetime, y = temperature, group = device)) +
  geom_line(aes(color = temperature), alpha = 0.8) +  # Color based on temperature
  geom_smooth(aes(group = device), method = "loess", span = 0.2, se = FALSE, color = "black",lwd=0.75) +  # Smoothed line, grouped by device
  scale_color_viridis(option = "C", direction = 1) +  # Viridis color scale for temperature
  labs(title = "2019 VR2W temperature logger deployment",
       x = "Time",
       y = "Temperature (°C)",
       color = "Temperature (°C)") +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.05,0.80),
        legend.background = element_blank(),
        legend.title = element_blank())

ggsave("output/ESI 2025 CSAS/VR2W_tlogger_2019_plot.png",p1,width=7,height=5,units="in",dpi=300)
