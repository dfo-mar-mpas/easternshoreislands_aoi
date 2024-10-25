#Function for scraping data from minilog files

minilog_process<-function(x){
  
  require(dplyr)
  require(lubridate)
  
  df_meta <- read.csv(x)
  
  #the column headers from the minilog have a degress symbol which R doesn't like. Date, Time, Temp, so id Date and then key out the temp
  data_start <- which(grepl("Date",df_meta[,1]))%>%suppressWarnings() + 2
  
  #specified start and stop times
  start <- df_meta[which(grepl("Study Start",df_meta[,1]))%>%suppressWarnings(),1]%>%
    gsub("Study Start Time: ","",.)%>%
    as.POSIXct(., format="%Y-%m-%d %H:%M:%S")
  
  stop <- df_meta[which(grepl("Study Stop",df_meta[,1]))%>%suppressWarnings(),1]%>%
    gsub("Study Stop Time: ","",.)%>%
    as.POSIXct(., format="%Y-%m-%d %H:%M:%S")
  
  #identify the mini log id
  minilog <- df_meta[which(grepl("Source Device",df_meta[,1]))%>%suppressWarnings(),1]%>%
    gsub("Source Device: ","",.)
  
  df_data <- read.csv(x,skip=data_start,header=FALSE)%>%
    rename(date=1,time=2,temp=3)%>%
    mutate(device = minilog,
           datetime = as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M:%S"),
           date=as.POSIXct(date),
           time=hms(time),
           year=year(date),
           month=month(date),
           day=day(date),
           start=start,
           stop=stop)%>%
    dplyr::select(device,datetime,date,time,year,month,day,temp,start,stop)
  
  
  return(df_data)
  
  
}