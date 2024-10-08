## analysis of t-chain data


#load libaries
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(purrr)
library(lubridate)

#load processing function 
load("code/ESI 2025 CSAS/minilog_process_function.R")

minilog_files <- dir('data/Oceanography/2019 T-Chain/')%>%
                .[grepl(".csv",.)]%>%
                .[!grepl("lnk",.)]

minilog_df <- map(minilog_files, minilog_process)%>%
  do.call("rbind",.)

minilog_df_filtered <- minilog_df%>%
                        group_by(device)%>%
                        filter(datetime >= unique(start) & datetime <= unique(stop))%>%
                        ungroup()




