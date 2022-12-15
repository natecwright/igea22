# script to read in raw data and organize it
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(stringi)


setwd('/Users/emmaboudreau/Documents/GitHub/igea22/')



g3_files = list.files('outputs/munged_3/EPs/ALT/')
input_file = "ALT_3EP13.rds"


#edit_function = function(input_file) {
  
  reach_ID = substring(strsplit(input_file, "_")[[1]][2], 1, 5)
  

  
  master = readRDS(paste0('outputs/munged_3/EPs/master/master_',reach_ID,'.rds'))
  alt_df = readRDS(paste0('outputs/munged_3/EPs/ALT/ALT_',reach_ID,'.rds'))
  
  
  # edit the data to exclude all submerged water points----
  alt_df = alt_df%>%
    mutate(number= substr(UID2,8,8))%>%
    mutate(XSection= substr(UID2,9,9))%>%
    mutate(Xlabel = number)%>%
  filter(ALT > 0)
    
    
    
    
  tundra_points = alt_df%>%
    filter(LWR != 'W')%>%
    mutate(Xlabel = number)
  
  temp = alt_df%>%
    filter(LWR == 'W')%>%
    filter(XSection != '3')%>%
    filter(XSection != '8')%>%
     filter(number != '2')%>%
    filter(number != '4')
  
  temp2 = alt_df%>%
    filter(LWR == 'W')%>%
    filter(XSection == '3' | XSection == '8')%>%
    filter(number != '2')%>%
    filter(number != '3')%>%
    filter(number != '4')
  
  bank_points = rbind(temp, temp2)%>%
    mutate(Xlabel = as.double('0'))
  
  
  land_points = rbind(tundra_points, bank_points)
  
  submerged_points = anti_join(alt_df, land_points)%>%
    mutate(Xlabel = 'S')
  
  
  
  # ----
  ALT_violin = rbind(land_points,submerged_points)%>%
    filter(!is.na(ALT))
  #saveRDS(ALT_violin, paste0('outputs/munged_3/EP_violin/ALT_violin_',reach_ID,'.rds'))
  
#}

#lapply(g3_files,edit_function)





