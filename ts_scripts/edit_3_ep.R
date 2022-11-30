# script to read in raw data and organize it
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(stringi)


setwd('/Users/emmaboudreau/Documents/GitHub/igea22/')



g3_files = list.files('outputs/munged_3/')
#input_file = "ALT_3EP01.rds"


edit_function = function(input_file) {
  
  reach_ID = substring(strsplit(input_file, "_")[[1]][2], 1, 5)
  

  
  master = readRDS(paste0('outputs/munged_3/master_',reach_ID,'.rds'))
  alt_df = readRDS(paste0('outputs/munged_3/ALT_',reach_ID,'.rds'))
  
  
  # edit the data to exclude all submerged water points----
  
  tundra_points = alt_df%>%
    filter(LRW != 'W')%>%
    mutate(Xlabel = Number)
  
  temp = alt_df%>%
    filter(LRW == 'W')%>%
    mutate(Number= substr(UID2,8,8))%>%
    mutate(XSection= substr(UID2,9,9))%>%
    filter(XSection != '3')%>%
    filter(XSection != '8')%>%
     filter(Number != '2')%>%
    filter(Number != '4')
  
  temp2 = alt_df%>%
    mutate(Number= substr(UID2,8,8))%>%
    mutate(XSection= substr(UID2,9,9))%>%
    filter(LRW == 'W')%>%
    filter(XSection == '3' | XSection == '8')%>%
    filter(Number != '2')%>%
    filter(Number != '3')%>%
    filter(Number != '4')
  
  bank_points = rbind(temp, temp2)%>%
    mutate(Xlabel = as.double('0'))
  
  
  land_points = rbind(tundra_points, bank_points)
  
  submerged_points = anti_join(alt_df, land_points)%>%
    mutate(Xlabel = 'S')
  
  
  
  # ----
  ALT_violin = rbind(land_points, submerged_points)
  saveRDS(ultimate, paste0('outputs/munged_3/ALT_violin_',reach_ID,'.rds'))
  
}

lapply(g3_files,edit_function)





