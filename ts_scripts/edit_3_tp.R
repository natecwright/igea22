# script to read in raw data and organize it
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(stringi)

setwd('/Users/emmaboudreau/Documents/GitHub/igea22/')


g3_files = list.files('outputs/munged_3/TPs/ALT/')
#input_file = "ALT_3TP11.rds"

edit_function = function(input_file){
  
reach_ID = substring(strsplit(input_file, "_")[[1]][2], 1, 5)
  
  master = readRDS(paste0('outputs/munged_3/TPs/master/master_',reach_ID,'.rds'))
  alt_df = readRDS(paste0('outputs/munged_3/TPs/ALT/ALT_',reach_ID,'.rds'))%>%
    filter(ALT>0)
  
  
  alt_df = alt_df%>%
    mutate(number= substr(UID2,8,8))%>%
    mutate(XSection= substr(UID2,9,9))
  
  
  # edit the data to exclude all submerged water points----
  
  tundra_points = alt_df%>%
    filter(LWR!= 'W')%>%
    mutate(Xlabel = number)
  
  bank_points = alt_df%>%
    filter(LWR =='W')%>%
    filter(XSection != '2')%>%
    filter(XSection != '5')%>%
    filter(number != '2')%>%
    mutate(Xlabel = as.double('0'))
  
  land_points = rbind(tundra_points, bank_points)
  
  # ----
  
  
  # edit the data to only have submerged water points----
  submerged_points = anti_join(alt_df, land_points)%>%
    mutate(Xlabel = 'S') 
   
  
  
  #create final df
  ALT_violin = rbind(land_points, submerged_points)
  
  saveRDS(ALT_violin, paste0('outputs/munged_3/TP_violin/ALT_violin_',reach_ID,'.rds'))
  
  # ----
  
  
}

lapply(g3_files,edit_function)


