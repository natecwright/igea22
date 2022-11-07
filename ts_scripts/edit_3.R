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
  
  step1 = alt_df%>%
    filter(LWR != 'W')%>%
    mutate(number= substr(UID2,8,8))%>%
    mutate(XSection= substr(UID2,9,9))%>%
    mutate(Temp = paste0(LWR, number))%>%
    filter(Temp != 'W2')%>%
    filter(Temp != 'W4')%>%
    mutate(Xlabel = as.double('0'))
  
  step2 = step1%>%
    filter(XSection !='3')%>%
    filter(XSection !='8')
  
  step3 = step1%>%
    filter(XSection == '3' | XSection == '8')%>%
    filter(number != '3')%>%
    mutate(Xlabel = '0')
  
  step4 = alt_df%>%
    filter(LWR != 'W')%>%
    mutate(number= substr(UID2,8,8))%>%
    mutate(XSection= substr(UID2,9,9))%>%
    mutate(Temp = paste0(LWR, number))%>%
    mutate(Xlabel = number)
  
  step5 = rbind(step2, step3)
  
  land_points = rbind(step4, step5)
  
  # ----
  
  
  # edit the data to only have submerged water points----
  
  step11 = alt_df%>%
    filter(LWR == 'W')%>%
    mutate(number= substr(UID2,8,8))%>%
    mutate(XSection= substr(UID2,9,9))%>%
    mutate(Temp = paste0(LWR, number))%>%
    filter(Temp != 'W1')%>%
    filter(Temp != 'W5')
  
  x3_water = step11%>%
    filter(XSection =='3')%>%
    filter(Temp!='W2')%>%
    filter(Temp!='W4')
  
  x8_water = step11%>%
    filter(XSection =='8')%>%
    filter(Temp!='W2')%>%
    filter(Temp!='W4')
  
  most_water = step11%>%
    filter(Temp!='W3')
  
  submerged_points = rbind(most_water, x3_water, x8_water)%>%
    mutate(Xlabel = 'S')
  
  
  # 
  ultimate = rbind(land_points, submerged_points)
 
  
  saveRDS(ultimate, paste0('outputs/munged_3/ALT_violin_',reach_ID,'.rds'))
  
  # ----
  
  
}

lapply(g3_files,edit_function)








# find mean of all points & just water ----
# mean = mean(alt_df$ALT)
# 
# 
# water = alt_df%>%
#   filter(LWR =='W')
# 
# mean2 = mean(water$ALT)
