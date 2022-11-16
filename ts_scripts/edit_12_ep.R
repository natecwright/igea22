# script to read in raw data and organize it
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(stringi)

setwd('C:/Users/ncw02/Downloads/IGEA/')


g12_files = list.files('outputs/munged_12/EPs')
#input_file = "ALT_EP12.rds"

edit_function = function(input_file){

reach_ID = substring(toupper(strsplit(input_file, "_")[[1]][2]), 1, 4)

master = readRDS(paste0('outputs/munged_12/EPs/master_',reach_ID,'.rds'))
alt_df = readRDS(paste0('outputs/munged_12/EPs/ALT_',reach_ID,'.rds'))

# edit the data to exclude all submerged water points----
tundra_points = alt_df%>%
  filter(LRW != 'W')%>%
  mutate(Xlabel = Number)

water_points = alt_df%>%
  filter(LRW == 'W')
  
temp = water_points%>%
  filter(Cross.section !='3')%>%
  filter(Cross.section !='8')%>%
  filter(Number != '2')

temp2 = water_points%>%
  filter(Cross.section == '3' | Cross.section =='8')%>%
  filter(Number != '2')%>%
  filter(Number != '3')%>%
  filter(Number != '4')
  
bank_points = rbind(temp, temp2)%>%
  mutate(Xlabel = as.double('0'))

land_points = rbind(bank_points, tundra_points)


# ----


# edit the data to only have submerged water points----

submerged_points = anti_join(water_points, bank_points, by = 'UID2')%>%
  mutate(Xlabel = 'S')

# step11 = alt_df%>%
#   filter(LRW == 'W')%>%
#   mutate(Temp = paste0(LRW, Number))%>%
#   filter(Temp != 'W1')%>%
#   filter(Temp != 'W5')
# 
# x3_water = step11%>%
#   filter(Cross.section =='3')%>%
#   filter(Temp!='W2')%>%
#   filter(Temp!='W4')
# 
# x8_water = step11%>%
#   filter(Cross.section =='8')%>%
#   filter(Temp!='W2')%>%
#   filter(Temp!='W4')
# 
# most_water = step11%>%
#   filter(Temp!='W3')
# 
# submerged_points = rbind(most_water, x3_water, x8_water)%>%
#   mutate(Xlabel = 'S')


# create the final df by joining land points and submerged points
ALT_violin = rbind(land_points, submerged_points)

saveRDS(ALT_violin, paste0('outputs/munged_12/ALT_violin_',reach_ID,'.rds'))

# ----


}

dummy = lapply(g12_files,edit_function)




