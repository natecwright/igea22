# script to read in raw data and organize it
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(stringi)

setwd('C:/Users/ncw02/Downloads/IGEA/')


g12_files = list.files('outputs/munged_12/EPs/')
input_file = "ALT_EP16.rds"

#edit_function = function(input_file){

reach_ID = substring(toupper(strsplit(input_file, "_")[[1]][2]), 1, 4)

master = readRDS(paste0('outputs/munged_12/EPs/master_',reach_ID,'.rds'))
alt_df = readRDS(paste0('outputs/munged_12/EPs/ALT_',reach_ID,'.rds'))%>%
  filter(ALT > 0)


# edit the data to exclude all submerged water points----

tundra_points = alt_df%>%
  filter(LRW != 'W')%>%
  mutate(Xlabel = Number)

temp = alt_df%>%
  filter(LRW == 'W')%>%
  filter(Cross.section != '3')%>%
  filter(Cross.section != '8')%>%
  filter(Number != '2')%>%
  filter(Number != '4')

temp2 = alt_df%>%
  filter(LRW == 'W')%>%
  filter(Cross.section == '3' | Cross.section == '8')%>%
  filter(Number != '2')%>%
  filter(Number != '3')%>%
  filter(Number != '4')

bank_points = rbind(temp, temp2)%>%
  mutate(Xlabel = as.double('0'))

land_points = rbind(tundra_points, bank_points)
  
submerged_points = anti_join(alt_df, land_points)%>%
  mutate(Xlabel = 'S')

# ----


# edit the data to only have submerged water points----



# 
alt = mean(alt_df$ALT)

ALT_violin = rbind(land_points, submerged_points)

#saveRDS(ALT_violin, paste0('outputs/munged_12/EP_violin/ALT_violin_',reach_ID,'.rds'))

# ----

#}

#dummy = lapply(g12_files,edit_function)




