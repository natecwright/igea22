# script to read in raw data and organize it
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(stringi)

setwd('C:/Users/ncw02/Downloads/IGEA/')


g12_files = list.files('outputs/munged_12/TPs/')
input_file = "ALT_TP06.rds"

#edit_function = function(input_file){

reach_ID = substring(toupper(strsplit(input_file, "_")[[1]][2]), 1, 4)

master = readRDS(paste0('outputs/munged_12/TPs/master_',reach_ID,'.rds'))
alt_df = readRDS(paste0('outputs/munged_12/TPs/ALT_',reach_ID,'.rds'))


# edit the data to exclude all submerged water points----

bank_points = alt_df%>%
  filter(LRW == 'W')%>%
  filter(Cross.section != '2')%>%
  filter(Cross.section != '5')%>%
  filter(Number != '2')%>%
  mutate(Xlabel = as.double('0'))

tundra_points = alt_df%>%
  filter(LRW != 'W')%>%
  mutate(Xlabel = Number)

land_points = rbind(bank_points, tundra_points)

# ----


# edit the data to only have submerged water points----



submerged_points = anti_join(alt_df, land_points, by = 'UID2')%>%
  mutate(Xlabel = 'S')


# 
ALT_violin = rbind(land_points, submerged_points)#%>%
  #filter(UID2 != 'EP7W381')

saveRDS(ALT_violin, paste0('outputs/munged_12/ALT_violin_',reach_ID,'.rds'))

# ----


#}

#dummy = lapply(g12_files,edit_function)




