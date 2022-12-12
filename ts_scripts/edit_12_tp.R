# script to read in raw data and organize it
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(stringi)

setwd('C:/Users/ncw02/Downloads/IGEA/')


g12_files = list.files('outputs/munged_12/TPs/')
#input_file = "ALT_TP06.rds"

edit_function = function(input_file){

reach_ID = substring(toupper(strsplit(input_file, "_")[[1]][2]), 1, 4)

master = readRDS(paste0('outputs/munged_12/TPs/master_',reach_ID,'.rds'))
alt_df = readRDS(paste0('outputs/munged_12/TPs/ALT_',reach_ID,'.rds'))%>%
  filter(ALT > 0)


# edit the data to exclude all submerged water points----

tundra_points = alt_df%>%
  filter(LRW != 'W')%>%
  mutate(Xlabel = Number)

bank_points = alt_df%>%
  filter(LRW =='W')%>%
  filter(Cross.section != '2')%>%
  filter(Cross.section != '5')%>%
  filter(Number != '2')%>%
  mutate(Xlabel = as.double('0'))
  
land_points = rbind(tundra_points, bank_points)

# ----


# edit the data to only have submerged water points----
submerged_points = anti_join(alt_df, land_points)%>%
  mutate(Xlabel = 'S')


#create final df
ALT_violin = rbind(land_points, submerged_points)

saveRDS(ALT_violin, paste0('outputs/munged_12/TP_violin/ALT_violin_',reach_ID,'.rds'))

# ----


}

dummy = lapply(g12_files,edit_function)




