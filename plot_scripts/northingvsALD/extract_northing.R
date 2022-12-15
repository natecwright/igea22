#ALD vs Northing
library(dplyr)
library(ggplot2)

setwd('C:/Users/ncw02/Downloads/IGEA/')

g12_files = list.files('outputs/munged_12/TPs/')
input_file = "ALT_TP13.rds"

#northing_function = function(input_file){

reach_ID = substring(toupper(strsplit(input_file, "_")[[1]][2]), 1, 4)

master = readRDS(paste0('outputs/munged_12/TPs/master_',reach_ID,'.rds'))
alt_df = readRDS(paste0('outputs/munged_12/TPs/ALT_',reach_ID,'.rds'))%>%
  filter(ALT > 0)

mean = mean(alt_df$ALT)

northing = master[1, 'Northing']

plot = master[1, 'Reach']%>%
  mutate(Northing = northing$Northing)%>%
  mutate(ALT = mean)%>%
  rename("Northing" = "Northing")

#saveRDS(plot, paste0('outputs/munged_12/TP_northing/northing_',reach_ID,'.rds'))

#}

#lapply(g12_files,northing_function)
