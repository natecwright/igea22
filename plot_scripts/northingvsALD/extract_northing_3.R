#ALD vs Northing
library(dplyr)
library(ggplot2)

setwd('C:/Users/ncw02/Downloads/IGEA/')

g3_files = list.files('outputs/munged_3/EPs/ALT/')
#input_file = "ALT_3EP01.rds"

northing_function = function(input_file){

reach_ID = substring(toupper(strsplit(input_file, "_")[[1]][2]), 1, 5)

master = readRDS(paste0('outputs/munged_3/EPs/master/master_',reach_ID,'.rds'))
alt_df = readRDS(paste0('outputs/munged_3/EPs/ALT/ALT_',reach_ID,'.rds'))%>%
  filter(ALT > 0)

mean = mean(alt_df$ALT)

northing = master[1, 'North']

plot = master[1, 'Reach']%>%
  mutate(Northing = northing$North)%>%
  mutate(ALT = mean)%>%
  rename("Northing" = "Northing")

saveRDS(plot, paste0('outputs/EP_northing/northing_',reach_ID,'.rds'))

}

lapply(g3_files,northing_function)
