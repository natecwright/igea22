# script to read in raw data and organize it
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(stringi)

setwd('C:/Users/ncw02/Downloads/IGEA/')


g12_files = list.files('outputs/munged_12/')
input_file = "ALT_TP17.rds"

#edit_function = function(input_file){

reach_ID = substring(toupper(strsplit(input_file, "_")[[1]][2]), 1, 4)

master = readRDS(paste0('outputs/munged_12/master_',reach_ID,'.rds'))
alt_df = readRDS(paste0('outputs/munged_12/ALT_',reach_ID,'.rds'))


# edit the data to exclude all submerged water points----
# we want all W1, W3, and W5 EXCEPT on cross sections 3 and 8, exclude W3

#exclude water points; initial pass
step1 = alt_df%>%
  filter(LRW != 'W')%>%
  mutate(Temp = paste0(LRW, Number))%>%
  filter(Temp != 'W2')%>%
  filter(Temp != 'W4')

#remove cross section 3 and 8
step2 = step1%>%
  filter(Cross.section !='3')%>%
  filter(Cross.section !='8')


step3 = step1%>%
  filter(Cross.section == '3' | Cross.section == '8')%>%
  filter(Number != '3')%>%
  mutate(Xlabel = '0')

step4 = alt_df%>%
  filter(LRW != 'W')%>%
  mutate(Temp = paste0(LRW, Number))%>%
  mutate(Xlabel = Number)

step5 = rbind(step2, step3)

land_points = rbind(step4, step5)%>%
  mutate(Xlabel = as.double('0'))

# ----


# edit the data to only have submerged water points----

step11 = alt_df%>%
  filter(LRW == 'W')%>%
  mutate(Temp = paste0(LRW, Number))%>%
  filter(Temp != 'W1')%>%
  filter(Temp != 'W5')

x3_water = step11%>%
  filter(Cross.section =='3')%>%
  filter(Temp!='W2')%>%
  filter(Temp!='W4')

x8_water = step11%>%
  filter(Cross.section =='8')%>%
  filter(Temp!='W2')%>%
  filter(Temp!='W4')

most_water = step11%>%
  filter(Temp!='W3')

submerged_points = rbind(most_water, x3_water, x8_water)%>%
  mutate(Xlabel = 'S')


# 
ultimate = rbind(land_points, submerged_points)#%>%
  #filter(UID2 != 'EP7W381')

saveRDS(ultimate, paste0('outputs/munged_12/ALT_violin_',reach_ID,'.rds'))

# ----


#}

#dummy = lapply(g12_files,edit_function)




