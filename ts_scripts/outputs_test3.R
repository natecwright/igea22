
# script to read in raw data and organize it
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(stringi)


setwd('/Users/emmaboudreau/Documents/GitHub/igea22/')



g3_files = list.files('outputs/munged_3/')
input_file = g3_files[10]



#test_function=function(input_file)     {
  
  
reach_ID = substring(strsplit(input_file, "_")[[1]][2], 1, 5)

master = readRDS(paste0('outputs/munged_3/master_',reach_ID,'.rds'))
alt_df = readRDS(paste0('outputs/munged_3/ALT_',reach_ID,'.rds'))
violin_df = readRDS(paste0('outputs/munged_3/ALT_violin_',reach_ID,'.rds'))


#}

#lapply(g3_files,test_function)

















