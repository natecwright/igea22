# script to read in raw data and organize it
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(stringi)

setwd('C:/Users/ncw02/Downloads/IGEA/')


master=readRDS('outputs/master_ep7.rds')
alt_df=readRDS('outputs/ALT_ep7.rds')


# find mean of all points & just water ----
mean = mean(alt_df$ALT)


water = alt_df%>%
  filter(LRW =='W')

mean2 = mean(water$ALT)


# ----


# edit the data to exclude all submerged water points----

step1 = alt_df%>%
  filter(LRW == 'W')%>%
  mutate(Temp = paste0(LRW, Number))%>%
  filter(Temp != 'W2')%>%
  filter(Temp != 'W4')%>%
  mutate(Xlabel = as.double('0'))

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

ultimate = rbind(step4, step5)

saveRDS(ultimate, 'outputs/ALT2.rds')


# ----


# edit the data to only have submerged water points----



# ----





