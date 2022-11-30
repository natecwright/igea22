#CRun a T test----

library(dplyr)
library(readxl)
#library(tidyverse)
#library(ggpubr)
#library(rstatix)

#setwd(path to data) 
setwd('/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs/')

#read in file
Sub=readRDS('mean_and_sd.rds')%>%
  filter

Notsub=

t.test(Sub, Notsub, var.equal=TRUE)









#--------------------