#CRun a T test----

library(dplyr)
library(readxl)
#library(tidyverse)
#library(ggpubr)
#library(rstatix)

#setwd(path to data) 
setwd('/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs/T_test')

#read in file
group1=readRDS('outputs/ALT_violin.rds')%>%
  Filter(Xlabel=1)

t.test(group1, group2, var.equal=TRUE)

#read in file
Sub=readRDS('ep_early.rds')%>%
  filter(Xlabel==1)
  

Notsub=readRDS('ep_early.rds')%>%
  filter(Xlabel=='S')

  
EPe_S_0=t.test(Sub$ALT, Notsub$ALT, var.equal=TRUE)






#--------------------