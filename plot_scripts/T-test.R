#Run a T test----

library(dplyr)
library(readxl)
#library(tidyverse)
#library(ggpubr)
#library(rstatix)

#setwd(path to data) 
setwd('/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs/T_test')


#read in file
Sub=readRDS('ep_early.rds')#%>%
  #filter(Xlabel==1 | Xlabel==2 | Xlabel==3 | Xlabel==4) #data set 1 to be compared in t test
  #filter(Xlabel==1)
var(Sub$ALT)

Notsub=readRDS('tp_early.rds')#%>%
  #filter(Xlabel=='S') #data set 2 to be compared
var(Notsub$ALT)  

EPe_TPe=t.test(Sub$ALT, Notsub$ALT, var.equal=F) #t test for the alt columns of two sets of data to be compared

EPe_TPe_p=EPe_TPe$p.value




#--------------------