#CRun a T test----

library(dplyr)
library(readxl)
#library(tidyverse)
#library(ggpubr)
#library(rstatix)

#setwd(path to data) 
setwd('/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22')

#read in file
group1=readRDS('outputs/ALT_violin.rds')%>%
  Filter(Xlabel=1)

t.test(group1, group2, var.equal=TRUE)









#--------------------