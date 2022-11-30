#Create a plot----

library(dplyr)
library(readxl)
library(ggplot2)

#setwd(path to data) 
setwd('/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs/munged_12/EPs/')

#read in file
ep7_elev=readRDS('master_EP01.rds')#%>%
#filter(LRW==W)

p2=ggplot(ep7_elev)+
  geom_point(aes(y=ALT, x=Temperature(C), colour="blue"))+ #as.factor makes discreet colors for the points
  filter(Type == '')+
  theme_bw()+ #gets rid of grey background
  xlab('UID2')+
  ylab('Active Layer Elevation(m)')+
  labs(colour = 'A or P')+ #title of the legend
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position ='left',
        axis.text= element_text(size=14),
        axis.title= element_text(size=14),
        plot.title = element_text(color="black", size=14, face="bold.italic"),
        legend.background = element_rect(fill=alpha('white',0.8)))


plot(p2)

#--------------------
