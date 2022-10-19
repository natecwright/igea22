#Create a plot----

library(dplyr)
library(readxl)
library(ggplot2)

#setwd(path to data) 
setwd('/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22')

#read in file
ep7_elev=readRDS('outputs/ALT.rds')%>%
  filter(LRW=="L"|LRW=="R")#%>%
  #filter(Number==1)
  

p3=ggplot(ep7_elev)+
  geom_violin(aes(y=ALT, x=Number, colour=as.factor(Number)))+ #as.factor makes discreet colors for the points
  #geom_Violin(aes(y=ALT, x=cross.section, colour=as.factor(Cross.section)))+
  stat_summary(aes(y=ALT, x=Number,colour=as.factor(Number)),
                 fun = "mean",
               geom = "crossbar",
               width = 0.5)+
  theme_bw()+ #gets rid of grey background
  xlab('LRW')+
  ylab('Active Layer Thickness(m)')+
  labs(colour = 'LRW')+ #title of the legend
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position ='left',
        axis.text= element_text(size=14),
        axis.title= element_text(size=14),
        plot.title = element_text(color="black", size=14, face="bold.italic"),
        legend.background = element_rect(fill=alpha('white',0.8)))


plot(p3)

#--------------------
