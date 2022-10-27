#Create and batch violin plot----

early_violin=function(file){
library(dplyr)
library(readxl)
library(ggplot2)

#setwd(path to data) 
setwd('/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22')

#read in file
violin_ALT=readRDS(file)#%>%
  #filter(LRW=="L"|LRW=="R")#%>%
  #filter(Number==1)
  

p3=ggplot(violin_ALT)+
  geom_violin(aes(y=ALT, x=Xlabel, colour=as.factor(Xlabel)))+ #as.factor makes discreet colors for the points
  #geom_Violin(aes(y=ALT, x=cross.section, colour=as.factor(Cross.section)))+
  stat_summary(aes(y=ALT, x=Xlabel,colour=as.factor(Xlabel)),
                 fun = "mean",
               geom = "crossbar",
               width = 0.5)+
  theme_bw()+ #gets rid of grey background
  xlab('Distance from Bank')+
  ylab('Active Layer Thickness(m)')+
  labs(colour = 'LRW')+ #title of the legend
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position ='none',
        axis.text= element_text(size=14),
        axis.title= element_text(size=14),
        plot.title = element_text(color="black", size=14, face="bold.italic"),
        legend.background = element_rect(fill=alpha('white',0.8)))


plot(p3)}


#batch
#reads and plots all of the files in this folder
file_list=list.files('/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs')

lapply(file_list, early_violin)


#--------------------
