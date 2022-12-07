#Create a plot----

library(dplyr)
library(readxl)
library(ggplot2)

setwd('/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs/munged_12/EP_violin/')

xlabel=list.files(pattern="ALT_violin")
file1=do.call(rbind, lapply(xlabel, readRDS))#file with xlabel

#setwd(path to data) 
setwd('/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs/munged_12/EPs/')

#read in file
ep_early=list.files(pattern = "master")
file2=do.call(rbind, lapply(ep_early, readRDS))#file with temp


new_df = left_join(file1,file2, by = 'UID2')%>%#join temp and xlabel
  filter(Xlabel=='S')%>%
  mutate(Temperature(C) = Temperature)

temp_xlabel=function(new_df){
  
p2=ggplot(new_df,aes(y=Temperature(C), x=ALT))+
  geom_point(size=2.5)+
  coord_cartesian(ylim=c(0,15))+
  theme_bw()+ #gets rid of grey background
  xlab('Distance from Bank(m)')+
  ylab('Temp(C)')+
  labs(title = "Mean Active Layer Thickness of Pool Type and Season")+
  labs(colour = 'Stream and Season')+ #title of the legend
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        #legend.position ='none',
        axis.text= element_text(size=14),
        axis.title= element_text(size=14),
        plot.title = element_text(color="black", size=14, face="bold.italic"),
        legend.background = element_rect(fill=alpha('white',0.8)))



return(p2)}

plot(temp_xlabel(new_df))

#--------------------
