 #Northing vs ALD plot
library(ggplot2)
library(dplyr)

setwd('C:/Users/ncw02/Downloads/IGEA/outputs/EP_northing/')
file_list=list.files(pattern="northing_")
EP_df=do.call(rbind, lapply(file_list, readRDS))

setwd('C:/Users/ncw02/Downloads/IGEA/outputs/TP_northing/')
file_list=list.files(pattern="northing_")
TP_df=do.call(rbind, lapply(file_list, readRDS))

p1=ggplot(TP_df,aes(y=Northing, x=ALT), size = 10)+
  geom_point()+
  theme_bw()+
  xlab('Active Layer Depth (m)')+
  ylab('Northing')+
  labs(title = "ALD vs Northing for Thaw Pits")+#CHANGE ME
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position ='none',
        axis.text= element_text(size=14),
        axis.title= element_text(size=14),
        plot.title = element_text(color="black", size=14, face="bold.italic"),
        legend.background = element_rect(fill=alpha('white',0.8)))

plot(p1)




p2=ggplot(EP_df,aes(y=Northing, x=ALT))+
  geom_point()+
  theme_bw()+
  xlab('Active Layer Depth (m)')+
  ylab('Northing')+
  labs(title = "ALD vs Northing for Elliptical Pools")+#CHANGE ME
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position ='none',
        axis.text= element_text(size=14),
        axis.title= element_text(size=14),
        plot.title = element_text(color="black", size=14, face="bold.italic"),
        legend.background = element_rect(fill=alpha('white',0.8)))

plot(p2)


