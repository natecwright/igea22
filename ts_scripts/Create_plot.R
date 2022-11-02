#Create a plot----

library(dplyr)
library(readxl)
library(ggplot2)

#setwd(path to data) 
setwd('/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22')

#read in file
ep7=readRDS('outputs/ep7.rds')#%>%
#filter(Cross.section==1)

p1=ggplot(ep7)+
  geom_point(aes(y=North,x=East, colour=as.factor(Cross.section)))+ #as.factor makes discreet colors for the points
  theme_bw()+ #gets rid of grey background
  xlab('Easting (m)')+
  ylab('Northing (m)')+
  labs(colour = 'Cross Section')+ #title of the legend
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position ='left',
        axis.text= element_text(size=14),
        axis.title= element_text(size=14),
        plot.title = element_text(color="black", size=14, face="bold.italic"),
        legend.background = element_rect(fill=alpha('white',0.8)))


plot(p1)

#--------------------
