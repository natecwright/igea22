library(dplyr)
library(readxl)
library(ggplot2)




setwd('/Users/emmaboudreau/Documents/GitHub/igea22/outputs/munged_3/')


master_ep = list.files('EPs/master/')
ALT_ep = list.files('EPs/ALT/')

readRDS('EPs/master/master_3EP01.rds')

master_df=do.call(rbind,lapply(paste0('EPs/master/',master_ep),readRDS))
saveRDS(master_df, '/Users/emmaboudreau/Documents/GitHub/igea22/outputs/munged_3/EP_master/EP_master.rds')
ALT_df=do.call(rbind,lapply(paste0('EPs/ALT/',ALT_ep),readRDS))
saveRDS(ALT_df, '/Users/emmaboudreau/Documents/GitHub/igea22/outputs/munged_3/EP_ALT/EP_ALT.rds')

ALT_mean=ALT_df%>%
  mutate(number= substr(UID2,8,8))%>%
  mutate(XSection= substr(UID2,9,9))%>%
  summarize(mean=mean(ALT, na.rm = TRUE))

#function will start here
#NvsALT= function(input_df){



p3=ggplot(input_df,aes(y=ALT, x=ElevationA))+
  #geom_point()+
  geom_point(size=2.5,aes(col=LWR))+
  #geom_line(aes(group=Source,col=Source))+#as.factor makes discreet colors for the points
  #geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, col=Source), width=.2)+
  #coord_cartesian(ylim=c(0,0.5))+
  #scale_x_discrete(labels=c("S" = "Submerged in Water", "1" = "25",
  #                          "2" = "50", "3" = "75", "4" = "100"), limits = c("S","0","1","2","3","4"))+#renames the x axis
  theme_bw()+ #gets rid of grey background
  # xlab('Distance from Bank(m)')+
  # ylab('Mean Active Layer Thickness(m)')+
  # labs(title = "Mean Active Layer Thickness of Pool Type and Season")+
  # labs(colour = 'Stream and Season')+ #title of the legend
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        #legend.position ='none',
        axis.text= element_text(size=14),
        axis.title= element_text(size=14),
        plot.title = element_text(color="black", size=14, face="bold.italic"),
        legend.background = element_rect(fill=alpha('white',0.8)))

plot(p3)

















