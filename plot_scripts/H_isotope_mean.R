#Create and batch violin plot----
library(dplyr)
library(ggplot2)

mean_line=function(rogerandtina){
  library(dplyr)
  library(ggplot2)
  
  p4=ggplot(rogerandtina,aes(y=mean, x=Source))+
    geom_point(size=2.5)+
    #geom_line(aes(group=Source,col=Source))+#as.factor makes discreet colors for the points
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2)+
    #coord_cartesian(xlim=c(0,2))+
    #scale_x_discrete(labels=c("S" = "Submerged in Water", "1" = "25","2" = "50", "3" = "75", "4" = "100"), limits = c("S","0","1","2","3","4"))+#renames the x axis
    theme_bw()+ #gets rid of grey background
    xlab('Stream Classification and Season')+
    ylab('Hydrogen Isotope Ratio')+
    labs(title = "Mean Hydrogen Isotope Ratios by Stream Classification and Season")+
    #labs(colour = 'Stream and Season')+ #title of the legend
    theme(legend.text=element_text(size=14),
          legend.title=element_text(size=14),
          legend.position ='none',
          axis.text= element_text(size=14),
          axis.title= element_text(size=14),
          plot.title = element_text(color="black", size=14, face="bold.italic"),
          legend.background = element_rect(fill=alpha('white',0.8)))
  
  return(p4)
  
}

#batch
#rp3eads and plots all of the files in this folder
setwd('C:/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/water_scripts/')

#Giant_df=do.call(rbind, lapply(file_list, readRDS))

#find the means and sds
ep_early_mean=(readRDS("avgEP_g12.rds"))%>%
  summarize(mean=mean(avgH, na.rm = TRUE), sd=sd(avgH, na.rm = TRUE))%>%
  mutate(Source='EP Early')

ep_late_mean=(readRDS("avgEP_g3.rds"))%>%
  summarize(mean=mean(avgH, na.rm = TRUE), sd=sd(avgH, na.rm = TRUE))%>%
  mutate(Source='EP Late')

tp_early_mean=(readRDS("avgTP_g12.rds"))%>%
  summarize(mean=mean(avgH, na.rm = TRUE), sd=sd(avgH, na.rm = TRUE))%>%
  mutate(Source='TP Early')

tp_late_mean=(readRDS("avgTP_g3.rds"))%>%
  summarize(mean=mean(avgH, na.rm = TRUE), sd=sd(avgH, na.rm = TRUE))%>%
  mutate(Source='TP Late')

final_df=rbind(ep_early_mean,ep_late_mean,tp_early_mean,tp_late_mean)
#final_df=rbind(ep_early_mean,tp_early_mean)
saveRDS(final_df, 'C:/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs/T_test/Isotope.rds')
#mean = Giant_df%>% 
#  group_by(Xlabel)%>% 
#  summarize(average = mean(ALT))%>%
#  ungroup()




plot(mean_line(final_df))