#Create and batch violin plot----

mean_line=function(rogerandtina){
  library(dplyr)
  library(ggplot2)
  
  p3=ggplot(rogerandtina,aes(y=mean, x=as.factor(Xlabel)))+
    geom_point(size=2.5,aes(col=Source))+
    geom_line(aes(group=Source,col=Source))+#as.factor makes discreet colors for the points
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, col=Source), width=.2)+
    coord_cartesian(ylim=c(0,0.6))+
    scale_x_discrete(labels=c("S" = "Submerged in Water", "1" = "25",
                              "2" = "50", "3" = "75", "4" = "100"), limits = c("S","0","1","2","3","4"))+#renames the x axis
    theme_bw()+ #gets rid of grey background
    xlab('Distance from Bank(m)')+
    ylab('Mean Active Layer Thickness(m)')+
    labs(title = "Mean Active Layer Thickness of Pool Type and Season")+
    labs(colour = 'Stream and Season')+ #title of the legend
    theme(legend.text=element_text(size=14),
          legend.title=element_text(size=14),
          #legend.position ='none',
          axis.text= element_text(size=14),
          axis.title= element_text(size=14),
          plot.title = element_text(color="black", size=14, face="bold.italic"),
          legend.background = element_rect(fill=alpha('white',0.8)))
  
  return(p3)
  
}

#batch
#rp3eads and plots all of the files in this folder
setwd('C:/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs/munged_3/')

#Giant_df=do.call(rbind, lapply(file_list, readRDS))

ep_late=list.files("EP_violin")#chooses the correct files to plot
tp_late=list.files("TP_violin")

setwd('C:/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs/munged_3/EP_violin/')
ep_late_df=do.call(rbind, lapply(ep_late, readRDS))
saveRDS(ep_late_df, 'C:/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs/T_test/ep_late.rds')
setwd('C:/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs/munged_3/TP_violin/')
tp_late_df=do.call(rbind, lapply(tp_late, readRDS))
saveRDS(tp_late_df, 'C:/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs/T_test/tp_late.rds')

setwd('C:/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs/munged_12/')
ep_early=list.files("EP_violin")
tp_early=list.files("Tp_violin")

setwd('C:/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs/munged_12/EP_violin/')
ep_early_df=do.call(rbind, lapply(ep_early, readRDS))
saveRDS(ep_early_df, 'C:/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs/T_test/ep_early.rds')
setwd('C:/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs/munged_12/TP_violin/')
tp_early_df=do.call(rbind, lapply(tp_early, readRDS))
saveRDS(tp_early_df, 'C:/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs/T_test/tp_early.rds')

#find the means and sds
ep_early_mean=ep_early_df%>%
  group_by(Xlabel)%>%
  summarize(mean=mean(ALT, na.rm = TRUE), sd=sd(ALT, na.rm = TRUE))%>%
  mutate(Source='EP early')
  
ep_late_mean=ep_late_df%>%
  group_by(Xlabel)%>%
  summarize(mean=mean(ALT, na.rm = TRUE), sd=sd(ALT, na.rm = TRUE))%>%
  mutate(Source='EP late')

tp_early_mean=tp_early_df%>%
  group_by(Xlabel)%>%
  summarize(mean=mean(ALT, na.rm = TRUE), sd=sd(ALT, na.rm = TRUE))%>%
  mutate(Source='TP early')

tp_late_mean=tp_late_df%>%
  group_by(Xlabel)%>%
  summarize(mean=mean(ALT, na.rm = TRUE), sd=sd(ALT, na.rm = TRUE))%>%
  mutate(Source='TP late')

final_df=rbind(ep_early_mean,ep_late_mean,tp_early_mean,tp_late_mean)
#final_df=rbind(ep_early_mean,tp_early_mean)
saveRDS(final_df, 'C:/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs/T_test/final.rds')
#mean = Giant_df%>% 
#  group_by(Xlabel)%>% 
#  summarize(average = mean(ALT))%>%
#  ungroup()




plot(mean_line(final_df))