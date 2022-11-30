#Create and batch violin plot----

mean_line=function(rogerandtina){
  library(dplyr)
  library(ggplot2)
  
  p3=ggplot(rogerandtina,aes(y=mean, x=as.factor(Xlabel)))+
    geom_point(size=2.5,aes(col=Source))+
    geom_line(aes(group=Source,col=Source))+#as.factor makes discreet colors for the points
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, col=Source), width=.2)+
    coord_cartesian(ylim=c(0,0.5))+
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

ep_late=list.files(pattern="ALT_violin_3EP")#chooses the correct files to plot
tp_late=list.files(pattern="ALT_violin_3TP")

ep_late_df=do.call(rbind, lapply(ep_late, readRDS))
saveRDS(ep_late_df, 'ep_late.rds')
tp_late_df=do.call(rbind, lapply(tp_late, readRDS))
saveRDS(tp_late_df, 'tp_late.rds')

setwd('C:/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs/munged_12/final/')
ep_early=list.files(pattern="ALT_violin_EP")
tp_early=list.files(pattern="ALT_violin_TP")

ep_early_df=do.call(rbind, lapply(ep_early, readRDS))
saveRDS(ep_early_df, 'ep_early.rds')
tp_early_df=do.call(rbind, lapply(tp_early, readRDS))
saveRDS(tp_early_df, 'tp_early.rds')

#find the means
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

#mean = Giant_df%>% 
#  group_by(Xlabel)%>% 
#  summarize(average = mean(ALT))%>%
#  ungroup()




plot(mean_line(final_df))