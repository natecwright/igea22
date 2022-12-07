#Create and batch violin plot----

early_violin=function(Giant_df){
library(dplyr)
library(ggplot2)

Mean_df=Giant_df%>%
    group_by(Xlabel)%>%
    summarize(mean=mean(ALT, na.rm = TRUE))#creates df with mean values for each Xlabel
  #!!!!! dire. manual reorganization
  mean_in_order= format(round(c(Mean_df$mean[6],Mean_df$mean[1:5]),2),nsmall=2)
  #!!!!!!!!!!!!
  
  print(mean_in_order)
  p3=ggplot(Giant_df,aes(y=ALT, x=Xlabel, colour=as.factor(Xlabel)))+
    geom_jitter()+#as.factor makes discreet colors for the points
    geom_boxplot(aes(fill=as.factor(Xlabel)), alpha = 0.5, colour="black")+
    coord_cartesian(ylim=c(0,1))+
    stat_summary(fun = "mean",
               geom = "crossbar",
               width = 0.5,
               alpha=0.5)+
    annotate(geom="text", x=Mean_df$Xlabel, y=1, label=paste('mu=',format(round(Mean_df$mean,2),nsmall=2)),
             color="black")+
    #stat_summary(fun = "mean",geom="text",label=mean_in_order,colour="black")+
    #scale_x_discrete(limits = c("S","0","1","2","3","4"))+#reorders the x-axis
    scale_x_discrete(labels=c("S" = "Submerged in Water", "1" = "25",
                              "2" = "50", "3" = "75", "4" = "100"), limits = c("S","0","1","2","3","4"))+#renames the x axis
  theme_bw()+ #gets rid of grey background
  xlab('Distance from Bank(m)')+
  ylab('Active Layer Thickness(m)')+
  labs(title = "Active Layer Thickness of Late Season Elliptical Pools")+#CHANGE ME
  labs(colour = 'LRW')+ #title of the legend
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position ='none',
        axis.text= element_text(size=14),
        axis.title= element_text(size=14),
        plot.title = element_text(color="black", size=14, face="bold.italic"),
        legend.background = element_rect(fill=alpha('white',0.8)))
  #geom_line(data = mean,mapping = aes(x = Xlabel, y = average, group=1),color="green")

return(p3)

}

#batch
#reads and plots all of the files in this folder
setwd('C:/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs/munged_3/EP_violin/') #CHANGE ME
file_list=list.files(pattern="ALT_violin") #chooses the correct files to plot


Giant_df=do.call(rbind, lapply(file_list, readRDS))

  
  #plot(Giant_df$Xlabel)
  
  #Giant_df2=Giant_df[ which(!is.na(str_match(Giant_df$UID2,"EP7"))),]
  
  #mean = Giant_df%>% 
  # group_by(Xlabel)%>% 
  # summarize(average = mean(ALT))%>%
  # ungroup()


plot(early_violin(Giant_df))


#------------------
#Create and batch violin plot----


#define list of objects to bind together
#file_list=list("ALT_tp6, ALT_tp11, ALT_tp13, ALT_tp14, ALT_tp17")

#bind together list of objects
#big_data <- rbindlist(data_list)

#batch
#reads and plots all of the files in this folder
#file_list=list.files('/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs')
#Giant_df=do.call(rbind, list.files("ALT_tp6, ALT_tp11, ALT_tp13, ALT_tp14, ALT_tp17"))
#early_violin(Giant_df)

#lapply(file_list, early_violin)

#--------------------
