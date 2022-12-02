
reach_ID = substring(toupper(strsplit(input_file, "_")[[1]][2]), 1, 4)
type = substring(toupper(strsplit(input_file, "_")[[1]][2]), 1, 2)
  
master = readRDS(paste0('outputs/munged_12/EPs/master_',reach_ID,'.rds'))
alt_df = readRDS(paste0('outputs/munged_12/EPs/ALT_',reach_ID,'.rds'))
violin = readRDS(paste0('outputs/munged_12/EP_violin/ALT_violin_',reach_ID,'.rds'))

elevation = select(violin, ElevationA, Xlabel)

early_violin=function(Giant_df){
  library(dplyr)
  library(ggplot2)
  
  elevation_df=Giant_df%>%
    group_by(Xlabel)%>%
    summarize(mean=mean(ALT, na.rm = TRUE))#creates df with mean values for each Xlabel
  #!!!!! dire. manual reorganization
  mean_in_order= format(round(c(Mean_df$mean[6],Mean_df$mean[1:5]),2),nsmall=2)
  
    p3=ggplot(Giant_df,aes(y=ALT, x=Xlabel, colour=as.factor(Xlabel)))+
    geom_line(aes(colour="black"))+
    coord_cartesian(ylim=c(0,0.5))+
    
    scale_x_discrete(labels=c("S" = "Submerged in Water", "1" = "25",
                              "2" = "50", "3" = "75", "4" = "100"), limits = c("S","0","1","2","3","4"))+#renames the x axis
    theme_bw()+ #gets rid of grey background
    xlab('Distance from Bank(m)')+
    ylab('Elevation')+
    labs(title = "...")+
    labs(colour = 'LRW')+ #title of the legend
    theme(legend.text=element_text(size=14),
          legend.title=element_text(size=14),
          legend.position ='none',
          axis.text= element_text(size=14),
          axis.title= element_text(size=14),
          plot.title = element_text(color="black", size=14, face="bold.italic"),
          legend.background = element_rect(fill=alpha('white',0.8)))

  return(p3)
  
}

#batch
#reads and plots all of the files in this folder
setwd('C:/Users/ncw02/Downloads/IGEA/outputs/munged_12/EP_violin')
file_list=list.files(pattern="ALT_violin_EP")#chooses the correct files to plot


Giant_df=do.call(rbind, lapply(file_list, readRDS))


plot(early_violin(Giant_df))





