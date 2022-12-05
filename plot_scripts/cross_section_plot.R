setwd('C:/Users/ncw02/Downloads/IGEA/')
input_file = 'ep01_ts1'
file_list = list.files('outputs/munged_12/EP_violin/')

#cross_section = function(reach_ID){
  library(dplyr)
  library(ggplot2)
  
  reach_ID = substring(toupper(strsplit(input_file, "_")[[1]][1]), 1, 4)
  #type = substring(toupper(strsplit(input_file, "_")[[1]][2]), 1, 2)
  
  master = readRDS(paste0('outputs/munged_12/EPs/master_',reach_ID,'.rds'))
  alt_df = readRDS(paste0('outputs/munged_12/EPs/ALT_',reach_ID,'.rds'))
  violin = readRDS(paste0('outputs/munged_12/EP_violin/ALT_violin_',reach_ID,'.rds'))
  
  elevation = select(violin, ElevationA, LRW, Number)

    p1=ggplot(elevation,aes(y=ElevationA, x=Xlabel))+
    geom_line(aes(colour="black"))+
    coord_cartesian(ylim=c(320,330))+
    
    scale_x_discrete(labels=c("S" = "S", "1" = "25",
                              "2" = "50", "3" = "75", "4" = "100"), limits = c("S","0","1","2","3","4"))+#renames the x axis
    theme_bw()+ #gets rid of grey background
    xlab('Distance from Bank(m)')+
    ylab('Elevation')+
    labs(title = "Elevation Along Transect")+
    labs(colour = 'LRW')+ #title of the legend
    theme(legend.text=element_text(size=14),
          legend.title=element_text(size=14),
          legend.position ='none',
          axis.text= element_text(size=14),
          axis.title= element_text(size=14),
          plot.title = element_text(color="black", size=14, face="bold.italic"),
          legend.background = element_rect(fill=alpha('white',0.8)))

  return(p1)


plot(p1)



