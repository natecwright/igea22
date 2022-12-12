# #Create and batch violin plot----
# 
# #early_violin=function(Giant_df){
#   library(dplyr)
#   library(ggplot2)
#   
#   Mean_df=Giant_df%>%
#     group_by(Xlabel)%>%
#     summarize(mean=mean(ALT, na.rm = TRUE))#creates df with mean values for each Xlabel
#   #!!!!! dire. manual reorganization
#   mean_in_order= format(round(c(Mean_df$mean[6],Mean_df$mean[1:5]),2),nsmall=2)
#   #!!!!!!!!!!!!
#   
#   print(mean_in_order)



setwd('C:/Users/ncw02/Downloads/IGEA/')
#file_list=list.files(pattern="ALT_violin_EP")#chooses the correct files to plot

g12_files = list.files('outputs/munged_12/EPs/')
input_file = "ALT_EP12.rds"

#edit_function = function(input_file){
  
  reach_ID = substring(toupper(strsplit(input_file, "_")[[1]][2]), 1,4)
 
  master_df = readRDS(paste0('outputs/munged_12/EPs/master_',reach_ID,'.rds'))%>%
    filter(Cross.section == 8)
  
  starting_point=filter(master_df,Location=='LA4')
start_east=starting_point$East
start_north=starting_point$North

distance_df=master_df%>%
  mutate(distance= sqrt((East-start_east)^2+(North-start_north)^2))%>%
  filter(Type=='A')

plot(distance_df$distance,distance_df$Elevation)


  p3=ggplot(alt_df,aes(y=ElevationA, x=Xlabel, colour="blue"))+
    coord_cartesian(ylim=c(0,0.5))+
    scale_x_discrete(labels=c("S" = "Submerged in Water", "1" = "25",
                              "2" = "50", "3" = "75", "4" = "100"), limits = c("S","0","1","2","3","4"))+#renames the x axis
    theme_bw()+ #gets rid of grey background
    xlab('Distance from Bank(m)')+
    ylab('Active Layer Thickness(m)')+
    labs(title = "Elevation Along Cross Section")+
    theme(legend.text=element_text(size=14),
          legend.title=element_text(size=14),
          legend.position ='none',
          axis.text= element_text(size=14),
          axis.title= element_text(size=14),
          plot.title = element_text(color="black", size=14, face="bold.italic"),
          legend.background = element_rect(fill=alpha('white',0.8)))
  
  return(p3)
  
  #}
  

plot(early_violin(Giant_df))



