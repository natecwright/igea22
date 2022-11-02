#Create and batch violin plot----

early_violin=function(giant_df){
library(dplyr)
library(ggplot2)


  

p3=ggplot(giant_df)+
  geom_boxplot(aes(y=ALT, x=Number, colour=as.factor(Number)))+ #as.factor makes discreet colors for the points
  #geom_Violin(aes(y=ALT, x=cross.section, colour=as.factor(Cross.section)))+
  coord_cartesian(ylim=c(0,1),xlim=c(0,5))+
  stat_summary(aes(y=ALT, x=Number,colour=as.factor(Number)),
                 fun = "mean",
               geom = "crossbar",
               width = 0.5)+
  theme_bw()+ #gets rid of grey background
  xlab('Distance from Bank')+
  ylab('Active Layer Thickness(m)')+
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
#rp3eads and plots all of the files in this folder
setwd('C:/Users/Stella/OneDrive - University of Massachusetts/Documents/IGEA/Munge/igea22/outputs/munged_12/')
file_list=list.files(pattern="ALT_EP")


Giant_df=do.call(rbind, lapply(file_list, readRDS))
  #Giant_df['Number'][Giant_df['Number'] == '1'] <- '25'
  #Giant_df['Number'][Giant_df['Number'] == '2'] <- '50'
  #Giant_df['Number'][Giant_df['Number'] == '3'] <- '75'
  #Giant_df['Number'][Giant_df['Number'] == '4'] <- '100'
  #Giant_df['Number'][Giant_df['Number'] == '5'] <- '0'
  #filter(ALT<0)

Giant_df2=Giant_df[ which(!is.na(str_match(Giant_df$UID2,"EP7"))),]



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
