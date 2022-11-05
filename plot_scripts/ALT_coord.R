#Create a plot----

library(dplyr)
library(readxl)
library(ggplot2)



#setwd(path to data) 
setwd('/Users/emmaboudreau/Documents/GitHub/igea22/')

#read in file-----
g3_elev=readRDS('outputs/munged_3/ALT_3ep07.rds')
xs3=substr(g3_elev$UID2,8,8)

g1_elev=readRDS('outputs/munged_12/ALT_EP07.rds')
xs1=substr(g1_elev$UID2,8,8)


g3_coord=readRDS('outputs/munged_3/master_3ep07.rds')

g12_coord=readRDS('outputs/munged_12/master_EP07.rds')

#-----

#xs_g3_df=







#--------------------

# p2=ggplot(ep7_elev)+
#   geom_point(aes(y=ALT, x=UID2, colour="blue"))+ #as.factor makes discreet colors for the points
#   geom_point(aes(y=ALT, x=UID2, colour="red"))+
#   theme_bw()+ #gets rid of grey background
#   xlab('UID2')+
#   ylab('Active Layer Elevation(m)')+
#   labs(colour = 'A or P')+ #title of the legend
#   theme(legend.text=element_text(size=14),
#         legend.title=element_text(size=14),
#         legend.position ='left',
#         axis.text= element_text(size=14),
#         axis.title= element_text(size=14),
#         plot.title = element_text(color="black", size=14, face="bold.italic"),
#         legend.background = element_rect(fill=alpha('white',0.8)))
# 
# 
# plot(p2)






