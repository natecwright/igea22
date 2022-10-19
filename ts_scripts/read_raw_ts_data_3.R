# script to read in basic data and look at it
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)

setwd('/Users/emmaboudreau/Documents/GitHub/igea22/')

# read in data ----------
# setwd (path to the data)
#read in total station excel

ts1_excel = read_xlsx('raw_ts_data/fd/fd3_ts1.xlsx')%>%
  mutate(TS_code  = "1")%>%
  filter(Reach == "3EP7") #specified reach


ts2_excel = read_xlsx('raw_ts_data/fd/fd3_ts2.xlsx')%>%
  mutate(TS_code  = "2")%>%
  filter(Reach == "3EP7") #specified reach
  
#read in metadata
metadata_excel = read_xlsx('raw_ts_data/fd/fd3_metadata.xlsx')
#read in reach code
ts1_reach = read.delim("raw_ts_data/group3.n/ep7_ts1.txt",
                       header = FALSE, nrows= 1, dec = ".", sep = '\t')%>%
  transmute(Reach = V1)

ts2_reach = read.delim("raw_ts_data/group3.n/ep7_ts2.txt",
                         header = FALSE, nrows= 1, dec = ".", sep = '\t')%>%
  transmute(Reach = V1)

ts1_txt = read.delim("raw_ts_data/group3.n/ep7_ts1.txt",
                      skip = 1, header = TRUE, dec = ".", sep = ',')%>%
  mutate(TS_code = "1")%>%
  mutate(Reach = ts1_reach$Reach)%>%
  mutate(PointID=as.double(PointID))

  
ts2_txt = read.delim("raw_ts_data/group3.n/ep7_ts2.txt",
                      skip = 1, header = TRUE, dec = ".", sep = ',')%>%
  mutate(TS_code = "2")%>%
  mutate(Reach = ts2_reach$Reach)%>%
  mutate(PointID=as.double(PointID))
#----

ts_excel_df = rbind(ts1_excel,ts2_excel)

ts_txt_df = rbind(ts1_txt,ts2_txt)

  #select(ts_txt_df,-Code) #trying to delete column code


#created data frame with excel and text file for specific reach
joined_excel_txt_df = left_join(ts_excel_df, ts_txt_df, by=c('Reach', 'TS_code', 'PointID')) %>%
  mutate(uniqueID = paste(Reach,TS_code,Location,XSection)) %>%
  mutate(AP = substr(uniqueID,9,9)) 
  #mutate(LWR=substr(uniqueID,))
    #pivot_wider(names_from = AP, values_from = Elevation) %>%
  #mutate(ID = str_replace_all(uniqueID,"A","")) %>%
  #mutate(ID = str_replace_all(ID,"P",""))%>%
  #group_by(ID)%>%
  #mutate(delta_elev = sum(A,na.rm=T)-sum(P,na.rm=T))
#sum is used because there are two A (and P) values for each ID one is an elevation and the other is an NA
#,na.rm=T tells the script to ignore the NA and continue with calculation
  

#where emma left off at 9


# -------------------
# P_df = select(joined_excel_txt_df, AP, uniqueID, Elevation) %>%
#     filter(AP == 'P') %>%
#     mutate(ID = str_replace_all(uniqueID,"P",""))
# 
# A_df = select(joined_excel_txt_df, AP, uniqueID, Elevation) %>%
#   filter(AP == 'A') %>%
# mutate(ID = str_replace_all(uniqueID,"A",""))
# 
# AP_df = rbind(P_df, A_df, by=c('ID')) %>%
#   #pivot_wider(names_from = AP, values_from = Elevation)
# 

# test a join ----
#joined_df = left_join(ts1_excel, metadata_excel, by='Reach')%>%
  #filter(Reach == 'E7')%>%
  #select('Reach', 'Point ID', 'Elevation')

#combined_df = rbind(ts1_elev, ts2_elev)

#plot(joined_df$'Point ID', joined_df$Elevation)

# -------

