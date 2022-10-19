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

# manual intervention to get rid of PT 101 which was being problematic
ts1_txt = ts1_txt[2:nrow(ts1_txt),]

  
ts2_txt = read.delim("raw_ts_data/group3.n/ep7_ts2.txt",
                      skip = 1, header = TRUE, dec = ".", sep = ',')%>%
  mutate(TS_code = "2")%>%
  mutate(Reach = ts2_reach$Reach)%>%
  mutate(PointID=as.double(PointID))

# manual intervention to get rid of PT 101 which was being problematic
ts2_txt = ts2_txt[2:nrow(ts2_txt),]

#----

ts_excel_df = rbind(ts1_excel,ts2_excel)

ts_txt_df = rbind(ts1_txt,ts2_txt)

  #select(ts_txt_df,-Code) #trying to delete column code


#created data frame with excel and text file for specific reach
joined_excel_txt_df = left_join(ts_excel_df, ts_txt_df, by=c('Reach', 'TS_code', 'PointID'))%>%
  mutate(uniqueID = paste(Reach,TS_code,Location,XSection))%>%
  mutate(AP = substr(uniqueID,9,9))%>%
  mutate(LWR = substr(uniqueID,8,8))%>%
  mutate(number = substr(uniqueID,10,10))%>%
  mutate(UID2 = paste0(Reach,TS_code,LWR,number,XSection))%>%
  mutate(Elevation = as.double(str_remove_all(Elevation, ' ')))
joined2_excel_txt_df = joined_excel_txt_df[!(joined_excel_txt_df$PointID==101),] #removing 101 from excel
#!means not
  
  

a_df = select(joined_excel_txt_df, UID2, AP, LWR, Elevation)%>%
  filter(AP =='A')%>% 
  rename("Active" = "AP")%>% 
  rename("ElevationA" = "Elevation")



p_df = select(joined_excel_txt_df, UID2, AP, LWR, Elevation)%>%
  filter(AP =='P')%>% 
  rename("Permafrost" = "AP")%>% 
  rename("ElevationP" = "Elevation")

final = left_join(a_df, p_df, by=c('UID2','LWR'))%>%
  mutate(ALT = (ElevationA-ElevationP))


saveRDS(joined_excel_txt_df, 'outputs/joined_excel_txt_df.rds')
saveRDS(final, 'outputs/ALT.rds')


violin_df=select(final, UID2, LWR, ElevationA, ElevationP, ALT)
  filter(LWR=="L"|LWR=="R")



