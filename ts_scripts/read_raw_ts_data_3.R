# script to read in basic data and look at it
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)


# setwd (path to the data)
setwd('/Users/emmaboudreau/Documents/GitHub/igea22/')


g3_files=list.files('raw_ts_data/group3.n')

read_function = function(input_file) {
  
  reach_ID = strsplit(input_file, "_")[[1]][1]

# read in data ----------


#read in total station excel

ts1_excel = read_xlsx('raw_ts_data/fd/fd3_ts1.xlsx')%>%
  mutate(TS_code  = "1")

ts2_excel = read_xlsx('raw_ts_data/fd/fd3_ts2.xlsx')%>%
  mutate(TS_code  = "2")
  



#read in metadata
metadata_excel = read_xlsx('raw_ts_data/fd/fd3_metadata.xlsx')


#read in reach code
ts1_reach = read.delim(paste0('raw_ts_data/group3.n/',reach_ID,'_ts1.txt'),
                       header = FALSE, nrows= 1, dec = ".", sep = '\t')%>%
  transmute(Reach = V1)
 


ts2_reach = read.delim(paste0('raw_ts_data/group3.n/',reach_ID,'_ts2.txt'),
                         header = FALSE, nrows= 1, dec = ".", sep = '\t')%>%
  transmute(Reach = V1)



#read in total station txt files
ts1_txt = read.delim(paste0('raw_ts_data/group3.n/',reach_ID,'_ts1.txt'),
                      skip = 1, header = TRUE, dec = ".", sep = ',')%>%
  mutate(TS_code = "1")%>%
  mutate(Reach = ts1_reach$Reach)%>%
  mutate(PointID=as.double(PointID))

# manual intervention to get rid of PT 101 which was being problematic
ts1_txt = ts1_txt[2:nrow(ts1_txt),]

  
ts2_txt = read.delim(paste0('raw_ts_data/group3.n/',reach_ID,'_ts2.txt'),
                      skip = 1, header = TRUE, dec = ".", sep = ',')%>%
  mutate(TS_code = "2")%>%
  mutate(Reach = ts2_reach$Reach)%>%
  mutate(PointID=as.double(PointID))


ts2_txt = ts2_txt[2:nrow(ts2_txt),]

#----Join data frames




#row bind excel files for ts1 and ts2
joined_df1 = rbind(ts1_excel,ts2_excel)

#row bind txt files for ts1 and ts2
joined_df2 = rbind(ts1_txt,ts2_txt)

  #select(joined_df4,-Code) #trying to delete column code


#created data frame with excel and text file for specific reach

master_df = left_join(joined_df1, joined_df2, by=c('Reach', 'TS_code', 'PointID'))%>%

  mutate(uniqueID = paste(Reach,TS_code,Location,XSection))%>%
  mutate(AP = substr(uniqueID,10,10))%>%
  mutate(LWR = substr(uniqueID,9,9))%>%
  mutate(number = substr(uniqueID,11,11))%>%
  mutate(UID2 = paste0(Reach,TS_code,LWR,number,XSection))%>%
  mutate(Elevation = as.double(str_remove_all(Elevation, ' ')))%>%
  filter(!PointID==101)
#joined2_excel_txt_df = master_df[!(master_df$PointID==101),] #removing 101 from excel
#!means not
  
  

# joins active layer df and permafrost layer df and creates a new column with elevation difference



a_df = select(master_df, UID2, AP, LWR, Elevation)%>%
  filter(AP =='A')%>% 
  rename("Active" = "AP")%>% 
  rename("ElevationA" = "Elevation")



p_df = select(master_df, UID2, AP, LWR, Elevation)%>%
  filter(AP =='P')%>% 
  rename("Permafrost" = "AP")%>% 
  rename("ElevationP" = "Elevation")



#joins active layer df and permafrost df and creates a new column with elevation difference
alt_df = left_join(a_df, p_df, by=c('UID2','LWR'))%>%
  mutate(ALT = (ElevationA-ElevationP))



#saving master_df and alt_df as rds and adding 3 in front of reach_ID to distinguish group 3 from 1 and 2
saveRDS(master_df, paste0('outputs/munged_3/master_3',reach_ID,'.rds'))
saveRDS(alt_df, paste0('outputs/munged_3/ALT_3',reach_ID,'.rds'))




}
#lapply takes thing to be looped over in first position and the function in second position
lapply(g3_files,read_function)
read_function(g3_files[1])


