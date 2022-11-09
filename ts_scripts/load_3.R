# script to read in basic data and look at it
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)


# setwd (path to the data)
setwd('/Users/emmaboudreau/Documents/GitHub/igea22/')


g3_files=list.files('raw_ts_data/group3.n')
input_file="3tp11_ts1.txt"

#read_function = function(input_file) {
  
  reach_ID = toupper(strsplit(input_file, "_")[[1]][1])

# read in data ----------


#read in total station excel

ts1_excel = read_xlsx('raw_ts_data/fd/fd3_ts1.xlsx')%>%
  mutate(TS_code  = "1")#%>%
    #filter(is.na(ERRORCODE))



ts2_excel = read_xlsx('raw_ts_data/fd/fd3_ts2.xlsx')%>%
  mutate(TS_code  = "2")#%>%
  #filter(is.na(ERRORCODE))
  
  
# names(ts1_excel) = make.names(names(ts1_excel), unique = TRUE)
# names(ts2_excel) = make.names(names(ts2_excel), unique = TRUE)


#read in metadata
metadata_excel = read_xlsx('raw_ts_data/fd/fd3_metadata.xlsx')%>%
  filter(is.na(Filter))


#read in reach code
ts1_reach = read.delim(paste0('raw_ts_data/group3.n/',reach_ID,'_ts1.txt'),
                       header = FALSE, nrows= 1, dec = ".", sep = '\t')%>%
  transmute(Reach = V1)
 


ts2_reach = read.delim(paste0('raw_ts_data/group3.n/',reach_ID,'_ts2.txt'),
                         header = FALSE, nrows= 1, dec = ".", sep = '\t')%>%
  transmute(Reach = V1)






# ts1_col = read.delim(paste0('raw_ts_data/group3/',reach_ID,'_ts1.txt'),
#                     header = FALSE, dec = ".", sep = ',')#%>%
#   #transmute(rod_height= V7)
# print(colnames(ts1_col))
# transmute(V1)
# ts1_test = read.delim(paste0('raw_ts_data/group3/',reach_ID,'_ts1.txt'),
#                     header = TRUE, col.names, dec = ".", sep = ',')


what=scan(paste0('raw_ts_data/group3/',reach_ID,'_ts1.txt'), what="RefHt",sep="")



#rod height
ts1_HR = read.delim(paste0('raw_ts_data/group3/',reach_ID,'_ts1.txt'),
                       header = FALSE, skip = 120, nrows= 1, dec = ".", sep = ',')%>%
  transmute(rod_height= V7)

ts2_HR = read.delim(paste0('raw_ts_data/group3/',reach_ID,'_ts2.txt'),
                    header = FALSE, skip = 120, nrows= 1, dec = ".", sep = ',')%>%
  transmute(rod_height= V7)


#read in total station txt files
ts1_txt = read.delim(paste0('raw_ts_data/group3.n/',reach_ID,'_ts1.txt'),
                      skip = 1, header = TRUE, dec = ".", sep = ',')%>%
  mutate(TS_code = "1")%>%
  mutate(Reach=ts1_reach$Reach)%>%
  mutate(PointID=as.double(PointID))%>%
  mutate(rod_height=ts1_HR$rod_height)
  #filter(!is.na(PointID))
# manual intervention to get rid of PT 101 which was being problematic


  
ts2_txt = read.delim(paste0('raw_ts_data/group3.n/',reach_ID,'_ts2.txt'),
                      skip = 1, header = TRUE, dec = ".", sep = ',')%>%
  mutate(TS_code = "2")%>%
  mutate(Reach=ts2_reach$Reach)%>%
  mutate(PointID=as.double(PointID))%>%
  mutate(rod_height=ts2_HR$rod_height)
  #filter(!is.na(PointID))




#----Join data frames


print(toupper(reach_ID))

#row bind excel files for ts1 and ts2
joined_df1 = rbind(ts1_excel,ts2_excel)%>%
  #filter(Reach==toupper(reach_ID))%>%
  filter(Reach==reach_ID)%>%
  filter(!is.na(XSection)) #remove PT 101

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
  filter(!PointID==101)%>%
  mutate(newheight=ifelse(ERRORCODE==2,Elevation + (rod_height-ADJUSTMENT),Elevation))#%>%
  # filter(ERRORCODE !="8")   #being problematic and removin all of TS1
  #filter(is.na(ERRORCODE))
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
saveRDS(master_df, paste0('outputs/munged_3/master_',reach_ID,'.rds'))
saveRDS(alt_df, paste0('outputs/munged_3/ALT_',reach_ID,'.rds'))




#}
#lapply takes thing to be looped over in first position and the function in second position
#lapply(g3_files,read_function)
 #read_function(g3_files[1])


