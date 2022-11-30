# script to read in basic data and look at it
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)


# setwd (path to the data)
setwd('/Users/emmaboudreau/Documents/GitHub/igea22/')


g3_files=list.files('raw_ts_data/group3.n')
#input_file="3tp11_ts1.txt"

read_function = function(input_file) {
  
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





input1_file=paste0('raw_ts_data/group3/',reach_ID,'_ts1.txt')
raw1_text=readLines(con=input1_file) #read every line
index1_1=grep("END SETUP",raw1_text) #ts1 start of section
index1_2=grep("END SLOPE",raw1_text) #ts1 end of section

ts1_hr_df= read.delim(input1_file,
                          header = TRUE, skip = index1_1, nrows= (index1_2-index1_1-2), dec = ".", sep = ',')%>%
  rename("PointID"="TgtID")%>%
  rename("HR"="RefHt")%>%
  filter(PointID!=101)


input2_file=paste0('raw_ts_data/group3/',reach_ID,'_ts2.txt')
raw2_text=readLines(con=input2_file) #read every line
index2_1=grep("END SETUP",raw2_text) #ts2 start of section
index2_2=grep("END SLOPE",raw2_text) #ts2 end of section

ts2_hr_df= read.delim(input2_file,
                      header = TRUE, skip = index2_1, nrows= (index2_2-index2_1-2), dec = ".", sep = ',')%>%
  rename("PointID"="TgtID")%>%
  rename("HR"="RefHt")%>%
  filter(PointID!=101)




#read in total station txt files
ts1_txt = read.delim(paste0('raw_ts_data/group3.n/',reach_ID,'_ts1.txt'),
                      skip = 1, header = TRUE, dec = ".", sep = ',')%>%
  mutate(TS_code = "1")%>%
  mutate(Reach=ts1_reach$Reach)%>%
  mutate(PointID=as.double(PointID))%>%
  filter(!PointID==101)%>%
  mutate(HR=ts1_hr_df$HR)




  
ts2_txt = read.delim(paste0('raw_ts_data/group3.n/',reach_ID,'_ts2.txt'),
                      skip = 1, header = TRUE, dec = ".", sep = ',')%>%
  mutate(TS_code = "2")%>%
  mutate(Reach=ts2_reach$Reach)%>%
  mutate(PointID=as.double(PointID))%>%
  filter(!PointID==101)%>%
  mutate(HR=ts2_hr_df$HR)
 




#----Join data frames


print(toupper(reach_ID))

#row bind excel files for ts1 and ts2
joined_df1 = rbind(ts1_excel,ts2_excel)%>%
  filter(Reach==reach_ID)%>%
  filter(!is.na(XSection)) %>% #remove PT 101
  mutate(ERRORCODE=ifelse(is.na(ERRORCODE),0,ERRORCODE))

#row bind txt files for ts1 and ts2
joined_df2 = rbind(ts1_txt,ts2_txt)

 


#created data frame with excel and text file for specific reach

master_df = left_join(joined_df1, joined_df2, by=c('Reach', 'TS_code', 'PointID'))%>%

  mutate(uniqueID = paste(Reach,TS_code,Location,XSection))%>%
  mutate(AP = substr(uniqueID,10,10))%>%
  mutate(LWR = substr(uniqueID,9,9))%>%
  mutate(number = substr(uniqueID,11,11))%>%
  mutate(UID2 = paste0(Reach,TS_code,LWR,number,XSection))%>%
  mutate(Elevation = as.double(str_remove_all(Elevation, ' ')))%>%
  filter(PointID!=101)%>%
  mutate(newelev=ifelse(ERRORCODE==2,Elevation + (HR-ADJUSTMENT),Elevation))%>% #adjustment for HR change if errorcode is 2
  filter(ERRORCODE!="8") 
  

  
  

# joins active layer df and permafrost layer df and creates a new column with elevation difference



a_df = select(master_df, UID2, AP, LWR, newelev)%>%
  filter(AP =='A')%>% 
  rename("Active" = "AP")%>% 
  rename("ElevationA" = "newelev")


p_df = select(master_df, UID2, AP, LWR, newelev)%>%
  filter(AP =='P')%>% 
  rename("Permafrost" = "AP")%>% 
  rename("ElevationP" = "newelev")



#joins active layer df and permafrost df and creates a new column with elevation difference
alt_df = left_join(a_df, p_df, by=c('UID2','LWR'))%>%
  mutate(ALT = (ElevationA-ElevationP))



#saving master_df and alt_df as rds and adding 3 in front of reach_ID to distinguish group 3 from 1 and 2
saveRDS(master_df, paste0('outputs/munged_3/master_',reach_ID,'.rds'))
saveRDS(alt_df, paste0('outputs/munged_3/ALT_',reach_ID,'.rds'))




}
#lapply takes thing to be looped over in first position and the function in second position
lapply(g3_files,read_function)
 #read_function(g3_files[1])


