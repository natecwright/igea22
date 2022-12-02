# script to read in raw data and organize it
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(stringi)

setwd('C:/Users/ncw02/Downloads/IGEA/')

g12_files = list.files('raw_ts_data/group12.n')
input_file = "tp17_ts2.txt"


reach_ID = strsplit(input_file, "_")[[1]][1]
  
# read in data ----------
# setwd (path to the data)

ts1_excel = read_xlsx('raw_ts_data/fd/fd12_ts1.xlsx')%>%
  mutate(TS_code  = "1")%>% 
  rename("Notebook.notes" = "Notes")%>%
  filter(is.na(ERRORCODE))

ts2_excel = read_xlsx('raw_ts_data/fd/fd12_ts2.xlsx')%>%
  mutate(TS_code  = "2")%>% 
  rename("Notebook.notes" = "Notes")%>%
  filter(is.na(ERRORCODE))

names(ts1_excel) = make.names(names(ts1_excel), unique = TRUE)
names(ts2_excel) = make.names(names(ts2_excel), unique = TRUE)

metadata_excel = read_xlsx('raw_ts_data/fd/fd12_metadata.xlsx')%>%
  rename("Sample.elevation" = "Elevation")


#----


# make ADJUSTMENTS ----

input2_file=paste0('raw_ts_data/group12/',reach_ID,'_ts2.txt')
raw2_text=readLines(con=input2_file) #read every line
index2_1=grep("END SETUP",raw2_text) #ts2 start of section
index2_2=grep("END SLOPE",raw2_text) #ts2 end of section

ts2_hr_df= read.delim(input2_file,
                      header = TRUE, skip = index2_1, nrows= (index2_2-index2_1-2), dec = ".", sep = ',')%>%
  rename("PointID"="TgtID")%>%
  rename("HR"="RefHt")%>%
  filter(PointID != '101')


#----

# extract reach from txt file for ts2 (should be identical for ts1)
ts2_reach = read.delim(paste0('raw_ts_data/group12.n/',reach_ID,'_ts2.txt'),
                        header = FALSE, nrows= 1, dec = ".", sep = '\t')%>%
  transmute(Reach = V1)

# read in ts2 xyz
ts2_txt = read.delim(paste0('raw_ts_data/group12.n/',reach_ID,'_ts2.txt'),
                     skip = 1, header = TRUE, dec = ".", sep = ',')%>%
  mutate(TS_code = "2")%>%
  mutate(Reach = ts2_reach$Reach)%>%
  mutate(PointID = as.double(PointID))%>%
  filter(PointID != '101')%>%
  filter(!is.na(PointID))%>%
  mutate(HR=ts2_hr_df$HR)

# manual intervention to get rid of PT 101 which was being problematic
ts2_txt = ts2_txt[2:nrow(ts2_txt),]



# -------------------


# join data frames ----
reach_ID = toupper(reach_ID)

# this joins excel (digitized data) with metadata for ts2
joined_df1 = left_join(ts2_excel, metadata_excel, by='Reach')%>%
  filter(Reach == reach_ID)

# this joins both ts1 and ts2 data to complete the entire reach
# creates a unique ID and another unique ID without A's and P's
master_df = left_join(joined_df1, ts2_txt, by=c('Reach','PointID', 'TS_code'))%>%
  mutate(ERRORCODE=ifelse(is.na(ERRORCODE),0,ERRORCODE))%>%
  mutate(uniqueID = paste0(Reach, PointID, Location, Cross.section, TS_code))%>%
  mutate(LRW = substr(uniqueID,8,8))%>%
  mutate(Type = substr(uniqueID,9,9))%>%
  mutate(Number = substr(uniqueID,10,10))%>%
  mutate(UID2 = paste0(Reach, LRW, Number, Cross.section, TS_code))%>%
  mutate(Elevation = as.double(str_remove_all(Elevation, ' ')))%>%
  mutate(newelev=ifelse(ERRORCODE==2,Elevation + (HR-ADJUSTMENT),Elevation))%>% #adjustment for HR change if errorcode is 2
  filter(ERRORCODE!="8") 

# -------


# separate A's and P's  ----

a_df = select(master_df, UID2, Cross.section, LRW, Type, Number, Elevation, newelev)%>%
  filter(Type == 'A')%>% 
  rename("Active" = "Type")%>% 
  rename("ElevationA" = "newelev")
  #mutate(ElevationA = as.double(str_remove_all(ElevationA, ' '))) #removes weird spaces from text file and converts
  # the string to a double

p_df = select(master_df, UID2, Cross.section, LRW, Type, Number, Elevation, newelev)%>%
  filter(Type == 'P')%>% 
  rename("Permafrost" = "Type")%>% 
  rename("ElevationP" = "newelev")
  #mutate(ElevationP = as.double(str_remove_all(ElevationP, ' '))) #removes weird spaces from text file and converts
  #the string to a double

# joins active layer df and permafrost layer df and creates a new column with elevation difference
alt_df = left_join(a_df, p_df, by=c('UID2','LRW', 'Number', 'Cross.section'))%>%
  mutate(ALT = (ElevationA-ElevationP))%>%
  filter(ALT > 0)

print(input_file)
print((nrow(alt_df) - length(unique(alt_df$UID2)))/nrow(alt_df))


saveRDS(master_df, paste0('outputs/munged_12/TPs/master_',reach_ID,'.rds'))
saveRDS(alt_df, paste0('outputs/munged_12/TPs/ALT_',reach_ID,'.rds'))


# --------


