# script to read in raw data and organize it
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(stringi)

setwd('C:/Users/ncw02/Downloads/IGEA/')

g12_files = list.files('raw_ts_data/group12.n')
input_file = "tp05_ts1.txt"

#read_function = function(input_file){

reach_ID = toupper(strsplit(input_file, "_")[[1]][1])
  
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

# extract reach from txt file for ts1 (should be identical for ts2)
ts1_reach = read.delim(paste0('raw_ts_data/group12.n/',reach_ID,'_ts1.txt'),
                       header = FALSE, nrows= 1, dec = ".", sep = '\t')%>%
  transmute(Reach = V1)

# extract reach from txt file for ts2 (should be identical for ts1)
ts2_reach = read.delim(paste0('raw_ts_data/group12.n/',reach_ID,'_ts2.txt'),
                        header = FALSE, nrows= 1, dec = ".", sep = '\t')%>%
  transmute(Reach = V1)

# read in ts1 xyz
ts1_txt = read.delim(paste0('raw_ts_data/group12.n/',reach_ID,'_ts1.txt'),
                      skip = 1, header = TRUE, dec = ".", sep = ',')%>%
  mutate(TS_code = "1")%>%
  mutate(Reach = ts1_reach$Reach)%>%
  mutate(PointID = as.double(PointID))

# manual intervention to get rid of PT 101 which was being problematic
ts1_txt = ts1_txt[2:nrow(ts1_txt),]

# read in ts2 xyz
ts2_txt = read.delim(paste0('raw_ts_data/group12.n/',reach_ID,'_ts2.txt'),
                      skip = 1, header = TRUE, dec = ".", sep = ',')%>%
  mutate(TS_code = "2")%>%
  mutate(Reach = ts2_reach$Reach)%>%
  mutate(PointID = as.double(PointID))

# manual intervention to get rid of PT 101 which was being problematic
ts2_txt = ts2_txt[2:nrow(ts2_txt),]



# -------------------


# join data frames ----

# this joins excel (digitized data) with metadata for ts1
joined_df1 = left_join(ts1_excel, metadata_excel, by='Reach')%>%
  filter(Reach == reach_ID)

# this joins excel (digitized data) with metadata for ts2
joined_df2 = left_join(ts2_excel, metadata_excel, by='Reach')%>%
  filter(Reach == reach_ID)

# this joins the previous file with txt file data 
joined_df3 = left_join(joined_df1, ts1_txt, by=c('Reach','PointID','TS_code'))
joined_df4 = left_join(joined_df2, ts2_txt, by=c('Reach','PointID','TS_code'))

# this joins both ts1 and ts2 data to complete the entire reach
# creates a unique ID and another unique ID without A's and P's
master_df = rbind(joined_df3, joined_df4)%>%
  mutate(uniqueID = paste0(Reach, PointID, Location, Cross.section, TS_code))%>%
  mutate(LRW = substr(uniqueID,8,8))%>%
  mutate(Type = substr(uniqueID,9,9))%>%
  mutate(Number = substr(uniqueID,10,10))%>%
  mutate(UID2 = paste0(Reach, LRW, Number, Cross.section, TS_code))%>%
  mutate(Elevation = as.double(str_remove_all(Elevation, ' ')))#%>%
  #filter(PointID != '147')%>%
  #filter(PointID != '148')

# -------


# separate A's and P's  ----

a_df = select(master_df, UID2, Cross.section, LRW, Type, Number, Elevation)%>%
  filter(Type == 'A')%>% 
  rename("Active" = "Type")%>% 
  rename("ElevationA" = "Elevation")
  #mutate(ElevationA = as.double(str_remove_all(ElevationA, ' '))) #removes weird spaces from text file and converts
  # the string to a double

p_df = select(master_df, UID2, Cross.section, LRW, Type, Number, Elevation)%>%
  filter(Type == 'P')%>% 
  rename("Permafrost" = "Type")%>% 
  rename("ElevationP" = "Elevation")
  #mutate(ElevationP = as.double(str_remove_all(ElevationP, ' '))) #removes weird spaces from text file and converts
  #the string to a double

# joins active layer df and permafrost layer df and creates a new column with elevation difference
alt_df = left_join(a_df, p_df, by=c('UID2','LRW', 'Number', 'Cross.section'))%>%
  mutate(ALT = (ElevationA-ElevationP))

saveRDS(master_df, paste0('outputs/munged_12/master_',reach_ID,'.rds'))
saveRDS(alt_df, paste0('outputs/munged_12/ALT_',reach_ID,'.rds'))



#}


#lapply(g12_files,read_function)

# --------


