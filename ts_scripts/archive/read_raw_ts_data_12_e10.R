# set up ----
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)

# set working directory
setwd('C:/Users/Oskar/Documents/UMass/IGEA/igea22')


# read in data ----

ts1_excel = read_xlsx('raw_ts_data/fd/fd12_ts1.xlsx')%>%
  mutate(TS_code  = "1")

ts2_excel = read_xlsx('raw_ts_data/fd/fd12_ts2.xlsx')%>%
  mutate(TS_code  = "2")

# rename column names to have periods instead of spaces
names(ts1_excel) = make.names(names(ts1_excel), unique=TRUE)
names(ts2_excel) = make.names(names(ts2_excel), unique=TRUE)

metadata_excel = read_xlsx('raw_ts_data/fd/fd12_metadata.xlsx')

# extract reach from .txt file for ts1 and ts2
# keeps only E10 and assigns it to Reach
ts1_reach = read.delim('raw_ts_data/group12/ep10_ts1.txt',
                       skip = 13, header = FALSE, nrows= 1, dec = ".", sep = '\t')%>%
  transmute(Reach = strsplit(V1, " +")[[1]][3])

ts2_reach = read.delim('raw_ts_data/group12/ep10_ts2.txt',
                        skip = 13, header = FALSE, nrows= 1, dec = ".", sep = '\t')%>%
  transmute(Reach = strsplit(V1, " +")[[1]][3])

# read in ts1 and ts2 points and create 2 new columns
ts1_txt = read.delim("raw_ts_data/group12/ep10_ts1.txt",
                      skip = 19, header = TRUE, nrows= 82, dec = ".", sep = ',')%>%
  mutate(TS_code = "1")%>%
  mutate(Reach = ts1_reach$Reach)%>%
  mutate(PointID = as.double(PointID))

ts2_txt = read.delim("raw_ts_data/group12/ep10_ts2.txt",
                      skip = 19, header = TRUE, nrows= 113, dec = ".", sep = ',')%>%
  mutate(TS_code = "2")%>%
  mutate(Reach = ts2_reach$Reach)%>%
  mutate(PointID = as.double(PointID))

ts1_txt = ts1_txt[2:nrow(ts1_txt),]

# join data frames ----

# this joins excel (digitized data) with metadata for ts1
joined_df1 = left_join(ts1_excel, metadata_excel, by='Reach')%>%
  filter(Reach == 'E10')

# this joins excel (digitized data) with metadata for ts2
joined_df2 = left_join(ts2_excel, metadata_excel, by='Reach')%>%
  filter(Reach == 'E10')

# this joins the previous file with txt file data 
joined_df3 = left_join(joined_df1, ts1_txt, by=c('Reach','PointID','TS_code'))
joined_df4 = left_join(joined_df2, ts2_txt, by=c('Reach','PointID','TS_code'))

# this joins both ts1 and ts2 data to complete the entire reach
# creates a unique ID and another unique ID without A's and P's
ep10 = rbind(joined_df3, joined_df4)%>%
  mutate(uniqueID = paste0(Reach, PointID, Location, Cross.section, TS_code))%>%
  mutate(LRW = substr(uniqueID,6,6))%>%
  mutate(Type = substr(uniqueID,7,7))%>%
  mutate(Number = substr(uniqueID,8,8))%>%
  mutate(UID2 = paste0(Reach, LRW, Number, Cross.section, TS_code))%>%
  mutate(Elevation.y = as.double(str_remove_all(Elevation.y, ' ')))



# -------


# separate A's and P's  ----


a_df = select(ep7, UID2, Type, Elevation.y)%>%
  filter(Type =='A')%>% 
  rename("Active" = "Type")%>% 
  rename("ElevationA" = "Elevation.y")%>%
  #mutate(ElevationA = as.double(str_remove_all(ElevationA, ' '))) #removes weird spaces from text file and converts
  # the string to a double


p_df = select(ep7, UID2, Type, Elevation.y)%>%
  filter(Type == 'P')%>% 
  rename("Permafrost" = "Type")%>% 
  rename("ElevationP" = "Elevation.y")%>%
  #mutate(ElevationP = as.double(str_remove_all(ElevationP, ' '))) #removes weird spaces from text file and converts
# the string to a double


# joins active layer df and permafrost layer df and creates a new column with elevation difference
final = left_join(a_df, p_df, by='UID2')%>%
  mutate(ALT = (ElevationA-ElevationP))

saveRDS(ep7, 'outputs/ep7.rds')
saveRDS(final, 'outputs/ALT.rds')


# --------










