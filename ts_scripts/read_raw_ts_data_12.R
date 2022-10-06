# script to read in basic data and look at it
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)

setwd('C:/Users/ncw02/Downloads/IGEA/')

# read in data ----------
# setwd (path to the data)

ts1_excel = read_xlsx('raw_ts_data/fd/fd12_ts1.xlsx')%>%
  mutate(TS_code  = "1")

ts2_excel = read_xlsx('raw_ts_data/fd/fd12_ts2.xlsx')%>%
  mutate(TS_code  = "2")

names(ts1_excel) <- make.names(names(ts1_excel), unique=TRUE)
names(ts2_excel) <- make.names(names(ts2_excel), unique=TRUE)


metadata_excel = read_xlsx('raw_ts_data/fd/fd12_metadata.xlsx')

# extract reach from txt file for ts1 (should be identical for ts2)
ts1_reach = read.delim('raw_ts_data/group12/ep7_ts1.txt',
                       skip = 13, header = FALSE, nrows= 1, dec = ".", sep = '\t')%>%
  transmute(Reach = strsplit(V1, " +")[[1]][3])

# extract reach from txt file for ts2 (should be identical for ts1)
ts2_reach = read.delim('raw_ts_data/group12/ep7_ts2.txt',
                        skip = 14, header = FALSE, nrows= 1, dec = ".", sep = '\t')%>%
  transmute(Reach = strsplit(V1, " +")[[1]][3])

# read in ts1 xyz
ts1_txt = read.delim("raw_ts_data/group12/ep7_ts1.txt",
                      skip = 19, header = TRUE, nrows= 113, dec = ".", sep = ',')%>%
  mutate(TS_code = "1")%>%
  mutate(Reach = ts1_reach$Reach)

# read in ts2 xyz
ts2_txt = read.delim("raw_ts_data/group12/ep7_ts2.txt",
                      skip = 20, header = TRUE, nrows= 122, dec = ".", sep = ',')%>%
  mutate(TS_code = "2")%>%
  mutate(Reach = ts2_reach$Reach)


# -------------------

# join data frames ----

# this joins excel (digitized data) with metadata for ts1
joined_df1 = left_join(ts1_excel, metadata_excel, by='Reach')%>%
  filter(Reach == 'E7')

# this joins excel (digitized data) with metadata for ts2
joined_df2 = left_join(ts2_excel, metadata_excel, by='Reach')%>%
  filter(Reach == 'E7')

# this joins the previous file with txt file data 
joined_df3 = left_join(joined_df1, ts1_txt, by=c('Reach','PointID','TS_code'))
joined_df4 = left_join(joined_df2, ts2_txt, by=c('Reach','PointID','TS_code'))

# this joins both ts1 and ts2 data to complete the entire reach
ep7 = rbind(joined_df3, joined_df4)%>%
  mutate(uniqueID = paste0(Reach, PointID, Location, Cross.section, TS_code))%>%
  mutate(LRW = substr(uniqueID,6,6))%>%
  mutate(Type = substr(uniqueID,7,7))%>%
  mutate(Number = substr(uniqueID,8,8))%>%
  mutate(UID2 = paste0(Reach, LRW, Number, Cross.section, TS_code))

#split the location
#ep7[c('LRW', 'AP', 'Number')] = str_split_fixed(ep7$Location, '', 3)
#mutate(ep7$UID2 = paste0(Reach, PointID, LRW, Number, Cross.section, TS_code))



# -------

# separate A's and P's  ----




a_df = select(ep7, UID2, Type, Elevation.y)%>%
  filter(Type =='A')%>% 
  rename("Active" = "Type")%>% 
  rename("ElevationA" = "Elevation.y")%>%
  mutate(ElevationA = as.double(str_remove_all(ElevationA, ' ')))


p_df = select(ep7, UID2, Type, Elevation.y)%>%
  filter(Type == 'P')%>% 
  rename("Permafrost" = "Type")%>% 
  rename("ElevationP" = "Elevation.y")%>%
  mutate(ElevationP = as.double(str_remove_all(ElevationP, ' ')))

final = left_join(a_df, p_df, by='UID2')%>%
  mutate(ALT = (ElevationA-ElevationP))

saveRDS(ep7, 'outputs/ep7.rds')
saveRDS(final, 'outputs/ALT.rds')


# --------










