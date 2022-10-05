# script to read in basic data and look at it
library(dplyr)
library(readxl)

setwd('C:/Users/ncw02/Downloads/IGEA/')

# read in data ----------
# setwd (path to the data)

ts1_excel = read_xlsx('raw_ts_data/fd/fd12_ts1.xlsx')%>%
  mutate(TS_code  = "1")#%>%
  #mutate('Cross section'= Xsection)

ts2_excel = read_xlsx('raw_ts_data/fd/fd12_ts2.xlsx')%>%
  mutate(TS_code  = "2")

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
combined_df = rbind(joined_df3, joined_df4)#%>%
  #mutate(UID == paste(Reach, PointID, Location, Xsection, TS_code)


# -------

