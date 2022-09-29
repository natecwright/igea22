# script to read in basic data and look at it
library(dplyr)
library(readxl)

setwd('/Users/emmaboudreau/Documents/GitHub/igea22/')

# read in data ----------
# setwd (path to the data)
#read in total station excel

ts1_excel = read_xlsx('raw_ts_data/fd/fd3_ts1.xlsx')%>%
  mutate(TS_code  = "TS1")


ts2_excel = read_xlsx('raw_ts_data/fd/fd3_ts2.xlsx')%>%
  mutate(TS_code  = "TS2")
#read in metadata
metadata_excel = read_xlsx('raw_ts_data/fd/fd3_metadata.xlsx')
#read in reach code
ts1_reach = read.delim("raw_ts_data/group3/ep7_ts1.txt",
                       skip = 11, header = FALSE, nrows= 1, dec = ".", sep = '\t')%>%
  transmute(Reach = strsplit(V1, " +")[[1]][3])


ts2_reach = read.delim("raw_ts_data/group3/ep7_ts2.txt",
                        skip = 11, header = FALSE, nrows= 1, dec = ".", sep = '\t')%>%
  transmute(Reach = strsplit(V1, " +")[[1]][3])

ts1_txt = read.delim("raw_ts_data/group3/ep7_ts1.txt",
                      skip = 17, header = TRUE, nrows= 70, dec = ".", sep = ',')%>%
  mutate(TS_code = "TS1")%>%
  mutate(Reach = ts1_reach$Reach)

  
ts2_txt = read.delim("raw_ts_data/group3/ep7_ts2.txt",
                      skip = 17, header = TRUE, nrows= 70, dec = ".", sep = ',')%>%
  mutate(TS_code = "TS2")%>%
  mutate(Reach = ts2_reach$Reach)

ts_excel_df = rbind(ts1_excel,ts2_excel)

#where emma left off at 9/29

# -------------------



# test a join ----
joined_df = left_join(ts1_excel, metadata_excel, by='Reach')%>%
  filter(Reach == 'E7')%>%
  select('Reach', 'Point ID', 'Elevation')

combined_df = rbind(ts1_elev, ts2_elev)

#plot(joined_df$'Point ID', joined_df$Elevation)

# -------

