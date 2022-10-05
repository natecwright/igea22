# script to read in basic data and look at it
library(dplyr)
library(readxl)

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
ts1_reach = read.delim("raw_ts_data/group3/ep7_ts1.txt",
                       skip = 11, header = FALSE, nrows= 1, dec = ".", sep = '\t')%>%
  transmute(Reach = strsplit(V1, " +")[[1]][3])


ts2_reach = read.delim("raw_ts_data/group3/ep7_ts2.txt",
                        skip = 11, header = FALSE, nrows= 1, dec = ".", sep = '\t')%>%
  transmute(Reach = strsplit(V1, " +")[[1]][3])

ts1_txt = read.delim("raw_ts_data/group3/ep7_ts1.txt",
                      skip = 17, header = TRUE, nrows= 124, dec = ".", sep = ',')%>%
  mutate(TS_code = "1")%>%
  mutate(Reach = ts1_reach$Reach)

  
ts2_txt = read.delim("raw_ts_data/group3/ep7_ts2.txt",
                      skip = 17, header = TRUE, nrows= 111, dec = ".", sep = ',')%>%
  mutate(TS_code = "2")%>%
  mutate(Reach = ts2_reach$Reach)

ts_excel_df = rbind(ts1_excel,ts2_excel)

ts_txt_df = rbind(ts1_txt,ts2_txt)

#created data frame with excel and text file for specific reach
joined_excel_txt_df = left_join(ts_excel_df, ts_txt_df, by=c('Reach', 'TS_code', 'PointID')) 
  joined_excel_txt_df$uniqueID = paste(joined_excel_txt_df$Reach,joined_excel_txt_df$TS_code,joined_excel_txt_df$Location,joined_excel_txt_df$XSection) 
  joined_excel_txt_df$AP = substr(joined_excel_txt_df$uniqueID,9,9)
  #sub([9],"",joined_excel_txt_df$uniqueID)

#where emma left off at 9


# -------------------

elev_df = select('AP','uniqueID','Elevation') %>%
    filter(AP == 'P')
  

# test a join ----
#joined_df = left_join(ts1_excel, metadata_excel, by='Reach')%>%
  #filter(Reach == 'E7')%>%
  #select('Reach', 'Point ID', 'Elevation')

#combined_df = rbind(ts1_elev, ts2_elev)

#plot(joined_df$'Point ID', joined_df$Elevation)

# -------

