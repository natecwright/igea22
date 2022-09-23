# script to read in basic data and look at it
library(dplyr)
library(readxl)

# read in data ----------
# setwd (path to the data)

total_station_1 = read_xlsx('C:/Users/ncw02/Downloads/Temp/fielddata2.xlsx',
                    sheet = 1)


total_station_2 = read_xlsx('C:/Users/ncw02/Downloads/Temp/fielddata2.xlsx',
                            sheet = 2)

metadata_in = read_xlsx('C:/Users/ncw02/Downloads/Temp/fielddata2.xlsx',
                        sheet = 3)

ts_number1 = read.delim("C:/Users/ncw02/Downloads/Temp/ts1.txt",
                       skip = 11, header = FALSE, nrows= 1, dec = ".", sep = '\t')%>%
  transmute(Reach = strsplit(V1, " +")[[1]][3])


ts_number2 = read.delim("C:/Users/ncw02/Downloads/Temp/ts2.txt",
                        skip = 11, header = FALSE, nrows= 1, dec = ".", sep = '\t')%>%
  transmute(Reach = strsplit(V1, " +")[[1]][3])

ts1_elev = read.delim("C:/Users/ncw02/Downloads/Temp/ts1.txt",
                      skip = 17, header = TRUE, nrows= 70, dec = ".", sep = ',')%>%
  mutate(TS_code = "TS1")%>%
  mutate(Reach = ts_number1$Reach)

  
ts2_elev = read.delim("C:/Users/ncw02/Downloads/Temp/ts2.txt",
                      skip = 17, header = TRUE, nrows= 70, dec = ".", sep = ',')%>%
  mutate(TS_code = "TS2")%>%
  mutate(Reach = ts_number2$Reach)


# -------------------


# test a join ----
joined_df = left_join(total_station_1, metadata_in, by='Reach')%>%
  filter(Reach == 'E7')%>%
  select('Reach', 'Point ID', 'Elevation')

combined_df = rbind(ts1_elev, ts2_elev)

#plot(joined_df$'Point ID', joined_df$Elevation)

# -------

