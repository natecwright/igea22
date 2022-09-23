# script to read in basic data and look at it
library(dplyr)
library(readxl)

# read in data ----------
# setwd (path to the data)

total_station_1 = read_xlsx('C:/Users/ncw02/Downloads/Temp/fielddata1.xlsx',
                    sheet = 1)

metadata_in = read_xlsx('C:/Users/ncw02/Downloads/Temp/fielddata1.xlsx',
                        sheet = 3)

# -------------------


# test a join ----
joined_df = left_join(total_station_1, metadata_in, by='Reach')%>%
  #filter(Reach == 'E7')%>%
  select('Reach', 'Point ID', 'Elevation')

plot(joined_df$'Point ID', joined_df$Elevation)

# -------

