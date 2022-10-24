# script to read in basic data and look at it
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(stringi)

setwd('C:/Users/ncw02/Downloads/IGEA/')

# read in data ----------
# setwd (path to the data)

ts1_excel = read_xlsx('raw_ts_data/fd/fd12_ts1.xlsx')%>%
  mutate(TS_code  = "1")%>% 
  rename("Notebook.notes" = "Notes")

ts2_excel = read_xlsx('raw_ts_data/fd/fd12_ts2.xlsx')%>%
  mutate(TS_code  = "2")%>% 
  rename("Notebook.notes" = "Notes")

# rename column names
names(ts1_excel) = make.names(names(ts1_excel), unique=TRUE)
names(ts2_excel) = make.names(names(ts2_excel), unique=TRUE)


metadata_excel = read_xlsx('raw_ts_data/fd/fd12_metadata.xlsx')%>%
  rename("Sample.elevation" = "Elevation")

# extract reach from txt file for ts1 (should be identical for ts2)
ts1_reach = read.delim('raw_ts_data/group12.n/ep7_ts1.txt',
                       header = FALSE, nrows= 1, dec = ".", sep = '\t')%>%
  transmute(Reach = V1)

# extract reach from txt file for ts2 (should be identical for ts1)
ts2_reach = read.delim('raw_ts_data/group12.n/ep7_ts2.txt',
                        header = FALSE, nrows= 1, dec = ".", sep = '\t')%>%
  transmute(Reach = V1)

# read in ts1 xyz
ts1_txt = read.delim("raw_ts_data/group12.n/ep7_ts1.txt",
                      skip = 1, header = TRUE, dec = ".", sep = ',')%>%
  mutate(TS_code = "1")%>%
  mutate(Reach = ts1_reach$Reach)%>%
  mutate(PointID = as.double(PointID))

# manual intervention to get rid of PT 101 which was being problematic
ts1_txt = ts1_txt[2:nrow(ts1_txt),]


# read in ts2 xyz
ts2_txt = read.delim("raw_ts_data/group12.n/ep7_ts2.txt",
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
  filter(Reach == 'EP7')

# this joins excel (digitized data) with metadata for ts2
joined_df2 = left_join(ts2_excel, metadata_excel, by='Reach')%>%
  filter(Reach == 'EP7')

# this joins the previous file with txt file data 
joined_df3 = left_join(joined_df1, ts1_txt, by=c('Reach','PointID','TS_code'))
joined_df4 = left_join(joined_df2, ts2_txt, by=c('Reach','PointID','TS_code'))

# this joins both ts1 and ts2 data to complete the entire reach
# creates a unique ID and another unique ID without A's and P's
ep7 = rbind(joined_df3, joined_df4)%>%
  mutate(uniqueID = paste0(Reach, PointID, Location, Cross.section, TS_code))%>%
  mutate(LRW = substr(uniqueID,7,7))%>%
  mutate(Type = substr(uniqueID,8,8))%>%
  mutate(Number = substr(uniqueID,9,9))%>%
  mutate(UID2 = paste0(Reach, LRW, Number, Cross.section, TS_code))%>%
  mutate(Elevation = as.double(str_remove_all(Elevation, ' ')))



# -------


# separate A's and P's  ----


a_df = select(ep7, UID2, Cross.section, LRW, Type, Number, Elevation)%>%
  filter(Type =='A')%>% 
  rename("Active" = "Type")%>% 
  rename("ElevationA" = "Elevation")
  #mutate(ElevationA = as.double(str_remove_all(ElevationA, ' '))) #removes weird spaces from text file and converts
  # the string to a double


p_df = select(ep7, UID2, Cross.section, LRW, Type, Number, Elevation)%>%
  filter(Type == 'P')%>% 
  rename("Permafrost" = "Type")%>% 
  rename("ElevationP" = "Elevation")
  #mutate(ElevationP = as.double(str_remove_all(ElevationP, ' '))) #removes weird spaces from text file and converts
# the string to a double


# joins active layer df and permafrost layer df and creates a new column with elevation difference
final = left_join(a_df, p_df, by=c('UID2','LRW', 'Number', 'Cross.section'))%>%
  mutate(ALT = (ElevationA-ElevationP))

saveRDS(ep7, 'outputs/ep7.rds')
saveRDS(final, 'outputs/ALT.rds')


# --------


# do some calcs ----
mean = mean(final$ALT)
water = final%>%
  filter(LRW =='W')

mean2 = mean(water$ALT)


# ----


# ----

new_df = final%>%
  filter(LRW == 'W')%>%
  mutate(Temp = paste0(LRW, Number))%>%
  filter(Temp != 'W2')%>%
  filter(Temp != 'W4')

newnew_df = new_df%>%
  filter(Cross.section !='3')%>%
  filter(Cross.section !='8')

temp_df = new_df%>%
  filter(Cross.section == '3' | Cross.section == '8')%>%
  filter(Number != '3')

ultimate_df = rbind(newnew_df, temp_df)

#----








