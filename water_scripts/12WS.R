#script for WS data analysis
#libraries and wd----
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(fuzzyjoin)

setwd('/Users/Oskar/Documents/UMass/IGEA/igea22/')

#group12 raw water data reading-in and normalization----
WS1_df = read_xlsx('Raw_water_data/12WS.xlsx')

#select and rename columns
WS1_clean_df=WS1_df%>%
  select('Line', 'd(18_16)Mean', 'd(D_H)Mean', 'Ignore', 'Identifier_1', 'Identifier_2')%>%
  rename("meanO"="d(18_16)Mean")%>%
  rename("meanH"="d(D_H)Mean")%>%
  filter(Line >= 323)%>%
  filter(Line <= 580)%>%
  filter(Ignore == 0)

#create df for normalizing
WS1_stand_df=WS1_clean_df%>%
  filter(Identifier_2 == "standard")%>%
  mutate(standardO=case_when(Identifier_1 == "Picarro zero 1 6_23_22" | Identifier_1 == "Picarro zero 2 6_23_22" | Identifier_1 == "Picarro zero 3 6_23_22"~.3,
                             Identifier_1 == "Picarro mid 3 6_23_22" | Identifier_1 == "Picarro mid 2 6_23_22"| Identifier_1 == "Picarro mid 1 6_23_22"~-20.6,
                             Identifier_1 == "Picarro depl 3 6_23_22" | Identifier_1 == "Picarro depl 2 6_23_22"| Identifier_1 == "Picarro depl 1 6_23_22"~-29.6,))%>%
  mutate(standardH=case_when(Identifier_1 == "Picarro zero 1 6_23_22" | Identifier_1 == "Picarro zero 2 6_23_22" | Identifier_1 == "Picarro zero 3 6_23_22"~1.8,
                           Identifier_1 == "Picarro mid 3 6_23_22" | Identifier_1 == "Picarro mid 2 6_23_22"| Identifier_1 == "Picarro mid 1 6_23_22"~-159,
                           Identifier_1 == "Picarro depl 3 6_23_22" | Identifier_1 == "Picarro depl 2 6_23_22"| Identifier_1 == "Picarro depl 1 6_23_22"~-235,))

#linear regressions
modelO = lm(formula = standardO ~ meanO, data = WS1_stand_df)
modelH = lm(formula = standardH ~ meanH, data = WS1_stand_df)

#create normalized df
WS1_norm_df=WS1_clean_df%>%
  filter(Identifier_2=="sample")%>%
  mutate(normO=modelO$coefficients[2]*meanO+modelO$coefficients[1])%>%
  mutate(normH=modelH$coefficients[2]*meanH+modelH$coefficients[1])

#average isotope ratios and join with metadata----
WS1_avg_df = WS1_norm_df%>%
  group_by(Identifier_1)%>%
  summarize(avgO=mean(normO),avgH=mean(normH))%>%
  transmute(Label=Identifier_1,avgO=avgO,avgH=avgH)

#join averages with metadata
metadata_df = read_xlsx('Raw_water_data/12metadata.xlsx')%>%
  mutate(newtime = sapply(strsplit(as.character(Sample_Time), " "),"[",2))%>%
  mutate(newdate = paste(Date, newtime, sep = " "))%>%
  mutate(date_time=as.POSIXct(newdate,tz="US/Alaska"))

BCC = left_join(WS1_avg_df,metadata_df,"Label")

#join our data with NEON data----
#read in NEON data
ground_df = read_xlsx('Raw_water_data/NEON_ground.xlsx',sheet=2, skip=1)%>%
  select('Latitude','Longitude','Elevation_mabsl','Sample_ID','Collection_Date','d2H','d18O')%>%
  mutate(date_time=as.POSIXct(Collection_Date,tz="US/Alaska"))

precip_df = read_xlsx('Raw_water_data/NEON_precipitation.xlsx',sheet=2, skip=1)%>%
  select('Latitude','Longitude','Elevation_mabsl','Sample_ID','Collection_Date','d2H','d18O')%>%
  mutate(date_time=as.POSIXct(Collection_Date,tz="US/Alaska"))

#don't need: extract 4-letter site ID from Sample_ID in ground_df (could try grep instead of strsplit)----
Site_ID = strsplit(ground_df$Sample_ID, "_")%>%
    sapply("[",2)%>%
  strsplit("[.]")%>%
    sapply("[",1)

ground_df = ground_df%>%
  mutate(Site_ID = Site_ID)

#notes----
fprecip=(dground-dstream)/(dground-dprecip)

# need to:
# find the average or mean for normO each sample (4,5,6=INJ Nrb)
# find closest date to our sample in NEON data
# decide or try to NEON data closest to sample date over all data given or the closest year (2021)
# 
# G1:5/30-6/5
# G2: 6/8-6/16

# hydrogen standards:
# picarro zero d2h=1.8+/-.9
# picarro mid d2h=-159+/-1.3
# picarro depl d2h=-235+/-1.8
