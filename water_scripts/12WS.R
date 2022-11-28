#script for WS data analysis
#libraries and wd----
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(fuzzyjoin)
library(gmt)
library(rgdal)

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
  summarize(avgO=mean(normO),avgH=mean(normH))

#join averages with metadata
metadata_df = read_xlsx('Raw_water_data/12metadata.xlsx')%>%
  mutate(newtime = sapply(strsplit(as.character(Sample_Time), " "),"[",2))%>%
  mutate(newdate = paste(Date, newtime, sep = " "))%>%
  mutate(date_time=as.POSIXct(newdate,tz="US/Alaska"))%>%
  mutate(doy=as.numeric(strftime(date_time, format = "%j")))

our_df = left_join(WS1_avg_df,metadata_df,"Identifier_1")

#read in NEON data----
ground_df = read_xlsx('Raw_water_data/NEON_ground_111622.xlsx')%>%
  select('Latitude','Longitude','Elevation_mabsl','Sample_ID','Collection_Date','d2H','d18O')%>%
  mutate(date_time=as.POSIXct(Collection_Date,tz="US/Alaska"))%>%
  mutate(doy=as.numeric(strftime(date_time, format = "%j")))

precip_df = read_xlsx('Raw_water_data/NEON_precipitation.xlsx',sheet=2, skip=1)%>%
  select('Latitude','Longitude','Elevation_mabsl','Sample_ID','Collection_Date','d2H','d18O')%>%
  mutate(date_time=as.POSIXct(Collection_Date,tz="US/Alaska"))%>%
  mutate(doy=as.numeric(strftime(date_time, format = "%j")))

#filter for North Slope
ground_NS_df = ground_df%>%
  filter(between(Latitude,68,69))%>%
  mutate(Site_ID=case_when(Longitude >= -149.4~"TOOK",
                           Longitude < -149.4~"OKSR"))

precip_NS_df = precip_df%>%
  filter(between(Latitude,68,69))

#join by coordinates----

ground_latlon = SpatialPoints(cbind(ground_NS_df$Longitude, -ground_NS_df$Latitude), proj4string=CRS("+proj=longlat"))
ground_utm = spTransform(ground_coords, CRS("+init=epsg:32606"))

by_dist=geodist(our_df$Latitude, our_df$Longitude, precip_NS_df$Latitude, precip_NS_df$Longitude)

#yet to try----
#join by doy within 365, created weighted column (inverse of days_apart)
by_time=difference_inner_join(our_df,precip_NS_df,by='doy',max_dist=365,distance_col='days_apart')%>%
  mutate(weights=1/'doy')
#weighted.mean within summarize

#yet to do----
#calculate fraction resembling ground and precip for each sample for which a match is found
