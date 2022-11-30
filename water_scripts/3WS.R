#script for group 3 WS data analysis
#libraries and wd----
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(fuzzyjoin)
library(gmt)
library(rgdal)
library(sf)

setwd('/Users/Oskar/Documents/UMass/IGEA/igea22/')

WS3_1 = read_xlsx('Raw_water_data/3WS.1.xlsx')
WS3_2= read_xlsx('Raw_water_data/3WS.2.xlsx')

#mine
#clean up----
WS3_clean=WS3_1%>%
  select('Line', 'd(18_16)Mean', 'd(D_H)Mean', 'Ignore', 'Identifier_1', 'Identifier_2')%>%
  rename("meanO"="d(18_16)Mean")%>%
  rename("meanH"="d(D_H)Mean")%>%
  filter(Line >= 160)%>%
  filter(Line <= 255)%>%
  filter(Ignore == 0)

#create df for normalizing
WS3_stand=WS3_clean%>%
  filter(Identifier_2 == "standard")%>%
  mutate(standardO=case_when(Identifier_1 == "Picarro zero 1 6_23_22" | Identifier_1 == "Picarro zero 2 6_23_22" | Identifier_1 == "Picarro zero 3 6_23_22"~.3,
                             Identifier_1 == "Picarro mid 3 6_23_22" | Identifier_1 == "Picarro mid 2 6_23_22"| Identifier_1 == "Picarro mid 1 6_23_22"~-20.6,
                             Identifier_1 == "Picarro depl 3 6_23_22" | Identifier_1 == "Picarro depl 2 6_23_22"| Identifier_1 == "Picarro depl 1 6_23_22"~-29.6,))%>%
  mutate(standardH=case_when(Identifier_1 == "Picarro zero 1 6_23_22" | Identifier_1 == "Picarro zero 2 6_23_22" | Identifier_1 == "Picarro zero 3 6_23_22"~1.8,
                             Identifier_1 == "Picarro mid 3 6_23_22" | Identifier_1 == "Picarro mid 2 6_23_22"| Identifier_1 == "Picarro mid 1 6_23_22"~-159,
                             Identifier_1 == "Picarro depl 3 6_23_22" | Identifier_1 == "Picarro depl 2 6_23_22"| Identifier_1 == "Picarro depl 1 6_23_22"~-235,))

#linear regressions
modelO = lm(formula = standardO ~ meanO, data = WS3_stand)
modelH = lm(formula = standardH ~ meanH, data = WS3_stand)

#create normalized df
WS3_norm=WS3_clean%>%
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
  mutate(Site_ID=case_when(Longitude < -149.4~"TOOK",
                           Longitude >= -149.4~"OKSR"))

precip_NS_df = precip_df%>%
  filter(between(Latitude,68,69))

#join by coordinates----
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  res=st_as_sf(res)
  return(data.frame(x=st_coordinates(res)[,1],y=st_coordinates(res)[,2]))}

ground_utm = ground_NS_df%>%
  mutate(LongLatToUTM(Longitude,Latitude,6))


precip_utm = precip_NS_df%>%
  mutate(LongLatToUTM(Longitude,Latitude,6))

by_time_precip=difference_left_join(our_df,precip_utm,by='doy',max_dist=365,distance_col='days_apart')

by_dist_group=distance_left_join(our_df,ground_utm,by=c("x","y"),max_dist=1000,distance_col='meters_apart')

#Emma's----
#creating data frame for standards for run1 in picarro analyzer
WS1_norm_df=WS_1%>%
  select('Line', 'Analysis', 'Inj Nr', 'd(18_16)Mean', 'd(D_H)Mean', 'Ignore', 'Identifier_1', 'Identifier_2')%>%
  rename("meanO"="d(18_16)Mean")%>% 
  filter(Identifier_1 == "Picarro zero 3 6_23_22" | Identifier_1 == "Picarro mid 3 6_23_22" | Identifier_1 == "Picarro depl 3 6_23_22"
         | Identifier_1 == "Picarro zero 2 6_23_22" | Identifier_1 == "Picarro mid 2 6_23_22" | Identifier_1 == "Picarro depl 2 6_23_22"
         | Identifier_1 == "Picarro zero 1 6_23_22" | Identifier_1 == "Picarro mid 1 6_23_22"| Identifier_1 == "Picarro depl 1 6_23_22")%>%
  filter(Ignore == 0)%>%
  mutate(standardO=case_when(Identifier_1 == "Picarro zero 1 6_23_22" | Identifier_1 == "Picarro zero 2 6_23_22" | Identifier_1 == "Picarro zero 3 6_23_22"~.3,
                            Identifier_1 == "Picarro mid 3 6_23_22" | Identifier_1 == "Picarro mid 2 6_23_22"| Identifier_1 == "Picarro mid 1 6_23_22"~-20.6,
                            Identifier_1 == "Picarro depl 3 6_23_22" | Identifier_1 == "Picarro depl 2 6_23_22"| Identifier_1 == "Picarro depl 1 6_23_22"~-29.6,))

#beta coefficient and intercept for linear regression
slope1 = lm( standardO ~ meanO, data = WS1_norm_df)$coefficients[2]
intercept1 = lm( standardO ~ meanO, data = WS1_norm_df)$coefficients[1]




#creating data frame for standards for run 2 in picarro analyzer

WS2_norm_df=WS_2%>%
select('Line', 'Analysis', 'Inj Nr', 'd(18_16)Mean', 'd(D_H)Mean', 'Ignore', 'Identifier_1', 'Identifier_2')%>%
  rename("meanO"="d(18_16)Mean")%>% 
  filter(Identifier_1 == "Picarro zero 3 6_23_22" | Identifier_1 == "Picarro mid 3 6_23_22" | Identifier_1 == "Picarro depl 3 6_23_22"
         | Identifier_1 == "Picarro zero 2 6_23_22" | Identifier_1 == "Picarro mid 2 6_23_22" | Identifier_1 == "Picarro depl 2 6_23_22"
         | Identifier_1 == "Picarro zero 1 6_23_22" | Identifier_1 == "Picarro mid 1 6_23_22"| Identifier_1 == "Picarro depl 1 6_23_22")%>%
  filter(Ignore == 0)%>%
  mutate(standardO=case_when(Identifier_1 == "Picarro zero 1 6_23_22" | Identifier_1 == "Picarro zero 2 6_23_22" | Identifier_1 == "Picarro zero 3 6_23_22"~.3,
                              Identifier_1 == "Picarro mid 3 6_23_22" | Identifier_1 == "Picarro mid 2 6_23_22"| Identifier_1 == "Picarro mid 1 6_23_22"~-20.6,
                              Identifier_1 == "Picarro depl 3 6_23_22" | Identifier_1 == "Picarro depl 2 6_23_22"| Identifier_1 == "Picarro depl 1 6_23_22"~-29.6,))


#beta coefficient and intercept for linear regression 
slope2 = lm( standardO ~ meanO, data = WS2_norm_df)$coefficients[2]
intercept2 = lm( standardO ~ meanO, data = WS2_norm_df)$coefficients[1]




WS1_final_df=WS_1%>%
  select('Line', 'Analysis', 'Inj Nr', 'd(18_16)Mean', 'd(D_H)Mean', 'Ignore', 'Identifier_1', 'Identifier_2')%>%
  rename("meanO"="d(18_16)Mean")%>% 
  filter(Ignore == 0)%>%
  filter(Identifier_2=="sample")%>%
  mutate(normO=slope1*meanO-intercept1)%>%
  group_by(Identifier_1)%>%
  summarize(normOmean = mean(normO))
 

WS2_final_df=WS_2%>%
  select('Line', 'Analysis', 'Inj Nr', 'd(18_16)Mean', 'd(D_H)Mean', 'Ignore', 'Identifier_1', 'Identifier_2')%>%
  rename("meanO"="d(18_16)Mean")%>% 
  filter(Ignore == 0)%>%
  filter(Identifier_2=="sample")%>%
  mutate(normO=slope2*meanO-intercept2)%>%
  group_by(Identifier_1)%>%
  summarize(normOmean = mean(normO))

# G3: 8/17-8/26

# hydrogen standards
# picarro zero d2h=1.8+/-.9
# picarro mid d2h=-159+/-1.3
# picarro depl dwh=-235+/-1.8








