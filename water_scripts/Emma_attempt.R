#script for group 1 and 2 WS data analysis
#libraries and wd----
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(fuzzyjoin)
library(gmt)


#setwd('/Users/Oskar/Documents/UMass/IGEA/igea22/')
setwd('/Users/emmaboudreau/Documents/GitHub/igea22/')


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
metadata_df = read_xlsx('Raw_water_data/12metadata.xlsx')

our_df = left_join(WS1_avg_df,metadata_df,"Identifier_1")

our3=readRDS('water_scripts/our3.rds')
ours=rbind(our_df,our3)









