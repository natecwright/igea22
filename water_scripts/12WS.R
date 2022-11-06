#script for WS data analysis
#libraries and wd----
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)

setwd('/Users/Oskar/Documents/UMass/IGEA/igea22/')

#group12 raw water data read-in----
WS1_df = read_xlsx('Raw_water_data/12WS.xlsx') #does this df includes G1 and G2 data?

#normalize group12 data----
#select and rename columns
WS1_clean_df=WS1_df%>%
  select('Line', 'Analysis', 'Inj Nr', 'd(18_16)Mean', 'd(D_H)Mean', 'Ignore', 'Identifier_1', 'Identifier_2')%>%
  rename("meanO"="d(18_16)Mean")%>%
  rename("meanH"="d(D_H)Mean")%>%
  filter(Ignore == 0)

#create df for normalizing
WS1_norm_df=WS1_clean_df%>%
  filter(Identifier_2 == "standard")%>%
  mutate(standardO=case_when(Identifier_1 == "Picarro zero 1 6_23_22" | Identifier_1 == "Picarro zero 2 6_23_22" | Identifier_1 == "Picarro zero 3 6_23_22"~.3,
                             Identifier_1 == "Picarro mid 3 6_23_22" | Identifier_1 == "Picarro mid 2 6_23_22"| Identifier_1 == "Picarro mid 1 6_23_22"~-20.6,
                             Identifier_1 == "Picarro depl 3 6_23_22" | Identifier_1 == "Picarro depl 2 6_23_22"| Identifier_1 == "Picarro depl 1 6_23_22"~-29.6,))%>%
  mutate(standardH=case_when(Identifier_1 == "Picarro zero 1 6_23_22" | Identifier_1 == "Picarro zero 2 6_23_22" | Identifier_1 == "Picarro zero 3 6_23_22"~1.8,
                           Identifier_1 == "Picarro mid 3 6_23_22" | Identifier_1 == "Picarro mid 2 6_23_22"| Identifier_1 == "Picarro mid 1 6_23_22"~-159,
                           Identifier_1 == "Picarro depl 3 6_23_22" | Identifier_1 == "Picarro depl 2 6_23_22"| Identifier_1 == "Picarro depl 1 6_23_22"~-235,))

#linear regressions
modelO = lm(formula = standardO ~ meanO, data = WS1_norm_df)
modelH = lm(formula = standardH ~ meanH, data = WS1_norm_df)

#create normalized df----
WS1_final_df=WS1_clean_df%>%
  filter(Identifier_2=="sample")%>%
  mutate(normO=modelO$coefficients[2]*meanO+modelO$coefficients[1]) #is this the best way to get the dependent variables of lm? is that what we want?

#notes----
fprecip=(dground-dstream)/(dground-dprecip)

# need to:
# find the average or mean for normO each sample (4,5,6=INJ Nrb)
# find closest date to our sample in NEON data
# decide or try to NEON data closest to sample date over all data given or the closetst year (2021)
# 
# G1:5/30-6/5
# G2: 6/8-6/16

# hydrogen standards: (is the number after +/- the standard deviation? should we use this at any point?)
# picarro zero d2h=1.8+/-.9
# picarro mid d2h=-159+/-1.3
# picarro depl d2h=-235+/-1.8


