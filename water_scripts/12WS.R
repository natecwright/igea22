#script for WS data analysis
#libraries and wd----
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)

setwd('/Users/Oskar/Documents/UMass/IGEA/igea22/')

#group12 raw water data read-in----
WS_1 = read_xlsx('Raw_water_data/12WS.xlsx')

#create group12 control df and linear regression----
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

#beta coefficients for linear regression
lm( standardO ~ meanO, data = WS1_norm_df)

#create group12 normalized df----
WS1_final_df=WS_1%>%
  select('Line', 'Analysis', 'Inj Nr', 'd(18_16)Mean', 'd(D_H)Mean', 'Ignore', 'Identifier_1', 'Identifier_2')%>%
  rename("meanO"="d(18_16)Mean")%>% 
  filter(Ignore == 0)%>%
  filter(Identifier_2=="sample")%>%
  mutate(normO=1.0125*meanO-0.1211)


#precip group 1 5/30-6/5

fprecip=(dground-dstream)/(dground-dprecip)



#precip group 2 6/8-6/16


# 
# need to:
#   find the average or mean for normO each sample (4,5,6=INJ Nrb)
# find closest date to our sample in NEON data
# decide or try to NEON data closest to sample date over all data given or the closetst year (2021)
# 
# G1:5/30-6/5
# G2: 6/8-6/16
# G3: 8/17-8/26




# 
# hydrogen standards
# picarro zero d2h=1.8+/-.9
# picarro mid d2h=-159+/-1.3
# picarro depl dwh=-235+/-1.8


