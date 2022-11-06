
#script to read in WS data 

library(dplyr)
library(readxl)
library(tidyr)
library(stringr)



setwd('/Users/emmaboudreau/Documents/GitHub/igea22/')

#group3 raw water data
WS_1 = read_xlsx('Raw_water_data/3WS.1.xlsx')
WS_2= read_xlsx('Raw_water_data/3WS.2.xlsx')


#group 3 df

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








