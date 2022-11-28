library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(pracma)
data_in=readRDS('general_df_12_2_v8.rds')
#cleanup----


#This dataframe is of points that aren't water, line or ground. Most should
#be GNSS or discard but it may be usful to take a look
nonWLG<-filter(data_in,!(WLG=="W"|WLG=="L"|WLG=="G"))%>%
  select(PointID_specific,WLG)
check<-data_in%>%
  ungroup()%>%
  mutate(point2=str_sub(PointID_specific,2,nchar(PointID_specific)-4))%>%
  #filter(is.na(Section))%>%
  select(point2)%>%
  distinct()
check2<-data_in%>%
  mutate(point2=str_sub(PointID_specific,2,nchar(PointID_specific)-4))%>%
  filter(point2=="060121")%>%
  select(-Top_temp,-Bottom_temp)%>%
  distinct()

clean_df<-data_in%>%
  ungroup()%>%
  filter(!is.na(XS))%>%
  mutate(
  #creates column called "reachid" that takes the 2nd-6th numbers in the
  #total station ID (streamid + reachid)
  reachid=str_sub(PointID_specific,2,nchar(PointID_specific)-6),
#standardize location id so each has three characters by adding 0s before
  Location=str_pad(Location, 3, pad = "0"),
#standardize cross section ids so that any cross sections with less than one digit have a 0 added to beginning
  XS=str_pad(XS, 2, pad = "0"),
#new id combines stream, reach, cross section, and location id into one string
  newid=str_c(reachid,XS,Location),
#creates unique cross section IDs by binding reach and cross section ids
  reach_xsec=str_c(reachid,XS),
  newid=as.factor(newid),
  Elevation=as.numeric(Elevation),
  SectionType=tolower(RiverType))%>%
  group_by(newid)%>%
  select(-Top_temp,-Bottom_temp,-mean_Q,-mean_v)%>% #don't know if we should keep this but doing it now so data works
  ungroup()%>%
  filter(!is.na(SP))%>%
  #mutate(SP=case_when(
   # PointID_specific=="T030216-232"~"P",
    #PointID_specific=="T010214-250"~"S",
    #TRUE~SP
 # ))%>%
  distinct()%>%
  select(-RiverType,-mean_d,-width,-Date_WQ)%>%
  mutate(Location=as.numeric(Location),
         Location=case_when(
           PointID_specific=="T120225-415"~-1,
           PointID_specific=="T090123-130"~-1,
           PointID_specific=="T090123-131"~-1,
           TRUE~`Location`),
         Location=case_when(
    Stream=="12"&Reach=="2"&XS=="17"~Location+1,
    Stream=="09"&Reach=="1"&XS=="03"~Location+1,
    TRUE~`Location`),
  Location=(as.character(Location)),
  Location=str_pad(Location, 3, pad = "0"))

  


wide_df= select(clean_df, SP,Elevation,newid)%>%
  mutate(SP= ifelse(SP==1,'P', ifelse(SP==2,'P',SP)))%>%# ifelse(SP==3,'P',SP))))%>%
  filter(SP == 'P' | SP == "S")%>%
  ungroup()%>%
  distinct()

#delete below once data is clean----
wide_df3=wide_df%>%#spread(wide_df, SP,Elevation)%>%
  group_by(newid)%>%
  summarize(numS=sum(SP=="S"), numP=sum(SP=="P"))%>%
  filter(numS ==1 & numP<2 )%>%
  select(newid)%>%
  left_join(wide_df,by='newid')%>%
  ungroup()


#PSwrong is a df of all the wird PS points
PSwrong=wide_df%>%
  group_by(newid)%>%
  summarize(numS=sum(SP=="S"), numP=sum(SP=="P"))%>%
  filter(numS >1 | numP>1|(numS ==0 & numP>0) )%>%
  select(newid)%>%
  left_join(wide_df,by='newid')%>%
  left_join(select(clean_df,newid),by="newid")%>%
  #select(-newid)%>%
  ungroup()%>%
  distinct()

  
wide_df4=wide_df3%>%
  spread(SP,Elevation)%>%
  #filter(!is.na(P) & !is.na(S) & P>0 & S>0) %>%
  right_join(clean_df,by='newid')%>%
  mutate(ALT=S-P)%>%
  filter(SP=="S")%>%
  #filter(!is.na(S))%>%
  select(-PointID_specific,-SP)%>% 
  mutate(Elevation_surf=Elevation)%>%
  select(-Elevation)%>%
  distinct()%>%
  ungroup()
#Finding points with neg AL
negAL=wide_df4%>%
  select(newid,ALT)%>%
  filter(ALT<0)%>%
  left_join(select(clean_df,newid,PointID_specific),by="newid")%>%
  select(-ALT,-newid)%>%
  ungroup()%>%
  distinct()

wide_df5<-wide_df4%>%
  filter(!ALT<0|is.na(ALT))%>%
  #filter(reachid != "0901"&reachid != "0902"&reachid != "0903")%>%
  ungroup()%>%
  filter(!SectionType=="")%>%
  filter((newid!="040110002"))
 # group_by(newid)%>%
 # mutate(n=sum(!is.na(newid)))%>%
  #filter(n==1)

#----
#Generating a new dataframe for calculating ditances of points along each cross section
#grouping the dataframe by cross section
Dist_df<-wide_df5%>%
  #select(North, East, reach_xsec,Location,newid,Elevation_surf,WLG,RiverType,Section,ALT)%>%
  group_by(reach_xsec)%>%
  mutate(East1=East[Location==min(Location)],North1=North[Location==min(Location)])%>%
  mutate(loc=sqrt((North-North1)^2+(East-East1)^2))%>%
  #merge(ALtabl,by="newid",all.x = T)%>%
  select(-North1,-East1)%>%
  ungroup()

#creates a dataframe with the average, max, and weighted average of active layer depth
#across each cross section
AL_df<-Dist_df%>%
  group_by(reach_xsec)%>%
  mutate(Location=as.numeric(Location),
         mean=mean(ALT[!is.na(ALT)]),#average of active layer thickness across entire cross section
          max=max(ALT[!is.na(ALT)]),#max ALT over entire cross section
          #vol=trapz(loc, ALT[!is.na(ALT)]),#integrates the total volume of active layer by trapezoids
          #weight_avg=vol/max(loc),#divides volume by total width to get a weighted average of active layer depth
          gmean=mean(ALT[WLG=="G"],na.rm=T),#average of active layer thickness across ground pts
          gmax=max(ALT[WLG=="G"],na.rm=T),#max active layer thickness across ground pts
          wmean=mean(ALT[WLG=="L"|WLG=="W"],na.rm=T),#average of active layer thickness across water pts
          wmax=max(ALT[WLG=="L"|WLG=="W"],na.rm=T),#max active layer thickness across water pts
         # wvol=trapz(loc[WLG=="L"|WLG=="W"], (ALT[!is.na(ALT)])[WLG=="L"|WLG=="W"]),#integrates the total volume of active layer under the water by trapezoids
          waterlev=mean(Elevation_surf[WLG=="L"],na.rm=T),#average elevation of L pts is the water line
          wEast=mean(East[WLG=="L"],na.rm=T),#average easting of L pts is the center easting
          wNorth=mean(North[WLG=="L"],na.rm=T),#average north of L pts is the center north
          lpts=sum(WLG=="L"),
         width=ifelse((lpts<2),NA,sqrt((East[loc==max(loc[WLG=="L"])]-East[loc==min(loc[WLG=="L"])])^2+
                    (North[loc==max(loc[WLG=="L"])]-North[loc==min(loc[WLG=="L"])])^2)))%>%
  select(-lpts)
         #width=max(loc[WLG=="L"])-min(loc[WLG=="L"]))
         #the above code finds the distance between the most extreme L measurements to find total width
         #wweight_avg=wvol/width)%>%#divides volume by total width to get a weighted average of active layer depth under water
     #select(-vol,-wvol)
  AL_df[AL_df==-Inf|AL_df==Inf|AL_df=="NaN"]<-NA #replaces all infinity and NaN values with NA
  AL_df$width[AL_df$width==0]<-NA #widths of 0 are places where there was only one L, so it should be NA
  #AL_df$wweight_avg[AL_df$wweight_avg==0]<-NA


#the code below creates a new column that takes the difference in the water line elevation
#between each cross section and the one before it, resetting the first cross section
#in each reach to 0
  wat_df=AL_df%>%
  group_by(reachid)%>%
    select(waterlev,wEast,wNorth,reach_xsec,reachid)%>%
    filter(!is.na(waterlev))%>%
    distinct()%>%
    mutate(elev_diff=c(0,tail(c(waterlev)-head(c(0,waterlev),-1),-1)),#takes the difference in elevation between the water level at one XS and the water level at the XS before
         east_diff=c(0,tail(c(wEast)-head(c(0,wEast),-1),-1)),#same as above but with easting
         north_diff=c(0,tail(c(wNorth)-head(c(0,wNorth),-1),-1)),#same as above but with northing
         dist=sqrt(east_diff^2+north_diff^2),#distance formula between center of XSs
         #the two lines below take the cumulative distance and elevation changes for each cross section
         cumdist=cumsum(dist),
         cumelev=cumsum(elev_diff))

#MASTER DF IS THE DF TO USE (CLEAN WITH POINTS FILTERED OUT)  
master_df<-right_join(select(wat_df,-waterlev,-wEast,-wNorth),AL_df,by=c("reach_xsec","reachid"))%>%
  mutate(waterdepth=waterlev-Elevation_surf)
master_df$waterdepth[!(master_df$WLG=="W")]<-NA

temps<- read_excel("temps.xlsx")%>%
  mutate(reachid=str_sub(`ID Number`,2,nchar(`ID Number`)-2),
         TOP=`Top Temp`, BOTTOM=`Bottom Temp`)%>%
  select(reachid,TOP,BOTTOM,Date,Time)%>%
  pivot_longer(!c(reachid,Date,Time),names_to ="Top/Bottom",values_to = "temp (C)")
  

WQ2 <- read_excel("WQ2.xlsx")
WQ2clean<-WQ2%>%
  mutate(Stream=str_pad(Stream, 2, pad = "0"),
         Reach=str_pad(Reach, 2, pad = "0"),
         reachid=str_c(Stream,Reach),
         `Top/Bottom`=toupper(`Top/Bottom`))%>%
  select(-etrex,-Reach,-Stream)%>%
  filter(!is.na(`temp (C)`))%>%
  merge(filter(temps,`temp (C)`!="NaN"),by=c("reachid","Top/Bottom","temp (C)","Date","Time"),all=T)%>%
  select(-east,-north)%>%
  merge(adv%>%
          select(-ID),all=T)

adv <- read_excel("adv.xlsx")%>%
  select(ID,`Discharge (CFS)`)%>%
  distinct()%>%
  mutate(ID=str_sub(ID,2,nchar(ID)-2),reachid=ID)%>%
  filter(!is.na(`Discharge (CFS)`))

master_dfWQ<-master_df%>%
  mutate(reachid=str_pad(reachid, 4, pad = "0"))%>%
  #merge(adv,by="reachid",all.x = T)%>%
  merge(WQ2clean,by="reachid",all.x=T)
  
check<-data_in%>%
  select(PointID_specific)%>%
  distinct()

longdist<-master_df%>%
  select(reach_xsec,dist)%>%
  filter(dist>20)

check<-master_df%>%
  filter(reachid=="0403")

GISpts<-data_in%>%
  select(PointID_general,North,East)%>%
  mutate(PointID_general=str_sub(PointID_general,1,7),
         PointID_general=gsub("O","0",PointID_general))%>%
  distinct()%>%
  group_by(PointID_general)%>%
  mutate(n=sum(!is.na(PointID_general)),North=as.numeric(North),East=as.numeric(East))%>%
  mutate(North=mean(North,na.rm=T),East=mean(East,na.rm=T))%>%
  filter(n>5)%>%
  distinct()%>%
  select(-n)


check<-data_in%>%
  select(-Top_temp,-Bottom_temp,-mean_v,-mean_d,-mean_Q,-width)%>%
  distinct()
ch<-check%>%
  filter(!is.na(WLG))%>%
  count(PointID_general)%>%
  filter(n>5)

chec<-data_in%>%
  filter(Stream=="04")

ch2ch<-master_df%>%
  filter(!is.na(WLG))%>%
  count(reachid)%>%
  filter(n>5)
rr<-data_in%>%
  filter(PointID_general=="T010314")



