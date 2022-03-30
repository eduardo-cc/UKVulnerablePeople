#Tesing changes for GitHub

library("tidyverse")
library("readxl")
library("writexl")


#Reading Data
MalesData<-read_excel("CensusEstimates.xlsx",sheet="MYE2 - Males",range="A5:CQ435")
FemalesData<-read_excel("CensusEstimates.xlsx",sheet="MYE2 - Females",range="A5:CQ435")


SelectedAreas<-read_excel("administrative-annex--a.xlsx",sheet="Final Import")

#Adding new comments for testing
#Classifying data

MalesData<-MalesData %>% 
  mutate(Gender="Male") %>% 
  rename(Geography=Geography1)


FemalesData<-FemalesData %>% 
  mutate(Gender="Female")


#Put together the data

AllData<-bind_rows(MalesData,FemalesData)


#Remove Variables not required

AllData<- AllData %>% 
  select(-"Geography",-"All ages",-"Name")

#Unpivot Data

AllDataUnpivot<-gather(AllData,Ages,Population,-Code,-Gender) %>% 
  mutate(Ages=as.numeric(Ages),
         AgeRanges=trunc(Ages/5)*5) 


#Merge only relevant areas

AllDataRelevant<-inner_join(AllDataUnpivot,SelectedAreas,by=c("Code"="Area Codes"))




####### Loading health data

MalesDataHD<-read_excel("Health and Disability.xlsx",sheet="HE - Male at 65",range="A4:Q490")
FemalesDataHD<-read_excel("Health and Disability.xlsx",sheet="HE - Female at 65",range="A4:Q490")


MalesDataHD<- MalesDataHD %>% 
  mutate(Gender="Males")

FemalesDataHD<- FemalesDataHD %>% 
  mutate(Gender="Females")


AllHDData<-bind_rows(MalesDataHD,FemalesDataHD)

AllHDData<- AllHDData %>% 
  select(`Area Codes`, LE,HLE,DfLE ,Gender) %>% 
  group_by(`Area Codes`) %>% 
  summarise(LE=mean(LE,na.rm=TRUE),
            HLE=mean(HLE,na.rm=TRUE),
            DfLE=mean(DfLE,na.rm=TRUE)) %>% 
  ungroup()

#Clean two codes that were different on the mapping

AllHDData <- AllHDData %>% 
  mutate(`Area Codes`=case_when(
    `Area Codes`==  "S12000046" ~ "S12000049",
    `Area Codes`==  "S12000044" ~ "S12000050",
    TRUE ~ `Area Codes`
  ))

AllHDDataAreas <- inner_join(AllHDData,SelectedAreas,by=c("Area Codes"="Area Codes"))

##### Put together with the proportion of 65s



AllDataRelevantProportion<-AllDataRelevant %>% 
  mutate(Over65=ifelse(Ages>=65,"Yes","No"),
         Region_Authority=paste(Region,Authority)) %>% 
  group_by(Code,Over65) %>% 
  summarise(Population=sum(Population,na.rm=TRUE)) %>% 
  ungroup()

AllDataRelevantProportionS<-spread(AllDataRelevantProportion,Over65,Population) %>% 
  mutate(All=Yes + No,PropOver65=Yes/All) 




AllHDDataAreasProp<-left_join(AllHDDataAreas,AllDataRelevantProportionS,by=c("Area Codes"="Code"))

AllHDDataAreasProp<-AllHDDataAreasProp %>% 
  filter(LE>0) %>% 
  mutate(HDLE=(HLE+DfLE)/2)

AllHDDataAreasProp<-AllHDDataAreasProp %>% 
  mutate(Authority=str_replace_all(Authority, "[:digit:]", ""))

#Final data set for data visualisation
write_xlsx(AllHDDataAreasProp,"AllHDDataAreasProp.xlsx")





