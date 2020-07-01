################# 
#### Set-Up  ####
#################

## Clear environment
rm(list = ls())

## Libraries

library(dataRetrieval)
library(tidyverse)
library(openxlsx)
library(leaflet)                                           
library(reshape2)

## Set Working Directory
setwd("C:/Users/jbehr/OneDrive - Duke University/Documents/5. R/2. WQP Ellerbe Flat")

####################### 
#### Download Data ####
#######################

# Download county-level data ("raw data")

#Upper_Neuse_Raw_Data <- readWQPdata(huc = "03020201") # Takes awhile to load
#write.csv(Upper_Neuse_Raw_Data, "output/WQP_Upper_Neuse_Data.csv")

Upper_Neuse_Raw_Data<-read.csv("output/WQP_Upper_Neuse_Data.csv") 

Upper_Neuse_Water_Raw<-Upper_Neuse_Raw_Data %>% 
  filter(ActivityMediaName == "Water")

# Add long/lat data
Upper_Neuse_Sites<-whatWQPsites(huc = "03020201") %>% 
  select(MonitoringLocationIdentifier, MonitoringLocationName, 
         MonitoringLocationTypeName, LatitudeMeasure, 
         LongitudeMeasure, StateCode, CountyCode, HUCEightDigitCode)

Upper_Neuse_Water<-left_join(Upper_Neuse_Water_Raw, Upper_Neuse_Sites, 
                              by = "MonitoringLocationIdentifier")

# Download USGS PCodes
Parameter_Data <- parameterCdFile %>% 
  dplyr::rename(USGSPCode = parameter_cd)

# Connect to parameter groups
Upper_Neuse_Water$USGSPCode<- as.character(Upper_Neuse_Water$USGSPCode)
Upper_Neuse_Water$USGSPCode<-str_pad(Upper_Neuse_Water$USGSPCode, 5, pad="0") #Makes analyses easier later

Upper_Neuse_Water<-left_join(Upper_Neuse_Water, Parameter_Data, by="USGSPCode")

# Add in decade and year for samples
Upper_Neuse_Water<-Upper_Neuse_Water %>% 
  mutate(Year = as.numeric(format(as.Date(ActivityStartDate, 
                                          format = "%Y-%m-%d"), "%Y"))) %>% 
  mutate(Decade = Year - Year %% 10)

# Subsect data into Flat and Ellerbe River
Flat_River_Water<-Upper_Neuse_Water %>% 
  filter(str_detect(MonitoringLocationName, "FLAT")) %>% 
  mutate(N = "1") %>% 
  mutate(Site_Group = "Flat River")

Ellerbe_Creek_Water<-Upper_Neuse_Water %>% 
  filter(str_detect(MonitoringLocationName, "ELLERBE")) %>% 
  mutate(N = "1") %>% 
  mutate(Site_Group = "Ellerbe Creek")

# Merge into combined set
Both_Water<-full_join(Flat_River_Water, Ellerbe_Creek_Water)

Both_Water$Unique_YearSiteParameter_Code<-paste(Both_Water$Year, "_", 
                                                Both_Water$MonitoringLocationIdentifier, "_", 
                                                Both_Water$USGSPCode)
Both_Water$Parameter_with_units<-paste(Both_Water$USGSPCode, "_", 
                                                Both_Water$CharacteristicName, "(", 
                                                Both_Water$parameter_units, ")")

Ellerbe_Flat_Sites<-Both_Water %>% 
  group_by(MonitoringLocationIdentifier) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  left_join(Upper_Neuse_Sites)

# Export the data or do some simple tests
write.csv(Upper_Neuse_Raw_Data, "output/Data/WQP_Upper-Neuse_RAW.csv")
write.csv(Upper_Neuse_Water, "output/Data/WQP_Upper-Neuse_WATER.csv")
write.csv(Both_Water, "output/Data/WQP_FR-EC_WATER.csv")
write.csv(Flat_River_Water, "output/Data/WQP_FR_WATER.csv")
write.csv(Ellerbe_Creek_Water, "output/Data/WQP_EC_WATER.csv")
write.csv(Upper_Neuse_Sites, "output/Data/WQP_Upper-Neuse_SITES.csv")
write.csv(Ellerbe_Flat_Sites, "output/Data/WQP_FR-EC_SITES.csv")


#tally(group_by(Upper_Neuse_Water_Raw, ActivityMediaName)) # Just to validate
#tally(group_by(Parameter_Data, parameter_group_nm)) # Count of parameters, per group
#tally(group_by(Upper_Neuse_Water, Decade))