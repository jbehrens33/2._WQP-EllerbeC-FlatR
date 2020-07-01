## Clear environment
rm(list = ls())

## Libraries

library(dataRetrieval)
library(dplyr)
library(tidyverse)
library(openxlsx)
library(leaflet)
#library(reshape)

## Set Working Directory
setwd("C:/Users/jbehr/OneDrive - Duke University/Documents/5. R/2. WQP Ellerbe Flat")

############################## 
#### Download County Data ####
##############################

# Download the USGS PCodes
Parameter_Data <- parameterCdFile %>% 
  dplyr::rename(USGSPCode = parameter_cd)

tally(group_by(Parameter_Data, parameter_group_nm)) # Count of parameters, per group

# Download county-level data ("raw data")
Durham_WQP_Raw_Data <- readWQPdata(countycode="US:37:063")

colnames(Durham_WQP_Raw_Data)

Types<- Durham_WQP_Raw_Data %>% 
  group_by(ActivityMediaName) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  view()

# Connect to long/lat data
D_Sites<-whatWQPsites(countycode="US:37:063") %>% 
  select(MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName, LatitudeMeasure, 
         LongitudeMeasure, StateCode, CountyCode, HUCEightDigitCode)

Durham_WQP_LongLat<- left_join(Durham_WQP_Raw_Data, D_Sites, by = "MonitoringLocationIdentifier")

#write.csv(Durham_WQP_LongLat, file = "output/Durham-Long-Lat-Data.csv")

# Investigate the data
Biological<-Durham_WQP_LongLat %>% 
  group_by(ActivityMediaName) %>% 
  filter(ActivityMediaName %in% c("Biological Tissue", "Biological")) # %in% is equivalent to ==, but for a group

Water<- Durham_WQP_LongLat %>% 
  group_by(ActivityMediaName) %>% 
  filter(ActivityMediaName == "Water") 

Sediment_Air<-Durham_WQP_LongLat %>% 
  group_by(ActivityMediaName) %>% 
  filter(ActivityMediaName %in% c("Sediment", "Air"))

tally(group_by(Durham_WQP_LongLat, ActivityMediaName))
tally(group_by(Biological, ActivityMediaName))
tally(group_by(Water, ActivityMediaName))
tally(group_by(Sediment_Air, ActivityMediaName))

# Map it out
Bio_Map<-Biological %>% 
  group_by(MonitoringLocationIdentifier) %>% 
  mutate(Count = count(MonitoringLocationIdentifier))

Wat_Map<-Water

Sed_Air_Map<-Sediment_Air


Map_by_type<-leaflet() %>% 
  addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(data=Biological, ~LongitudeMeasure, ~LatitudeMeasure,
                   color = "red", radius=3, stroke=FALSE,
                   fillOpacity = 0.8, opacity = 0.8,
                   popup=~MonitoringLocationIdentifier) #%>% 
  addCircleMarkers(data=Wat_Map, ~LongitudeMeasure, ~LatitudeMeasure,
                   color = "green", radius=3, stroke=FALSE,
                   fillOpacity = 0.8, opacity = 0.8,
                   popup=~MonitoringLocationIdentifier) %>% 
  addCircleMarkers(data=Sed_Air_Map, ~LongitudeMeasure, ~LatitudeMeasure,
                 color = "yellow", radius=3, stroke=FALSE,
                 fillOpacity = 0.8, opacity = 0.8,
                 popup=~MonitoringLocationIdentifier)
Map_by_type




#Other things
Wake_Durham_County<-c("US:37:063", "US:37:183")

W_D_WQP_Sites <- whatWQPsites(countycode=Wake_Durham_County)%>% 
  group_by(MonitoringLocationTypeName)

Types<-W_D_WQP_Sites %>% 
  group_by(MonitoringLocationTypeName) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  view()

Samples<- whatWQPsamples(countycode=Wake_Durham_County)

# Map it out

D_Sites<-whatWQPsites(countycode="US:37:063")
R_Sites<-whatWQPsites(countycode="US:37:183")

Types<-W_D_WQP_Sites %>% 
  group_by(ActivityMediaName) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  view()

D_Types<-R_Sites %>% 
  group_by(MonitoringLocationTypeName) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  view()


map_2<-leaflet() %>% 
  addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(data=D_Sites, ~LongitudeMeasure, ~LatitudeMeasure,
                   color = "red", radius=3, stroke=FALSE,
                   fillOpacity = 0.8, opacity = 0.8,
                   popup=~MonitoringLocationIdentifier) %>% 
  addCircleMarkers(data=R_Sites, ~LongitudeMeasure, ~LatitudeMeasure,
                   color = "green", radius=3, stroke=FALSE,
                   fillOpacity = 0.8, opacity = 0.8,
                   popup=~MonitoringLocationIdentifier)
map_2

map<- leaflet(data=W_D_WQP_Sites) %>% 
  addProviderTiles("Esri.WorldTopoMap") %>%
  addCircleMarkers(~LongitudeMeasure, ~LatitudeMeasure,
                   color = "red", radius=3, stroke=FALSE,
                   fillOpacity = 0.8, opacity = 0.8,
                   popup=~MonitoringLocationIdentifier)

map

# You can make "layers" by separating the data, and then using the addCircleMarkers for each

