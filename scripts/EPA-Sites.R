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

# Download data

Ellerbe_Gorman_Data<-readWQPdata(siteid= c("21NC02WQ-J1330000", "21NC03WQ-J1330000")) %>% # EPA site near Gorman
  filter(ActivityMediaName == "Water")

Flat_Dam_Data<-readWQPdata(siteid= c("21NC02WQ-J1100000", "21NC03WQ-J1100000")) %>% # EPA site near Dam
  filter(ActivityMediaName == "Water")

# Summary of Detects and Non-Detects
EC_Detect<-Ellerbe_Gorman_Data %>% 
  mutate(ResultDetectionConditionText=replace_na(ResultDetectionConditionText, "Detect")) %>% 
  group_by(MonitoringLocationIdentifier, CharacteristicName, ResultDetectionConditionText) %>% 
  mutate(Detect=n()) %>% 
  distinct_at(vars(CharacteristicName, ResultDetectionConditionText), .keep_all = TRUE) %>% 
  select(CharacteristicName, ResultDetectionConditionText, Detect, MonitoringLocationIdentifier) %>% 
  arrange(CharacteristicName) %>% 
  view()

FR_Detect<-Flat_Dam_Data %>% 
  mutate(ResultDetectionConditionText=replace_na(ResultDetectionConditionText, "Detect")) %>% 
  group_by(MonitoringLocationIdentifier, CharacteristicName, ResultDetectionConditionText) %>% 
  mutate(Detect=n()) %>% 
  distinct_at(vars(CharacteristicName, ResultDetectionConditionText), .keep_all = TRUE) %>% 
  select(CharacteristicName, ResultDetectionConditionText, Detect, MonitoringLocationIdentifier) %>% 
  arrange(CharacteristicName) %>% 
  view()

# Look at Lead
Pb<-data %>% 
  filter(str_detect(CharacteristicName, "Lead")) %>%
  filter(!is.na(ResultMeasureValue)) %>% 
  ggplot(aes(x= ActivityStartDate, y=ResultMeasureValue, color=CharacteristicName)) +
  geom_point() +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1), legend.text=element_text(size=7)) +
  labs(title="Lead for Ellerbe Creek Near Gorman (EPA Site)", y="Measured Value (ug/L)")
print(Pb)
#ggsave("2. WQP Ellerbe Flat/output/Lead.png")
