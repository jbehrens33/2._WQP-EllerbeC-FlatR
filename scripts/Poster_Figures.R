################# 
#### Set-Up  ####
#################

## Clear environment
#rm(list = ls())

## Libraries

library(dataRetrieval)
library(tidyverse)
library(openxlsx)
library(leaflet)                                           
library(reshape2)
library(MazamaSpatialUtils)

## Set Working Directories
setwd("C:/Users/jrb146/OneDrive - Duke University/Documents/5. R")

setSpatialDataDir("C:/Users/jrb146/OneDrive - Duke University/Documents/5. R/3. Mapping/data_raw/Data_HUCs")

####################### 
#### Download Data ####
#######################

#### Water Data ####

# Download county-level data ("raw data")
Upper_Neuse_Raw_Data<-read.csv("2. WQP Ellerbe Flat/output/WQP_Upper_Neuse_Data.csv") 

Upper_Neuse_Water_Raw<-Upper_Neuse_Raw_Data %>% 
  filter(ActivityMediaName == "Water")

# Add long/lat data
Upper_Neuse_Sites<-whatWQPsites(huc = "03020201") %>% 
  select(MonitoringLocationIdentifier, MonitoringLocationName, 
         MonitoringLocationTypeName, LatitudeMeasure, 
         LongitudeMeasure, StateCode, CountyCode, HUCEightDigitCode)

Upper_Neuse_Water<- left_join(Upper_Neuse_Water_Raw, Upper_Neuse_Sites, 
                              by = "MonitoringLocationIdentifier")

# Connect to parameter groups
Parameter_Data <- parameterCdFile %>% # Download USGS PCodes
  dplyr::rename(USGSPCode = parameter_cd)

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

# Add columns to data for plots (for 2014 data)
Both_Water_Plot_Data<-Both_Water %>% #Add average measure
  group_by(Year, MonitoringLocationIdentifier, USGSPCode) %>% 
  mutate(Average_Measure_YearLocationAnalyte=mean(ResultMeasureValue, na.rm=TRUE)) %>% 
  ungroup() %>% 
  select(Year, MonitoringLocationIdentifier, MonitoringLocationName, USGSPCode, Parameter_with_units, CharacteristicName, ActivityStartDate, ActivityStartTime.Time, Average_Measure_YearLocationAnalyte, ResultMeasureValue, parameter_units)

Both_Water_Plot_Data_2014<-Both_Water_Plot_Data %>% #Add daily flow (m3/sec)
  filter(USGSPCode == 30209) %>% #right_join below brings all data back
  rename(Stream_Flow_Value_m3persec=ResultMeasureValue) %>% 
  select(Year, MonitoringLocationIdentifier, MonitoringLocationName, ActivityStartDate, Stream_Flow_Value_m3persec) %>% 
  right_join(Both_Water_Plot_Data, 
             by= c("Year", "MonitoringLocationIdentifier", "MonitoringLocationName", "ActivityStartDate")) %>% #Brings back all the data
  filter(Year == 2014)

Param_Groups<-Parameter_Data %>%#Add parameter information
  select(USGSPCode, parameter_group_nm)
Both_Water_Plot_Data_2014<-left_join(Both_Water_Plot_Data_2014, Param_Groups)

Both_Water_Plot_Data_2014<-Both_Water_Plot_Data_2014 %>%#Add max value
  group_by(MonitoringLocationIdentifier, Parameter_with_units) %>% 
  mutate(Max_Param_Value = max(ResultMeasureValue)) %>% 
  ungroup()

Two_Sites_2014<-Both_Water_Plot_Data_2014 %>% #Select 2 sites for 2 years
  filter(str_detect(MonitoringLocationIdentifier, 
                    c("02085500|02086849"))) %>% 
  filter(Year == 2014)

Both_Water_Plot_Data_2014<-Both_Water_Plot_Data_2014 %>% #Convert to common g/L measure
  mutate(Measured_Value_gperL = case_when(str_detect(parameter_units, "mg/l") ~ Max_Param_Value/1000,
                                          parameter_units == "ug/l" ~ Max_Param_Value/1000000,
                                          parameter_units == "ng/l" ~ Max_Param_Value/1000000000)) %>%
  mutate(Measured_Value_ugperL = Measured_Value_gperL * 1000000) %>% 
  mutate(Measured_Value_ugperL_not_min = case_when(str_detect(parameter_units, "mg/l") ~ ResultMeasureValue/1000,
                                                  parameter_units == "ug/l" ~ ResultMeasureValue/1000000,
                                                  parameter_units == "ng/l" ~ ResultMeasureValue/1000000000))

 # filter(str_detect(parameter_group_nm , c("rganic", "Nutrient"))) #%>% 
 # mutate_all(~replace(., is.na(.), 0))


#### Inverts Data ####

# Download data
loadSpatialData("WBDHU8.RData") # Download from http://data.mazamascience.com/MazamaSpatialUtils/Spatial/

NC_Data<-read.csv("3. Mapping/output/NC_gauge-wwtp-stormwater_data.csv")
Upper_Neuse_Measures<- read.csv("3. Mapping/output/WQP-and-Inverts-Upper-Neuse_data.csv")
Upper_Neuse_Map_data<- read.csv("3. Mapping/output/All-map-data_Upper-Neuse.csv")

Inverts<-read.csv("3. Mapping/data_raw/Data_Biodata_NC/20200420.1152.InvertResults.csv")
Sites<-read.csv("3. Mapping/data_raw/Data_Biodata_NC/20200420.1152.SiteInfo.csv")

Inverts_Upper_Neuse_raw<-left_join(Inverts, Sites, by = "SiteNumber")

Inverts_Upper_Neuse_raw<- Inverts_Upper_Neuse_raw%>% 
  mutate(HUC8_FUll_Name = getHUCName(as.numeric(Inverts_Upper_Neuse_raw$Longitude_dd), as.numeric(Inverts_Upper_Neuse_raw$Latitude_dd), dataset = "WBDHU8")) %>% 
  mutate(HUC8_Number = getHUC(as.numeric(Inverts_Upper_Neuse_raw$Longitude_dd), as.numeric(Inverts_Upper_Neuse_raw$Latitude_dd), dataset = "WBDHU8"))%>% 
  filter(HUC8_Number == "03020201")

colnames(Inverts_Upper_Neuse_raw)[1]<-"SIDNO" # Need to rename, some glitch with how the first column's name comes in

Inverts_Upper_Neuse<- Inverts_Upper_Neuse_raw %>% 
  filter(FieldComponent == "M") %>% 
  rename(Field_Site = SiteName.x)

########################## 
#### Analysis of Data ####
##########################
setwd("C:/Users/jrb146/OneDrive - Duke University/Documents/5. R")

#### Water ####
Two_Sites_2014_Small<- read.csv(file="2. WQP Ellerbe Flat/output/labeled-analytes.csv")

Two_Sites<-Two_Sites_2014_Small %>%
  group_by(CharacteristicName) %>% 
  ggplot(aes(x=reorder(str_wrap(CharacteristicName, 40), Measured_Value_ugperL), y=Measured_Value_ugperL, fill=Analyte_Type)) +
  geom_bar(stat="identity", position="dodge") +
  # scale_y_continuous(trans='log10') +
  coord_flip() +
  #geom_text(aes(label = ..count..), stat="count") +
  theme_classic() + 
  labs(title="Ellerbe Creek (Downstream), Top 20 Organic Analytes", y="Max Measured Value (ug/L)", x=NULL)
print(Two_Sites)
ggsave("2. WQP Ellerbe Flat/output/two-sites.png")

#### Inverts ####

# Data grouped by Order, genus specific

Orders_Plot<-Inverts_Upper_Neuse %>%  
  filter(str_detect(Field_Site, 
                    c("DAM|ELLERBE"))) %>%
  mutate(Field_Site=recode(Field_Site, 
                           "ELLERBE CREEK NEAR GORMAN, NC"="Ellerbe Creek (Downstream)",
                           "FLAT RIVER AT DAM NEAR BAHAMA, NC"="Flat River (Drinking Water Intake)")) %>% 
  ggplot(aes(x=reorder(str_wrap(Family, 30), -Abundance), y =Abundance, fill = Field_Site)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(~Order, scales = "free_x") +
  theme_classic() +
  theme(legend.position="bottom", axis.text.x = element_text(angle=45, hjust = 1)) + 
  labs(title="Aquatic Invertebrates, by Family", y="Abundance (count)", x= "Family")
print(Orders_Plot)

Orders_Plot_Insect<-Inverts_Upper_Neuse %>%  
  filter(str_detect(Field_Site, 
                    c("DAM|ELLERBE"))) %>%
  filter(Order == "Ephemeroptera" | Order == "Trichoptera" | Order == "Diptera") %>% 
  mutate(Field_Site=recode(Field_Site, 
                    "ELLERBE CREEK NEAR GORMAN, NC"="Ellerbe Creek (Downstream)",
                    "FLAT RIVER AT DAM NEAR BAHAMA, NC"="Flat River (Drinking Water Intake)")) %>% 
  ggplot(aes(x=reorder(str_wrap(Family, 30), -Abundance), y =Abundance, fill = Field_Site)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(~Order, scales = "free_x") +
  theme_classic() +
  theme(legend.position="bottom", axis.text.x = element_text(angle=45, hjust = 1)) + 
  labs(title="Emerging Aquatic Invertebrates, by Family", y="Abundance (count)", x= "Family")
print(Orders_Plot_Insect)
ggsave("2. WQP Ellerbe Flat/output/Family_Aq-Insects.png")

Mayflies_Plot<-Inverts_Upper_Neuse %>%
  filter(str_detect(Field_Site, 
                    c("DAM|ELLERBE"))) %>% 
  filter(Order == "Ephemeroptera") %>%  
  ggplot(aes(x=reorder(Family, -Abundance), y =Abundance, fill = Field_Site)) +
  geom_bar(stat="identity", position = "dodge") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
  labs(title="Upper Neuse: Mayflies, by Family and Site (June 2014)", y="Abundance (count)", x= "Family")
print(Mayflies_Plot)
#ggsave("2. WQP Ellerbe Flat/output/Mayflies_EC-FR.png")

Caddisflies_Plot<-Inverts_Upper_Neuse %>% 
  filter(str_detect(Field_Site, 
                    c("DAM|ELLERBE"))) %>% 
  filter(Order == "Trichoptera") %>%  
  ggplot(aes(x=reorder(Genus, -Abundance), y =Abundance, fill = Field_Site)) +
  geom_bar(stat="identity", position = "dodge") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  labs(title="Upper Neuse: Caddisflies, by Genus and Site (June 2014)", y="Abundance (count)", x= "Genus")
print(Caddisflies_Plot)
#ggsave("2. WQP Ellerbe Flat/output/Caddisflies_EC-FR.png")

Flies_Plot<-Inverts_Upper_Neuse %>% 
  filter(str_detect(Field_Site, 
                    c("DAM|ELLERBE"))) %>% 
  filter(Order == "Diptera") %>%  
  ggplot(aes(x=reorder(Family, -Abundance), y =Abundance, fill = Field_Site)) +
  geom_bar(stat="identity", position = "dodge") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  labs(title="Upper Neuse: True Flies, by Family and Site (June 2014)", y="Abundance (count)", x= "Family")
print(Flies_Plot)
#ggsave("2. WQP Ellerbe Flat/output/True-Flies_EC-FR.png")

###############
### Back-Up ###
###############

Stream_Flow<-Both_Water_Plot_Data_2014 %>% 
  filter(str_detect(MonitoringLocationName, 
                    c("DAM|ELLERBE"))) %>% 
  filter(USGSPCode == 30209) %>% #m^3/sec
  filter(!is.na(ResultMeasureValue)) %>% 
  ggplot(aes(x=MonitoringLocationName, y=ResultMeasureValue)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1)) + 
  labs(title="Stream Flow, Instaneous") +
  aes(str_wrap(MonitoringLocationName, 10), ResultMeasureValue) + xlab(NULL) +
  ylab("Measured Value (m^3/sec)")
print(Stream_Flow)
ggsave("2. WQP Ellerbe Flat/output/Stream_Flow.png")

Top_7_flow<-Both_Water_Plot_Data_2014 %>% 
  filter(str_detect(CharacteristicName, "Glyphosate|Fexo|Tolyl|Atrazine|Methocarb|Desmethyl|Carbend")) %>%
  filter(str_detect(MonitoringLocationName, "DAM|ELLERBE")) %>% 
  filter(!is.na(ResultMeasureValue)) %>% 
  ggplot(aes(x= Stream_Flow_Value_m3persec, y=Measured_Value_ugperL_not_min, color=CharacteristicName)) +
  geom_point() +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1), legend.text=element_text(size=7)) +
  labs(title="Various Nitrogen Components for Ellerbe Creek Near Gorman", y="Measured Value (ug/L)")
print(Top_7_flow)
ggsave("2. WQP Ellerbe Flat/output/Top_7_flow.png")

Glyphosphate_flow<-Both_Water_Plot_Data_2014 %>% 
  filter(str_detect(CharacteristicName, "Glyphosate")) %>%
  filter(str_detect(MonitoringLocationName, "DAM|ELLERBE")) %>% 
  filter(!is.na(ResultMeasureValue)) %>% 
  ggplot(aes(x= Stream_Flow_Value_m3persec, y=Measured_Value_ugperL_not_min, color=CharacteristicName)) +
  geom_point() +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1), legend.text=element_text(size=7)) +
  labs(title="Various Nitrogen Components for Ellerbe Creek Near Gorman", y="Measured Value (ug/L)")
print(Glyphosphate_flow)
ggsave("2. WQP Ellerbe Flat/output/Glyphosphate_flow.png")
