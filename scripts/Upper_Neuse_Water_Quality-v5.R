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

#Upper_Neuse_Raw_Data<-read.csv("output/Data/WQP_Upper-Neuse_RAW.csv") #large file
Upper_Neuse_Water<-read.csv("output/Data/WQP_Upper-Neuse_WATER.csv") #large file

Both_Water<-read.csv("output/Data/WQP_FR-EC_WATER.csv")
Flat_River_Water<-read.csv("output/Data/WQP_FR_WATER.csv")
Ellerbe_Creek_Water<-read.csv("output/Data/WQP_EC_WATER.csv")

Upper_Neuse_Sites<-read.csv("output/Data/WQP_Upper-Neuse_SITES.csv")
Ellerbe_Flat_Sites<-read.csv("output/Data/WQP_FR-EC_SITES.csv")

####################### 
#### Analyize Data ####
#######################

# Graph of decadal sampling
Year_Plot<- Both_Water %>% 
  ggplot(aes(x=as.character(Year), fill=Site_Group)) +
  geom_bar() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
  labs(title="Both Areas by Decade")
print(Year_Plot)
ggsave("output/Years.png")

Year_4_site_Plot<- Both_Water  %>% 
  filter(str_detect(MonitoringLocationIdentifier, 
                    "USGS-02085500|USGS-02086500|USGS-02086849|J1269000")) %>% 
  ggplot(aes(x=as.character(Year), fill=MonitoringLocationName)) +
  geom_bar() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
  labs(title="Both Areas by Decade")
print(Year_4_site_Plot)
ggsave("output/Years_4_sites.png")

decade_EC_Plot<-Ellerbe_Creek_Water %>% 
  ggplot(aes(x=MonitoringLocationIdentifier, fill = as.character(Decade))) +
  geom_bar() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
  labs(title="Ellerbe Creek by Decade")
print(decade_EC_Plot)
ggsave("output/Decades_EC.png")

decade_FR_Plot<-Flat_River_Water %>% 
  ggplot(aes(x=MonitoringLocationIdentifier, fill = as.character(Decade))) +
  geom_bar() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
  labs(title="Flat River by Decade")
print(decade_FR_Plot)
ggsave("output/Decades_FR.png")

# Plot by categories
cat_EC_Plot<-Ellerbe_Creek_Water %>% 
  ggplot(aes(x=MonitoringLocationIdentifier, fill = parameter_group_nm)) +
  geom_bar() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
  labs(title="Ellerbe Creek by Categories")
print(cat_EC_Plot)
ggsave("output/cats_EC.png")

cat_FR_Plot<-Flat_River_Water %>% 
  ggplot(aes(x=MonitoringLocationIdentifier, fill = parameter_group_nm)) +
  geom_bar() + 
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
  labs(title="Flat River by Category") 
print(cat_FR_Plot)
ggsave("output/Cats_FR.png")

# Common PCode Graphs
Lead_Flat_River<-Flat_River_Water %>% 
  filter(USGSPCode == 01051) %>% 
  ggplot(aes(x=MonitoringLocationIdentifier, y=ResultMeasureValue)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
  labs(title="Flat River Lead") 
print(pH_Flat_River)
ggsave("output/pH_Flat_River.png")

Lead_Ellerbe_Creek<-Ellerbe_Creek_Water %>% 
  filter(USGSPCode == 01051) %>% 
  ggplot(aes(x=MonitoringLocationIdentifier, y=ResultMeasureValue)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
  labs(title="Ellerbe Creek Lead") 
print(pH_Ellerbe_Creek)
ggsave("output/pH_Ellerbe_Creek.png")

######################
#### Data Analysis ###
######################

# Manipulate data to prepare for figure
Both_Water_Plot_Data<-Both_Water %>% 
  group_by(Year, MonitoringLocationIdentifier, USGSPCode) %>% 
  mutate(Average_Measure_YearLocationAnalyte=mean(ResultMeasureValue, na.rm=TRUE)) %>% 
  ungroup() %>% 
  select(Year, MonitoringLocationIdentifier, MonitoringLocationName, USGSPCode, Parameter_with_units, CharacteristicName, ActivityStartDate, ActivityStartTime.Time, Average_Measure_YearLocationAnalyte, ResultMeasureValue, parameter_units)

# Subsect of sites
Four_Site_Data<-Both_Water_Plot_Data %>% 
  filter(str_detect(MonitoringLocationIdentifier, 
                    "USGS-02085500|USGS-02086500|USGS-02086849|J1269000")) # "J1330000" is another site, near Glenstone Preserve 

Two_Site_Data<-Both_Water_Plot_Data %>% 
  filter(str_detect(MonitoringLocationIdentifier, 
                    c("02086500|02086849"))) %>% 
  filter(Year == 2014)

## Investigate the most common PCodes

# Identify the number of PCodes that are NAs (USGS sites should have 0)
Both_Water_Plot_Data %>% 
  filter(is.na(USGSPCode)) %>%
  group_by(USGSPCode) %>%
  tally() %>% 
  arrange(desc(n)) #View the number of unlabeled measurements

Both_Water_Plot_Data %>% 
  filter(str_detect(MonitoringLocationIdentifier, c("USGS-02086849"))) %>%
  filter(is.na(USGSPCode)) %>%
  group_by(USGSPCode) %>%
  tally() %>% 
  arrange(desc(n)) #This shows the count of NAs in USGS-02086849 (should be 0)

Both_Water_Plot_Data %>% 
  filter(str_detect(MonitoringLocationIdentifier, c("USGS-02086500"))) %>%
  filter(is.na(USGSPCode)) %>%
  group_by(USGSPCode) %>%
  tally() %>% 
  arrange(desc(n)) #This shows the count of NAs in USGS-02086500 (should be 0)

# Summary of the parameters
Parm_Sum_Gorman_EC<-Both_Water_Plot_Data %>% 
  filter(str_detect(MonitoringLocationIdentifier, c("USGS-02086849"))) %>%
  group_by(USGSPCode) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  left_join(Parameter_Data)

Parm_Sum_Dam_FR<-Both_Water_Plot_Data %>% 
  filter(str_detect(MonitoringLocationIdentifier, c("USGS-02086500"))) %>%
  group_by(USGSPCode) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  left_join(Parameter_Data)

#write.csv(Both_Water_Plot_Data, file = "output/Both-water-data_sum.csv")
#write.csv(Parm_Sum_Gorman_EC, file = "output/Gorman_Ellerbe-Creek_Parameter-Sum.csv")
#write.csv(Parm_Sum_Dam_FR, file = "output/Mickie-Dam_Flat-River_Parameter-Sum.csv")

# Examine the most popular dates
EC_Dates_Sum<-Two_Site_Data %>%
  filter(str_detect(MonitoringLocationIdentifier, c("USGS-02086849"))) %>%
  group_by(ActivityStartDate) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  rename(Count_Ellerbe_Creek = n)

FR_Dates_Sum<-Two_Site_Data %>%
  filter(str_detect(MonitoringLocationIdentifier, c("USGS-02086500"))) %>%
  group_by(ActivityStartDate) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  rename(Count_Flat_River = n)

Dates_Sum<-full_join(EC_Dates_Sum, FR_Dates_Sum, by = "ActivityStartDate") %>% 
  #drop_na() %>% 
  view()

# Other things
Four_Site_Sum<-Four_Site_Sum %>% 
  distinct(Unique_Code, .keep_all= TRUE)

Two_Site_Year_Sum<-Both_Water_Year_Sum %>% 
  ungroup() %>% 
  filter(str_detect(MonitoringLocationIdentifier, 
                    c("02086500", "02086849")))

#view(Four_Site_Sum)

# Try to work with pH
pH_PLOT <- Four_Site_Sum %>% 
  filter(USGSPCode == "00400") %>% 
  ggplot(aes(x=Year, y = Average_Measure_YearLocationAnalyte,
             colour = MonitoringLocationIdentifier,
             shape = MonitoringLocationIdentifier)) +
  geom_line() + 
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
  labs(title="Flat River by Category") 
print(pH_PLOT)

#Look at the PCodes
thing<-arrange(tally(group_by(Four_Site_Sum, USGSPCode)), desc(n)) %>% 
  left_join(Parameter_Data) %>% 
  view()

#################
#### FIGURES ####
#################

# Get data ready
Both_Water_Plot_Data_2014<-Both_Water_Plot_Data %>% 
  filter(USGSPCode == 30209) %>% #adding in data for daily flow (m^3/sec), right_join below brings all data back
  rename(Stream_Flow_Value_m3persec=ResultMeasureValue) %>% 
  select(Year, MonitoringLocationIdentifier, MonitoringLocationName, ActivityStartDate, Stream_Flow_Value_m3persec) %>% 
  right_join(Both_Water_Plot_Data, 
             by= c("Year", "MonitoringLocationIdentifier", "MonitoringLocationName", "ActivityStartDate")) %>% #Brings back all the data
  filter(Year == 2014)

Param_Groups<-Parameter_Data %>% 
  select(USGSPCode, parameter_group_nm)
Both_Water_Plot_Data_2014<-left_join(Both_Water_Plot_Data_2014, Param_Groups)
  
Both_Water_Plot_Data_2014<-Both_Water_Plot_Data_2014 %>% 
  group_by(MonitoringLocationIdentifier, Parameter_with_units) %>% 
  mutate(Max_Param_Value = max(ResultMeasureValue)) %>% 
  ungroup()

Two_Sites_2014<-Both_Water_Plot_Data_2014 %>% 
  filter(str_detect(MonitoringLocationIdentifier, 
                    c("02085500|02086849"))) %>% 
  filter(Year == 2014)

#write.csv(Both_Water_Plot_Data_2014, file = "output/Both-water_2014_data.csv")

# Get max data into g/L for comparison (on a log graph)
Both_Water_Plot_Data_2014<-Both_Water_Plot_Data_2014 %>% 
  mutate(Measured_Value_gperL = case_when(str_detect(parameter_units, "mg/l") ~ Max_Param_Value/1000,
                                          parameter_units == "ug/l" ~ Max_Param_Value/1000000,
                                          parameter_units == "ng/l" ~ Max_Param_Value/1000000000)) %>%
  mutate(Measured_Value_ugperL = Measured_Value_gperL * 1000000) %>% 
  mutate(Measured_Value_ugperL_not_min = case_when(str_detect(parameter_units, "mg/l") ~ ResultMeasureValue/1000,
                                                   parameter_units == "ug/l" ~ ResultMeasureValue/1000000,
                                                   parameter_units == "ng/l" ~ ResultMeasureValue/1000000000)) %>% 
  filter(str_detect(parameter_group_nm , c("rganic", "Nutrient"))) #%>% 
#  mutate_all(~replace(., is.na(.), 0))

Max_Data_2014<-Both_Water_Plot_Data_2014 %>% 
  select(Year, MonitoringLocationIdentifier, MonitoringLocationName,
         Parameter_with_units, parameter_units, parameter_group_nm, Max_Param_Value, Measured_Value_gperL) %>% 
  unique()


# First plot
g_per_L_graph<-Max_Data_2014 %>% 
  ggplot(aes(x=MonitoringLocationName, y=Measured_Value_gperL, shape = parameter_group_nm, colour = parameter_group_nm)) +
  geom_jitter() + #lot(binaxis = "y", stackdir = "center") #+
  scale_y_continuous(trans='log10') +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1)) + 
  labs(title="Max Measured Nutrients and Contaminants") +
  aes(str_wrap(MonitoringLocationName, 10), Measured_Value_gperL) + xlab(NULL) +
  ylab("Max Measured Value Log(g/L)")
print(g_per_L_graph)
ggsave("output/Max_Measured_Nuts_Contams.png")

g_per_L_graph_new<-Max_Data_2014 %>% 
  ggplot(aes(x=str_wrap(Parameter_with_units, 10), y=Measured_Value_gperL, shape = MonitoringLocationIdentifier, colour = parameter_group_nm)) +
  geom_jitter() + #lot(binaxis = "y", stackdir = "center") #+
  scale_y_continuous(trans='log10') +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1)) + 
  labs(title="Max Measured Nutrients and Contaminants", x="Max Measured Value Log(g/L)")
print(g_per_L_graph_new)
#ggsave("output/Max_Measured_Nuts_Contams.png")

# More plots
Grouped_Box<-Max_Data_2014 %>% 
  filter(!is.na(Measured_Value_gperL)) %>% 
  ggplot(aes(x=str_wrap(parameter_group_nm, 10), y=Measured_Value_gperL, fill=MonitoringLocationName)) +
  geom_boxplot() +
  scale_y_continuous(trans='log10') +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1)) + 
  labs(title= "Max Value Measured, by Analyte Group", y="Max Measured Value (g/L)", x="Analyte Groups")
print(Grouped_Box)
ggsave("output/grouped-box.png")

Count_by_Group<-Max_Data_2014 %>% 
  filter(!is.na(Measured_Value_gperL)) %>% 
  group_by(MonitoringLocationIdentifier, Parameter_with_units) %>% 
  unique() %>% 
  ggplot(aes(x=str_wrap(parameter_group_nm, 10), fill=MonitoringLocationName)) +
  geom_bar() +
  #geom_text(aes(label = ..count..), stat="count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1)) + 
  labs(title="Count of Detected Analytes, by Group", y="Count of Analytes Detected", x="Analyte Groups")
print(Count_by_Group)
ggsave("output/count-by-group.png")

Detects_by_Group<-Both_Water_Plot_Data_2014 %>% 
  filter(!is.na(Measured_Value_gperL)) %>%
  ggplot(aes(x=str_wrap(parameter_group_nm, 10), fill=MonitoringLocationName)) +
  geom_bar(stat="count") +
  #geom_text(aes(label = ..count..), stat="count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1)) + 
  labs(title= "Count of Total Detects, per Group", y="Count of Measurements, Above Detection Limit", x="Analyte Group")
print(Detects_by_Group)
ggsave("output/detects-by-group.png")

Nondetects_by_Group<-Both_Water_Plot_Data_2014 %>% 
  filter(is.na(Measured_Value_gperL)) %>%
  ggplot(aes(x=str_wrap(parameter_group_nm, 10), fill=MonitoringLocationName)) +
  geom_bar(stat="count") +
  #geom_text(aes(label = ..count..), stat="count") +
  theme_classic() +
  scale_y_continuous(trans='log10') +
  theme(axis.text.x = element_text(angle=60, hjust = 1)) + 
  labs(title= "Count of Total NON-Detects, per Group", y="Count of Measurements, BELOW Detection Limit", x="Analyte Group")
print(Nondetects_by_Group)
ggsave("output/nondetects-by-group.png")

Detects_by_Analyte<-Both_Water_Plot_Data_2014 %>% 
  filter(!is.na(Measured_Value_gperL)) %>%
  group_by(parameter_group_nm) %>% 
  filter(str_detect(parameter_group_nm, "pest")) %>% #"other" for pharma
  ggplot(aes(x=str_wrap(CharacteristicName, 20), fill=MonitoringLocationName)) +
  geom_bar(stat="count") +
  #geom_text(aes(label = ..count..), stat="count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1)) + 
  labs(title="Count of Total Detects, Pesticides", y="Count of Positive Detects", x="Analyte")
print(Detects_by_Analyte)
ggsave("output/detects-by-analyte.png")


Both_Water_Plot_Data_2014_edit<-read.csv("output/Both-water_2014_data_altered.csv") 

#Newertoday
All_Analytes<-Both_Water_Plot_Data_2014 %>%
  distinct(MonitoringLocationName, CharacteristicName, Max_Param_Value, .keep_all = TRUE) %>%
  filter(!is.na(Measured_Value_gperL)) %>%
  filter(Measured_Value_gperL>0) %>% 
 # filter(str_detect(parameter_group_nm, "other")) %>% #"other" 
  group_by(CharacteristicName) %>% 
  ggplot(aes(x=str_wrap(CharacteristicName, 30), y=Measured_Value_gperL, colour=MonitoringLocationName)) +
  geom_point() +
  scale_y_continuous(trans='log10') +
  coord_flip() +
  #geom_text(aes(label = ..count..), stat="count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1)) + 
  labs(title="Max Measured Value, by Analyte", y="Max Measured Value (g/L)", x=NULL)
print(All_Analytes)
ggsave("output/all-analytes.png")


Pest_Pharm<-Both_Water_Plot_Data_2014_edit %>% 
  distinct(MonitoringLocationName, CharacteristicName, Max_Param_Value, .keep_all = TRUE) %>%
  filter(!is.na(Measured_Value_gperL)) %>%
  filter(Measured_Value_gperL>0) %>% 
  filter(str_detect(parameter_group_nm, "other|pest")) %>% #"other" 
  group_by(CharacteristicName) %>% 
  ggplot(aes(x=str_wrap(CharacteristicName, 30), y=Measured_Value_gperL, colour=MonitoringLocationName)) +
  geom_point() +
  scale_y_continuous(trans='log10') +
  coord_flip() +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1)) + 
  labs(title="Pesticides & Pharma: Max Measured Value, by Analyte", y="Max Measured Value (g/L)", x=NULL)
print(Pest_Pharm)
ggsave("output/pest-pharma.png")

Not_Pest_Pharm<-Both_Water_Plot_Data_2014_edit %>%
  distinct(MonitoringLocationName, CharacteristicName, Max_Param_Value, .keep_all = TRUE) %>% 
  filter(!is.na(Measured_Value_gperL)) %>%
  filter(Measured_Value_gperL>0) %>% 
  filter(!str_detect(parameter_group_nm, "other|pest")) %>% #"other" 
  group_by(CharacteristicName) %>% 
  ggplot(aes(x=str_wrap(CharacteristicName, 30), y=Measured_Value_gperL, colour=MonitoringLocationName)) +
  geom_point() +
  scale_y_continuous(trans='log10') +
  coord_flip() +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1)) + 
  labs(title="Not Pesticides & Pharma: Max Measured Value, by Analyte", y="Max Measured Value (g/L)", x=NULL)
print(Not_Pest_Pharm)
ggsave("output/not-pest-pharma.png")

# Newest plots for 2 sites
Two_Sites_2014_Small_start<- Two_Sites_2014 %>% 
  distinct(MonitoringLocationName, CharacteristicName, Max_Param_Value, .keep_all = TRUE) %>%
  filter(!is.na(Measured_Value_ugperL)) %>%
  filter(Measured_Value_ugperL>0) %>% 
  filter(str_detect(parameter_group_nm, "other|pest")) %>% #"other" is pharma 
  filter(!str_detect(Parameter_with_units, "Organic"))

Two_Sites_2014_Small<- read.csv(file="output/labeled-analytes.csv")

Two_Sites<-Two_Sites_2014_Small %>%
  group_by(CharacteristicName) %>% 
  ggplot(aes(x=reorder(str_wrap(CharacteristicName, 40), Measured_Value_ugperL), y=Measured_Value_ugperL, fill=Analyte_Type)) +
  geom_bar(stat="identity", position="dodge") +
 # scale_y_continuous(trans='log10') +
  coord_flip() +
  #geom_text(aes(label = ..count..), stat="count") +
  theme_classic() +
  theme(legend.position="bottom", axis.text.x = element_text(angle=60, hjust = 1)) + 
  labs(title="Ellerbe Creek Near Gorman, Top 20 Organic Analytes", y="Max Measured Value (ug/L)", x=NULL)
print(Two_Sites)
ggsave("output/two-sites.png")



# plots
Stream_Flow<-Both_Water_Plot_Data_2014 %>% 
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
ggsave("output/Stream_Flow.png")

Nitrogen<-Both_Water_Plot_Data_2014 %>% 
  filter(str_detect(USGSPCode, "00618|00608|00613|00605|00631")) %>%
  filter(!is.na(ResultMeasureValue)) %>% 
  ggplot(aes(x=Parameter_with_units, y=ResultMeasureValue, fill=MonitoringLocationName)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1)) + 
  labs(title="Various Nitrogen Components", y="Measured Value (mg/L)") +
  aes(str_wrap(Parameter_with_units, 10), ResultMeasureValue) + xlab(NULL) +
  ylab("Result Measure")
print(Nitrogen)
ggsave("output/Nitrogen.png")

Other_Nutrients<-Both_Water_Plot_Data_2014 %>% 
  filter(str_detect(USGSPCode, "00671|00665|00945|00925|00940|00915|00930|00935|00300")) %>%
  filter(!is.na(ResultMeasureValue)) %>% 
  ggplot(aes(x=Parameter_with_units, y=ResultMeasureValue, fill=MonitoringLocationName)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1)) + 
  labs(title="Various Nutrients and Metals", y="Measured Value (mg/L)") +
  aes(str_wrap(Parameter_with_units, 10), ResultMeasureValue) + xlab(NULL) +
  ylab("Result Measure")
print(Other_Nutrients)
ggsave("output/Others-Nutrients-Metals.png")

Other_Analytes<-Both_Water_Plot_Data_2014 %>% 
  filter(str_detect(USGSPCode, "80154|00900|00681")) %>%
  filter(!is.na(ResultMeasureValue)) %>% 
  ggplot(aes(x=Parameter_with_units, y=ResultMeasureValue, fill=MonitoringLocationName)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1)) + 
  labs(title="Other Analytes", y="Measured Value (mg/L)") +
  aes(str_wrap(Parameter_with_units, 10), ResultMeasureValue) + xlab(NULL) +
  ylab("Result Measure")
print(Other_Analytes)
ggsave("output/Other-Analytes.png")

Organics_ng<-Both_Water_Plot_Data_2014 %>% 
  filter(str_detect(USGSPCode, "67436|67440|67443|67488|67500|67493|68687|367517|67457")) %>%
  filter(!is.na(ResultMeasureValue)) %>% 
  ggplot(aes(x=Parameter_with_units, y=ResultMeasureValue, fill=MonitoringLocationName)) +
  geom_dotplot(binaxis = "y", stackdir = "center") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1)) + 
  labs(title="Other Analytes", y="Measured Value (ng/L)") +
  aes(str_wrap(Parameter_with_units, 10), ResultMeasureValue) + xlab(NULL) +
  ylab("Result Measure")
print(Organics_ng)
ggsave("output/Organics_ng-per-L.png")

Organics_ug<-Both_Water_Plot_Data_2014 %>% 
  filter(str_detect(USGSPCode, "62722|77061|34475|34010")) %>%
  filter(!is.na(ResultMeasureValue)) %>% 
  ggplot(aes(x=Parameter_with_units, y=ResultMeasureValue, fill=MonitoringLocationName)) +
  geom_dotplot(binaxis = "y", stackdir = "center") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1)) + 
  labs(title="Other Analytes", y="Measured Value (ug/L)") +
  aes(str_wrap(Parameter_with_units, 10), ResultMeasureValue) + xlab(NULL) +
  ylab("Result Measure")
print(Organics_ug)
ggsave("output/Organics_ug-per-L.png")

# Compare relative to flow for Ellerbe
Nitrogen_flow<-Both_Water_Plot_Data_2014 %>% 
  filter(str_detect(USGSPCode, "00618|00608|00613|00605|00631")) %>%
  filter(str_detect(MonitoringLocationIdentifier, "02086849")) %>% 
  filter(!is.na(ResultMeasureValue)) %>% 
  ggplot(aes(x= Stream_Flow_Value_m3persec, y=ResultMeasureValue, color=Parameter_with_units)) +
  geom_point() +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1), legend.text=element_text(size=7)) +
  labs(title="Various Nitrogen Components for Ellerbe Creek Near Gorman", y="Measured Value (mg/L)")
print(Nitrogen_flow)
ggsave("output/Nitrogen_flow.png")

Other_Nutrients_flow<-Both_Water_Plot_Data_2014 %>% 
  filter(str_detect(USGSPCode, "00671|00665|00945|00925|00940|00915|00930|00935|00300")) %>%
  filter(str_detect(MonitoringLocationIdentifier, "02086849")) %>% 
  filter(!is.na(ResultMeasureValue)) %>% 
  ggplot(aes(x= Stream_Flow_Value_m3persec, y=ResultMeasureValue, color=Parameter_with_units)) +
  geom_point() +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1), legend.text=element_text(size=7)) + 
  labs(title="Various Nutrients and Metals for Ellerbe Creek Near Gorman", y="Measured Value (mg/L)") 
print(Other_Nutrients_flow)
ggsave("output/Others-Nutrients-Metals_flow.png")

Other_Analytes_flow<-Both_Water_Plot_Data_2014 %>% 
  filter(str_detect(USGSPCode, "80154|00900|00681")) %>%
  filter(str_detect(MonitoringLocationIdentifier, "02086849")) %>% 
  filter(!is.na(ResultMeasureValue)) %>% 
  ggplot(aes(x= Stream_Flow_Value_m3persec, y=ResultMeasureValue, color=Parameter_with_units)) +
  geom_point() +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1), legend.text=element_text(size=7)) + 
  labs(title="Other Analytes for Ellerbe Creek Near Gorman", y="Measured Value (mg/L)") 
print(Other_Analytes_flow)
ggsave("output/Other-Analytes_flow.png")

Organics_ng_flow<-Both_Water_Plot_Data_2014 %>% 
  filter(str_detect(USGSPCode, "67436|67440|67443|67488|67500|67493|68687|367517|67457")) %>%
  filter(str_detect(MonitoringLocationIdentifier, "02086849")) %>% 
  filter(!is.na(ResultMeasureValue)) %>% 
  ggplot(aes(x= Stream_Flow_Value_m3persec, y=ResultMeasureValue, color=Parameter_with_units)) +
  geom_point() +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust = 1), legend.text=element_text(size=7)) + 
  labs(title="Other Analytes for Ellerbe Creek Near Gorman", y="Measured Value (ng/L)") 
print(Organics_ng_flow)
ggsave("output/Organics_ng-per-L_flow.png")

Organics_ug_flow<-Both_Water_Plot_Data_2014 %>% 
  filter(str_detect(USGSPCode, "62722|77061|34475|34010")) %>%
  filter(str_detect(MonitoringLocationIdentifier, "02086849")) %>% 
  filter(!is.na(ResultMeasureValue)) %>% 
  ggplot(aes(x= Stream_Flow_Value_m3persec, y=ResultMeasureValue, color=Parameter_with_units)) +
  geom_point() +
  theme_classic() +
  labs(title="Other Analytes for Ellerbe Creek Near Gorman", y="Measured Value (ug/L)") 
print(Organics_ug_flow)
ggsave("output/Organics_ug-per-L_flow.png")


#manually do positions
#positions <- c("Goalkeeper", "Defense", "Striker")
#p <- ggplot(theTable, aes(x = Position)) + scale_x_discrete(limits = positions)

#ggsave("output/Stream_Flow.png")

# Other things
#Four_Site_Data %>% 
# filter(MonitoringLocationIdentifier == "21NC01WQ-J1269000") %>% 
#mutate(NEW = tally(group_by(Four_Site_Data, Decade))) %>% 
#  group_by(Decade) %>% 
# tally() %>% 
#view()

#Four_Site_Data %>% 
# ggplot(aes(x=Decade, y=X, 
#           colour=MonitoringLocationIdentifier, 
#          group=MonitoringLocationIdentifier)) + 
#  geom_point() + geom_line() +
# facet_grid(variable ~ ., scale = "free_y")