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

# Work
Flat<-read.table("data_raw/Flat_Dam.txt", header = TRUE, sep = "\t") %>% 
  mutate(Gage_ft = ifelse(parameter_cd=="65", mean_va, NA)) %>% 
  mutate(Discharge_cu3sec = ifelse(parameter_cd=="60", mean_va, NA)) %>% 
  unite(year_month, c("year_nu","month_nu")) %>% 
  select(-c(mean_va, ts_id))

Flat_1<-filter(Flat, parameter_cd == "60") %>% select(-Gage_ft)
Flat_2<-filter(Flat, parameter_cd == "65") %>% select(-Discharge_cu3sec)
Flat<-full_join(Flat_1, Flat_2, by = c("agency_cd", "site_no", "year_month")) %>% 
  select(-contains("parameter")) 

Club<-read.table("data_raw/Ellerbe_Club.txt", header = TRUE, sep = "\t") %>% 
  mutate(Gage_ft = ifelse(parameter_cd=="65", mean_va, NA)) %>% 
  mutate(Discharge_cu3sec = ifelse(parameter_cd=="60", mean_va, NA)) %>% 
  unite(year_month, c("year_nu","month_nu")) %>% 
  select(-c(mean_va, ts_id))

Club_1<-filter(Club, parameter_cd == "60") %>% select(-Gage_ft)
Club_2<-filter(Club, parameter_cd == "65") %>% select(-Discharge_cu3sec)
Club<-full_join(Club_1, Club_2, by = c("agency_cd", "site_no", "year_month")) %>% 
  select(-contains("parameter"))

Glennstone<-read.table("data_raw/Ellerbe_Glennstone.txt", header = TRUE, sep = "\t") %>% 
  mutate(Gage_ft = ifelse(parameter_cd=="65", mean_va, NA)) %>% 
  mutate(Discharge_cu3sec = ifelse(parameter_cd=="60", mean_va, NA)) %>% 
  unite(year_month, c("year_nu","month_nu")) %>% 
  select(-c(mean_va, ts_id))

Glennstone_1<-filter(Glennstone, parameter_cd == "60") %>% select(-Gage_ft)
Glennstone_2<-filter(Glennstone, parameter_cd == "65") %>% select(-Discharge_cu3sec)
Glennstone<-full_join(Glennstone_1, Glennstone_2, by = c("agency_cd", "site_no", "year_month")) %>% 
  select(-contains("parameter"))


Haw<-read.table("data_raw/Haw_Near-Jordan-Lake.txt", header = TRUE, sep = "\t") %>% 
  mutate(Gage_ft = ifelse(parameter_cd=="65", mean_va, NA)) %>% 
  mutate(Discharge_cu3sec = ifelse(parameter_cd=="60", mean_va, NA)) %>% 
  unite(year_month, c("year_nu","month_nu")) %>% 
  select(-c(mean_va, ts_id))

Haw_1<-filter(Haw, parameter_cd == "60") %>% select(-Gage_ft)
Haw_2<-filter(Haw, parameter_cd == "65") %>% select(-Discharge_cu3sec)
Haw<-full_join(Haw_1, Haw_2, by = c("agency_cd", "site_no", "year_month")) %>% 
  select(-contains("parameter")) 

All_sites<-full_join(Flat, Club)
All_sites<-full_join(All_sites, Glennstone) 
All_sites<-full_join(All_sites, Haw) %>% 
  mutate(site_no=recode(site_no,
                        "2086500" = "Flat River Near Dam",
                        "208675010" = "Ellerbe Creek by Club Blvd",
                        "2086849" = "Ellerbe Creek by Glennstone",
                        "2096960" = "Haw River upstream/near Jordan Lake"))

# Rating Curve
Rating_Curve<-All_sites %>% 
  ggplot(aes(x= Gage_ft, y=Discharge_cu3sec, color = as.character(site_no))) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(trans='log10') +
  theme(axis.text.x = element_text(angle=60, hjust = 1), legend.text=element_text(size=7)) +
  labs(title="Rating Curve for USGS Gages")
print(Rating_Curve)
ggsave("output/3-site_Rating-Curve.png")
