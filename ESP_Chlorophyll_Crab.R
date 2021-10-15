#  Calculate spring chlorophyll biomass for St Matts King Crab and for Bristol Bay King Crab.
#  jordan.watson@noaa.gov


library(tidyverse)
library(lubridate)

temp <- readRDS("Data/chlorophyll/modis_8day/merged/merged_8day_2003_2021_EBS.RDS") %>%
  mutate(month=month(date),
         YEAR=year(date)) %>%
  filter(crab%in%c("StMatts","BristolBay") & month%in%c(4:6)) %>%
  group_by(YEAR,crab) %>%
  summarise(DATA_VALUE=mean(chlorophyll,na.rm=TRUE))

lapply(unique(temp$crab),function(x) temp %>% 
         filter(crab==x) %>% 
         mutate(INDICATOR_NAME=case_when(crab=="BristolBay"~"SPR_CHLORO_BIOM_SEBS_IS",
                                         crab=="StMatts"~"SPR_CHLORO_BIOM_SMBKC")) %>% 
         dplyr::select(-crab) %>% 
         dplyr::select(YEAR,INDICATOR_NAME,DATA_VALUE) %>% 
         write.csv(paste0("Data/Kalei/Spring_Chlorophylla_Biomass_",x,"_Satellite.csv"),row.names = F))


