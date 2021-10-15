## Calculate GOA chlorophyll peak timing and biomass for the ESPs
## Author: jordan.watson@noaa.gov
## 10/15/2021

library(tidyverse)
library(gridExtra)

# Read in the spatially merged chl files for each year and extract the WGGOA (610,620,630) and EGOA (640,650) areas.
# Within each area and on each day, average all records that were in federal waters with depths between 10 and 200m.
# First do the complete years (2003-2020) and then the most recent, incomplete year (2021). 
data <- lapply(2003:2020,function(x) readRDS(paste0("Data/chlorophyll/modis_8day/merged/chl_8day_merged_",x,".RDS")) %>% 
         filter((nmfsarea %in% c(610,620,630,640,650)) & statefed=="FED" & between(depth,-200,-10)) %>% 
         mutate(region=ifelse(nmfsarea%in%c(610,620,630),"WCGOA","EGOA")) %>% 
         group_by(time,region) %>% 
         summarise(meanchl=mean(chlorophyll,na.rm=TRUE))) %>% 
  bind_rows() %>% 
  mutate(date=as_date(as_datetime(time))) %>% # Reformat the date from integer to date.
  ungroup %>% 
  dplyr::select(-time) %>% 
  bind_rows(readRDS("Data/chlorophyll/modis_8day/merged/chl_8day_merged_2021_through_07162021.RDS") %>% 
              filter((nmfsarea %in% c(610,620,630,640,650)) & statefed=="FED" & between(depth,-200,-10)) %>% 
              mutate(region=ifelse(nmfsarea%in%c(610,620,630),"WCGOA","EGOA")) %>% 
              group_by(time,region) %>% 
              summarise(meanchl=mean(chlorophyll,na.rm=TRUE)) %>% 
              mutate(date=as_date(as_datetime(time))) %>% 
              ungroup %>% 
              dplyr::select(-time))

#  Table the merged files and for each region and year, find the first day on which the maximum annual value of chlorophyll occurs.
#  This is the caveman approach. Jens uses smoothing. He's also a much faster runner than me and speaks more languages. 
lapply(unique(data$region),function(x) data %>% 
         mutate(month=month(date),
                year=year(date),
                doy=yday(date)) %>% 
         filter(month%in%(3:6)) %>% 
         filter(region==x) %>%
         group_by(year) %>% 
         summarise(DATA_VALUE=doy[meanchl==max(meanchl)][1],
                   INDICATOR_NAME=paste0("Spring_Chlorophylla_Peak_",x,"_Satellite")) %>% 
         ungroup %>% 
         dplyr::select(YEAR=year,INDICATOR_NAME,DATA_VALUE) %>% 
         write.csv(paste0("Data/Kalei/Spring_Chlorophylla_Peak_",x,"_Satellite.csv"),row.names = F))




# EGOA and WCGOA calculation for peak biomass concentration. This is defined as the average chlorophyll during the typical peak chlorophyll month (May).
# This is primarily for sablefish so we changed the depth filter to be 10m to 1000m.
# Note that here I average over the entire gridded data for May instead of averaging the data within areas first and then averaging the averages over date.

data2 <- lapply(2003:2020,function(x) readRDS(paste0("Data/chlorophyll/modis_8day/merged/chl_8day_merged_",x,".RDS")) %>% 
                  mutate(date=as_date(as_datetime(time)), # Reformat the date from integer to date.
                         month=month(date),
                         YEAR=year(date)) %>% 
                  filter((nmfsarea %in% c(610,620,630,640,650) & month%in%c(5) & depth<(-10) & depth>(-200))) %>% 
                  mutate(region=ifelse(nmfsarea%in%c(610,620,630),"WCGOA","EGOA")) %>% 
                  group_by(region,YEAR) %>% 
                  summarise(DATA_VALUE=mean(chlorophyll,na.rm=TRUE))) %>% 
  bind_rows() %>% 
  ungroup %>% 
  bind_rows(readRDS("Data/chlorophyll/modis_8day/merged/chl_8day_merged_2021_through_07162021.RDS") %>% 
              mutate(date=as_date(as_datetime(time)), # Reformat the date from integer to date.
                     month=month(date),
                     YEAR=year(date)) %>% 
              filter((nmfsarea %in% c(610,620,630,640,650) & month%in%c(5) & depth<(-10) & depth>(-200))) %>% 
              mutate(region=ifelse(nmfsarea%in%c(610,620,630),"WCGOA","EGOA")) %>% 
              group_by(region,YEAR) %>% 
              summarise(DATA_VALUE=mean(chlorophyll,na.rm=TRUE))) %>% 
  ungroup %>% 
  mutate(INDICATOR_NAME=paste0("SPR_CHLORO_BIOM_",region))

lapply(unique(data2$region),function(x) data2 %>% 
  filter(region=="WCGOA") %>% 
  dplyr::select(-region) %>% 
  write.csv(paste0("Data/Kalei/Spring_Chlorophylla_Biomass_",x,"_Satellite.csv"),row.names = F))

# p1 <- data2 %>% 
#   ggplot(aes(year,meanchl,color=region)) + 
#   geom_line() +
#   geom_point() + 
#   ggtitle("10m to 1000m")
# 
# p2 <- data2 %>% 
#   ggplot(aes(year,meanchl,color=region)) + 
#   geom_line() +
#   geom_point() + 
#   ggtitle("10m to 200m")
# 
# grid.arrange(p1,p2)
