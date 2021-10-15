library(tidyverse)
library(lubridate)

#----------------------------------------------------------------------------
#  Extract Western and Central GOA grid coordinates from spatial lookup table
lkp <- readRDS("Data/crwsst/crwsst_spatial_lookup_table.RDS") %>% 
  filter(nmfsarea%in%c("610","620","630") & !is.na(nmfsarea) & depth<(-10) & depth>=(-200)) %>% 
  dplyr::select(nmfsarea,id)


#----------------------------------------------------------------------------
#  Access the individual annual files, match points with the spatial lookup.
#  Average all points (43,779 points per day across all regions) for a given day
data <- lapply(1985:2021,function(x) readRDS(paste0("Data/crwsst/crw_sst_matched/crw_sst_matched_",x,".RDS")) %>%
                 inner_join(lkp) %>%
                 dplyr::select(-id) %>%
                 mutate(date=as_date(date),
                        month=month(date),
                        year=year(date)) %>%
                 filter(month%in%c(6:10)) %>% 
                 group_by(year) %>%
                 summarise(meansst=mean(CRW_SST,na.rm=TRUE))) %>%
  bind_rows()

#  Save this new aggregated dataset from 1985 to 2021
data %>%
  #saveRDS("Data/crwsst_goa_nmfs610_620_630_19850101_through_2021.RDS") %>% 
  saveRDS("Data/crwsst_goa_nmfs610_620_630_2021_through_Sept.RDS")

# #  Because 2020 is incomplete, we annoyingly have to do this separately (makes it easier to update new data)
# readRDS("Data/crwsst/crw_sst_matched/crw_sst_matched_2020_through_Sept.RDS") %>%
#   inner_join(lkp) %>%
#   dplyr::select(-id) %>%
#   mutate(date=as_date(date),
#          month=month(date),
#          year=year(date)) %>%
#   filter(month%in%c(6:10)) %>% 
#   group_by(year) %>%
#   summarise(meansst=mean(CRW_SST,na.rm=TRUE)) %>% 
#   saveRDS("Data/crwsst_goa_nmfs610_620_630_2020_through_Sept.RDS")

#  Merge 2020 with the other years
# readRDS("Data/crwsst_goa_nmfs610_620_630_19850401_through_2019.RDS") %>% 
# bind_rows(readRDS("Data/crwsst_goa_nmfs610_620_630_2020_through_Sept.RDS")) %>% 
#   arrange(year) %>% 
#   saveRDS("Data/crwsst_goa_nmfs610_620_630_JuneOct_Avg.RDS")

data %>% 
arrange(year) %>% 
  saveRDS("Data/Kalei/crwsst_goa_nmfs610_620_630_JuneOct_Avg.RDS")


lapply(1985:2021,function(x) readRDS(paste0("Data/crwsst/crw_sst_matched/crw_sst_matched_",x,".RDS")) %>%
                 inner_join(lkp) %>%
                 dplyr::select(-id) %>%
                 mutate(date=as_date(date),
                        month=month(date),
                        year=year(date)) %>%
                 filter(month%in%c(4:5)) %>% 
                 group_by(year) %>%
                 summarise(meansst=mean(CRW_SST,na.rm=TRUE))) %>%
  bind_rows() %>% 
  arrange(year) %>% 
  saveRDS("Data/Kalei/crwsst_goa_nmfs610_620_630_AprMay_Avg.RDS")

# readRDS("Data/Kalei/crwsst_goa_nmfs610_620_630_AprMay_Avg.RDS") %>% data.frame



#----------------------------------------------------------------------------
#  Now repeat for the eastern GOA 640-650
#----------------------------------------------------------------------------


#----------------------------------------------------------------------------
#  Extract Eastern GOA grid coordinates from spatial lookup table
lkp <- readRDS("Data/crwsst/crwsst_spatial_lookup_table.RDS") %>% 
  filter(nmfsarea%in%c("640","650") & !is.na(nmfsarea) & depth<(-10)) %>% 
  dplyr::select(nmfsarea,id)

#----------------------------------------------------------------------------
#  Access the individual annual files, match points with the spatial lookup.
#  Average all points  for a given day
data <- lapply(1985:2021,function(x) readRDS(paste0("Data/crwsst/crw_sst_matched/crw_sst_matched_",x,".RDS")) %>%
                 inner_join(lkp) %>%
                 dplyr::select(-id) %>%
                 mutate(date=as_date(date),
                        month=month(date),
                        year=year(date)) %>%
                 filter(month%in%c(5:6)) %>% 
                 group_by(year) %>%
                 summarise(meansst=mean(CRW_SST,na.rm=TRUE))) %>%
  bind_rows()

#  Save this new aggregated dataset from 1985 to 2019
# data %>%
#   saveRDS("Data/crwsst_goa_nmfs640_650_MayJune_Avg_through_2021.RDS")

test <- readRDS("Data/crwsst/crw_sst_matched/crw_sst_matched_2021.RDS") %>% 
  inner_join(lkp) %>%
  dplyr::select(-id) %>%
  mutate(date=as_date(date),
         month=month(date),
         year=year(date)) %>%
  filter(month%in%c(5:6)) %>% 
  group_by(year) %>%
  summarise(meansst=mean(CRW_SST,na.rm=TRUE))

data %>% 
  arrange(year) %>% 
  saveRDS("Data/Kalei/crwsst_goa_nmfs640_650_MayJune_Avg.RDS")


# #  Because 2020 is incomplete, we annoyingly have to do this separately (makes it easier to update new data)
# readRDS("Data/crwsst/crw_sst_matched/crw_sst_matched_2020_through_Sept.RDS") %>%
#   inner_join(lkp) %>%
#   dplyr::select(-id) %>%
#   mutate(date=as_date(date),
#          month=month(date),
#          year=year(date)) %>%
#   filter(month%in%c(5:6)) %>% 
#   group_by(year) %>%
#   summarise(meansst=mean(CRW_SST,na.rm=TRUE)) %>% 
#   saveRDS("Data/crwsst_goa_nmfs640_650_2020_MayJune_Avg.RDS")
# 
# #  Merge 2020 with the other years
# readRDS("Data/crwsst_goa_nmfs640_650_MayJune_Avg_through_2019.RDS") %>% 
#   bind_rows(readRDS("Data/crwsst_goa_nmfs640_650_2020_MayJune_Avg.RDS")) %>% 
#   arrange(year) %>% 
#   saveRDS("Data/crwsst_goa_nmfs640_650_MayJune_Avg.RDS")



#----------------------------------------------------------------------------------
#  Let's take a look
#----------------------------------------------------------------------------------

# Make sure the plot looks okay.
png("GOA_temps_ESP.png")
readRDS("Data/crwsst_goa_nmfs640_650_1985_through_Sept.RDS") %>%
  bind_rows(readRDS("Data/crwsst_goa_nmfs610_620_630_1985_through_Sept.RDS")) %>% 
  ggplot(aes(date,meansst)) + 
  geom_line() + 
  geom_smooth() +
  facet_wrap(~region)
dev.off()


#devtools::install_github("brisneve/ggplottimeseries")
library(ggplottimeseries)

data <- readRDS("Data/crwsst_goa_nmfs640_650_1985_through_Sept.RDS") %>%
  bind_rows(readRDS("Data/crwsst_goa_nmfs610_620_630_1985_through_Sept.RDS"))

#  The following could all be combined but I have left it separated out to be more transparent.
egoa <- data %>% 
  filter(region=="EGOA")

#  Perform the time series decomposition for the EGOA, setting the frequency as 365.25 because we have daily data with leap years.
dfegoa <- dts1(egoa$date,egoa$meansst,365.25, type = "additive") %>% 
  mutate(region="Eastern Gulf of Alaska",
         year=year(date))

#  Repeat for the wgoa
wgoa <- data %>% 
  filter(region=="WCGOA")

dfwgoa <- dts1(wgoa$date,wgoa$meansst,365.25, type = "additive") %>% 
  mutate(region="WCGOA",
         year=year(date))

#  Combine the time series decompositions for each area and reorder the factors.
df <- dfegoa %>% 
  bind_rows(dfwgoa)

#  Create the horizontal mean and sd lines for the 30 year baseline period.
dfmean <- df %>% 
  group_by(region) %>% 
  summarise(meantrend=mean(trend[between(year,1986,2015)],na.rm=TRUE),
            sdtrend=sd(trend[between(year,1986,2015)],na.rm=TRUE))

OceansBlue2='#0055A4'

jpeg(paste0("SST_GOA_ESP_TimeSeriesTrend",format(Sys.Date(),"%Y_%m_%d"),".jpeg"),width=6,height=4,units="in",quality=100,res=300)
df %>% 
  ggplot(aes(x = date, y = trend)) + 
  geom_line() + 
  geom_hline(data=dfmean,aes(yintercept=meantrend),linetype=2) +
  geom_hline(data=dfmean,aes(yintercept=meantrend+sdtrend),linetype=2,color="red") +
  geom_hline(data=dfmean,aes(yintercept=meantrend-sdtrend),linetype=2,color="red") +
  facet_wrap(~region) + 
  theme(strip.text = element_text(size=10,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill=OceansBlue2),
        axis.title = element_text(size=10,family="sans"),
        axis.text = element_text(size=10,family="sans"),
        #panel.border=element_rect(colour="black",size=0.5),
        plot.margin=unit(c(0.65,0,0.65,0),"cm")) + 
  ylab("Sea surface temperature (C)") + 
  xlab("Date")
dev.off()
