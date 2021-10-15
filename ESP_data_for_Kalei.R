#----------------------------------------------------------------------------------------------
#  Now update the Bering Sea data 
#----------------------------------------------------------------------------------------------

#  Create the bering lookup table
lkp <- readRDS("Data/crwsst/crwsst_spatial_lookup_table.RDS") %>% 
  filter(Ecosystem_sub=="Southeastern Bering Sea" & depth<(-10)) %>% 
  dplyr::select(Ecosystem_sub,id)

# To recreate the full time series from the raw data you can rerun the commented out code. Takes 10 minutes or so.
lapply(1985:2021,function(x) readRDS(paste0("Data/crwsst/crw_sst_matched/crw_sst_matched_",x,".RDS")) %>%
                inner_join(lkp) %>%
                dplyr::select(-id) %>%
                mutate(date=as_date(date),
                       month=month(date),
                       year=year(date)) %>%
                 filter(month%in%c(5:6)) %>% 
                group_by(year) %>%
                summarise(meansst=mean(CRW_SST,na.rm=TRUE))) %>%
 bind_rows() %>%
 saveRDS("Data/Kalei/crwsst_SEBS_19850401_through_2021_May_June_Avg.RDS")

#  Load the freshly downloaded / merged data from above, filter out just the bering data, and merge with the previous time series.
# readRDS("Data/crwsst/crw_sst_matched/crw_sst_matched_2020_through_Sept.RDS") %>%
#   inner_join(lkp) %>%
#   dplyr::select(-id) %>%
#   mutate(date=as_date(date),
#          month=month(date),
#          year=year(date)) %>%
#   filter(month%in%c(5:6)) %>% 
#   group_by(year) %>%
#   summarise(meansst=mean(CRW_SST,na.rm=TRUE)) %>%
#   bind_rows(readRDS("crwsst_SEBS_19850401_through_2019_May_June_Avg.RDS")) %>% 
#   distinct() %>% 
#   saveRDS("crwsst_SEBS_19850401_through_2020_May_June_Avg.RDS")

# data <- readRDS("Data/crwsst_bering_19850401_through_Sept.RDS")
# 
# data %>% 
#   mutate(month=month(date),
#          year=year(date)) %>%
#   filter(month%in%c(4:6)) %>% 
#   group_by(Ecosystem_sub,year) %>% 
#     summarise(meansst=mean(meansst)) %>%
#   saveRDS("Bering_Sea_Shelf_April_Through_June_Mean.RDS")
# 
# data %>% 
#   mutate(month=month(date),
#          year=year(date)) %>%
#   filter(month%in%c(6:10)) %>% 
#   group_by(Ecosystem_sub,year) %>% 
#   summarise(meansst=mean(meansst)) %>%
#   saveRDS("Bering_Sea_Shelf_June_Through_Oct_Mean.RDS")


#  The following files come from "wcgoa_for_ESP.R"

#  Make ESP versions for upload to AKFIN web tool
readRDS("Data/Kalei/crwsst_goa_nmfs640_650_MayJune_Avg.RDS") %>% 
  mutate(INDICATOR_NAME="Spring_Temperature_Surface_EGOA_Satellite",
         meansst=round(meansst,2)) %>% 
  dplyr::select(YEAR=year,INDICATOR_NAME,DATA_VALUE=meansst) %>% 
  write.csv("Data/Kalei/Spring_Temperature_Surface_EGOA_Satellite.csv",row.names = FALSE)

#  Make ESP versions for upload to AKFIN web tool
readRDS("Data/Kalei/crwsst_goa_nmfs610_620_630_AprMay_Avg.RDS") %>% 
  mutate(INDICATOR_NAME="Spring_Temperature_Surface_WCGOA_Satellite",
         meansst=round(meansst,2)) %>% 
  arrange(year) %>% 
  dplyr::select(YEAR=year,INDICATOR_NAME,DATA_VALUE=meansst) %>% 
  write.csv("Data/Kalei/Spring_Temperature_Surface_WCGOA_Satellite.csv",row.names = FALSE)


#  Make ESP versions for upload to AKFIN web tool
readRDS("Data/Kalei/crwsst_SEBS_19850401_through_2021_May_June_Avg.RDS") %>% 
  mutate(INDICATOR_NAME="Spring_Temperature_Surface_SEBS_Satellite",
         meansst=round(meansst,2)) %>% 
  arrange(year) %>% 
  dplyr::select(YEAR=year,INDICATOR_NAME,DATA_VALUE=meansst) %>% 
  write.csv("Data/Kalei/Spring_Temperature_Surface_SEBS_Satellite.csv",row.names = FALSE)



#  Create the bering lookup table
lkp <- readRDS("Data/crwsst/crwsst_spatial_lookup_table.RDS") %>% 
  filter(Ecosystem_sub=="Southeastern Bering Sea" & depth<=(-10) & depth>=(-200)) %>% 
  dplyr::select(Ecosystem_sub,id)

# To recreate the full time series from the raw data you can rerun the commented out code. Takes 10 minutes or so.
lapply(1985:2021,function(x) readRDS(paste0("Data/crwsst/crw_sst_matched/crw_sst_matched_",x,".RDS")) %>%
         inner_join(lkp) %>%
         dplyr::select(-id) %>%
         mutate(date=as_date(date),
                month=month(date),
                year=year(date)) %>%
         filter(month%in%c(4:6)) %>% 
         group_by(year) %>%
         summarise(meansst=mean(CRW_SST,na.rm=TRUE))) %>%
  bind_rows() %>%
  saveRDS("Data/Kalei/Bering_Sea_Shelf_April_Through_June_Mean.RDS")

#  Make ESP versions for upload to AKFIN web tool
readRDS("Data/Kalei/Bering_Sea_Shelf_April_Through_June_Mean.RDS") %>% 
  data.frame %>% 
  mutate(INDICATOR_NAME="Spring_Summer_Temperature_Surface_SEBS_Satellite",
         meansst=round(meansst,2)) %>% 
  arrange(year) %>% 
  dplyr::select(YEAR=year,INDICATOR_NAME,DATA_VALUE=meansst) %>% 
  write.csv("Data/Kalei/Spring_Summer_Temperature_Surface_SEBS_Satellite.csv",row.names = FALSE)


read.csv("Data/Kalei/Spring_Temperature_Surface_SEBS_Satellite.csv") %>% 
  bind_rows(read.csv("Data/Kalei/Spring_Summer_Temperature_Surface_SEBS_Satellite.csv")) %>% 
  bind_rows(read.csv("Data/Kalei/Spring_Temperature_Surface_EGOA_Satellite.csv")) %>% 
  bind_rows(read.csv("Data/Kalei/Spring_Temperature_Surface_WCGOA_Satellite.csv")) %>% 
  ggplot(aes(YEAR,DATA_VALUE)) + 
  geom_line() + 
  geom_smooth(method="lm") +
  geom_smooth() +
  facet_wrap(~INDICATOR_NAME)
