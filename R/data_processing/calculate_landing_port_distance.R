library(tidyverse)
library(ggplot2)
library(scales)

cpue_dat <-read.csv("data/cpue_snow_clean_2022.csv")
cpue_dat <- cpue_dat %>%
  #clean out strays
  filter(lat < 64 & lat > 53 & lon > -180 & lon < -155)

#==== Set up CPUE data for analysis per year per stat area
cpue_dat_station <- cpue_dat %>%
  # group_by(stat_area,season,week_season) %>% TOGGLE ON FOR BY SEASON
  group_by(stat_area,season,week_season,vessel) %>%
  # stat_area almost redundant but some vessels fished one area 2x in a week (20 instances)
  summarize(effort = sum(effort,na.rm=T),
            season = mean(season),
            week_season = mean(week_season),
            cpue = mean(cpue),
            week = mean(week),
            date_seq = min(date_seq),
            lat=mean(lat),
            lon=mean(lon)) %>%
  ungroup() %>% filter(season>2005) %>% arrange(vessel,date_seq)

#==== Join to Port
cpue<- cpue_dat_station %>% rename(cpue_date = date_seq) %>% mutate(cpue_date = as.Date(cpue_date))

# read in landing port data
landing_port = readxl::read_excel("data/ifq_crab_landings_port_01182022.xlsx", sheet = "data") %>% filter(CRAB_FISHERY_CODE=="BSS")
landing_port_date <- landing_port %>% rename(fish_date = FISHING_START_DATE, land_date = DATE_OF_LANDING,vessel=VESSEL_NAME) %>% 
  mutate(fish_date = as.Date(fish_date),land_date = as.Date(land_date))

port_coord = read.csv("data/port_coord.csv")

landing_port_sum = landing_port_date %>% group_by(vessel,land_date,PORT_NAME) %>% 
  summarise(LANDED_LBS=sum(LANDED_LBS,na.rm=T),fish_date=min(fish_date)) %>% left_join(port_coord)

# 10 dupes, most are split between port and stationary floating processor (8/10), usually 66/33% split
port_dupes = landing_port_sum %>% group_by(vessel,land_date) %>% 
  # Either calculate the weighted mean
  summarise(port_n=n())
landing_highest_port = landing_port_sum %>% group_by(vessel,land_date) %>% 
  
  # Either calculate the weighted mean
  # summarise(port_n=n(), 
  #           fish_date=fish_date,
  #           port_lat=weighted.mean(lat,w=LANDED_LBS),
  #           port_lon=weighted.mean(lon,w=LANDED_LBS))
  
  # Or just choose the highest landing port
  top_n(1, LANDED_LBS)

library(data.table)
# coerce to data.table and append join columns to preserve the original columns 
setDT(landing_highest_port)[, join_date := fish_date]
setDT(cpue)[, join_date := cpue_date]
# rolling join
cpue_port <- landing_highest_port[cpue, on = .(vessel, join_date), roll = "nearest"]

# calculate difference in catch 
cpue_port$catch_land_diff <- abs(cpue_port$land_date - cpue_port$cpue_date)

cpue_port_clean = cpue_port %>% filter(catch_land_diff<28)

# Calculate distance
library(geosphere)
cpue_port_clean$port_dist = distHaversine(cpue_port_clean[, c('lon', 'lat')], cpue_port_clean[, c('port_lon', 'port_lat')])/1000


cpue_port_clean_sfp = cpue_port_clean %>% mutate(port_dist = case_when(PORT_NAME == "Stationary Floating Processor" ~ 250,
                                                                       PORT_NAME == "Catcher/processor" ~ 0,
                                                 TRUE ~ port_dist))

cpue_port_clean_sfp_diff = cpue_port_clean_sfp %>% filter(catch_land_diff<22)

cpue_port_final = cpue_port_clean_sfp_diff %>% select(vessel, port_dist, cpue_date, stat_area)

write.csv(cpue_port_final,"data/trip_port_dist.csv")
