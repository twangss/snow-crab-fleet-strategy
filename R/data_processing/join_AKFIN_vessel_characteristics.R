library(readxl)
library(ggplot2)
library(tidyverse)
library(ggridges)

vessel_char = read_excel("data/Wang_bss_vessel_characteristics_07282023.xlsx",sheet="DATA")

vessel_2003 = vessel_char %>% filter(YEAR > 2003)

# Indvidual vessels are NOT changing from 2003 to now
ggplot(vessel_2003,aes(y=AKFIN_LENGTH,x=YEAR,group=CFEC_VESSEL_NAME,color=CFEC_VESSEL_NAME)) + geom_line() + geom_point()+
  theme(legend.position="none")
ggplot(vessel_2003,aes(y=AKFIN_HPOWER,x=YEAR,group=CFEC_VESSEL_NAME,color=CFEC_VESSEL_NAME)) + geom_line() + geom_point()+
  theme(legend.position="none")
ggplot(vessel_2003,aes(y=AKFIN_FUEL_CAPACITY,x=YEAR,group=CFEC_VESSEL_NAME,color=CFEC_VESSEL_NAME)) + geom_line() + geom_point()+
  theme(legend.position="none")
ggplot(vessel_2003,aes(y=AKFIN_HOLD,x=YEAR,group=CFEC_VESSEL_NAME,color=CFEC_VESSEL_NAME)) + geom_line() + geom_point()+
  theme(legend.position="none")


# How is the fleet changing over time? 
# In general the smaller vessels leave
ggplot(vessel_2003, aes(x = AKFIN_LENGTH, y = YEAR, group = YEAR)) + 
  geom_density_ridges() 

# smaller fuel capacity vessels leave
ggplot(vessel_2003, aes(x = AKFIN_FUEL_CAPACITY, y = YEAR, group = YEAR)) + 
  geom_density_ridges() 

# smaller hold vessels leave
ggplot(vessel_2003, aes(x = AKFIN_HOLD, y = YEAR, group = YEAR)) + 
  geom_density_ridges() 

# low hpower vessels leave
ggplot(vessel_2003, aes(x = AKFIN_HPOWER, y = YEAR, group = YEAR)) + 
  geom_density_ridges() 

# They are all correlated with each other
ggplot(vessel_2003) + geom_point(aes(x = AKFIN_LENGTH, y = AKFIN_FUEL_CAPACITY, color = YEAR))
ggplot(vessel_2003) + geom_point(aes(x = AKFIN_LENGTH, y = AKFIN_HPOWER, color = YEAR))
ggplot(vessel_2003) + geom_point(aes(x = AKFIN_LENGTH, y = AKFIN_HOLD, color = YEAR))
ggplot(vessel_2003) + geom_point(aes(x = AKFIN_FUEL_CAPACITY, y = AKFIN_HPOWER, color = YEAR))

# CPUE dat
cpue_dat <-read.csv("data/cpue_snow_clean.csv")
cpue_dat <- cpue_dat %>%
  #clean out strays
  filter(lat < 64 & lat > 53 & lon > -180 & lon < -155)
cpue_dat_station <- cpue_dat %>%
  # group_by(stat_area,season,week_season) %>% TOGGLE ON FOR BY SEASON
  group_by(stat_area,season,week_season,vessel) %>%
  # stat_area almost redundant but some vessels fished one area 2x in a week (20 instances)
  dplyr::summarize(effort = sum(effort,na.rm=T),
                   season = mean(season),
                   week_season = mean(week_season),
                   cpue = mean(cpue),
                   week = mean(week),
                   date_seq = min(date_seq)) %>%
  ungroup() %>% filter(season>2003)


cpue_vess_season = cpue_dat_station %>% select(vessel,season) %>% distinct() %>% arrange(vessel,season)


joined = cpue_vess_season %>% left_join(vessel_2003,by=c('season'='YEAR', 
                                         'vessel'='CFEC_VESSEL_NAME'))


good_vessel = joined %>% drop_na(vessel) %>% arrange(vessel) %>%pull(vessel) %>% unique() 

bad_vessel =  joined %>% filter(is.na(vessel))%>% arrange(vessel) %>% pull(vessel)%>% unique() 

# which vessels are not accounted for
setdiff(bad_vessel,good_vessel)

# actually join TEST!! youll join after the model runs
# just get the latest specs for vessel bc theres not much change
vess_1year = vessel_2003 %>% group_by(CFEC_VESSEL_NAME) %>% filter(YEAR==max(YEAR))

cpue_dat_vess = cpue_dat_station %>% left_join(vess_1year,by=c('vessel'='CFEC_VESSEL_NAME')) %>% 
  filter(vessel!="")
# ok none is missing
sum(is.na(cpue_dat_vess$AKFIN_LENGTH))/length(cpue_dat_vess$AKFIN_LENGTH)


vess_AKFIN = cpue_dat_vess %>% select(vessel,AKFIN_LENGTH,AKFIN_NET_TONS,AKFIN_GROSS_TONS,AKFIN_HOLD,AKFIN_HPOWER,AKFIN_HPOWER,AKFIN_FUEL_CAPACITY) %>%
  distinct()

# write.csv(vess_AKFIN,"data/AKFIN_vess_specs.csv",row.names = F)
