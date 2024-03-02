library(tidyverse)
library(sf)
library(Hmisc)
library(fauxnaif)

cpue_dat <-read.csv("data/cpue_snow_clean_2022.csv")
cpue_dat <- cpue_dat %>%
  #clean out strays
  filter(lat < 64 & lat > 53 & lon > -180 & lon < -155)

#==== Set up CPUE data for analysis per year per stat area
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

#=== Calculate Tradition, CPUE lag, Variance lag, and Congestion for Previous 2-week records for each vessel
# slow for loop but it works 
trad_loop_table <- cpue_dat_station %>% select(vessel,date_seq) %>% distinct() %>% arrange(vessel,date_seq)
trad_table_to_join <- NULL
for(i in 1:nrow(trad_loop_table)){
  vess_tmp = pull(trad_loop_table[i,1])
  # Toggle these on and off depending on time frame of tradition
      # past 2 weeks
          last_date_tmp = as.Date(pull(trad_loop_table[i,2]))
          first_date_tmp = as.Date(last_date_tmp)-15
      # past 4 weeks
          # last_date_tmp = as.Date(pull(trad_loop_table[i,2]))
          # first_date_tmp = as.Date(last_date_tmp)-29
      # past 2 weeks of last year
          # last_date_tmp = as.Date(pull(trad_loop_table[i,2]))-365
          # first_date_tmp = as.Date(last_date_tmp)-15
      # past 4 weeks of last year
          # last_date_tmp = as.Date(pull(trad_loop_table[i,2]))-365
          # first_date_tmp = as.Date(last_date_tmp)-29
 
  # filter down to previous two weeks of fishing of the vessel
  cpue_tmp <- cpue_dat_station %>% filter(date_seq < last_date_tmp  & date_seq > first_date_tmp & 
                                            vessel == vess_tmp) %>%
    # then get single values for each stat area fished for past 2 weeks
    group_by(stat_area) %>%
    summarise(effort_lag2 = sum(effort, na.rm=T),
              cpue_lag2 = mean(cpue,na.rm=T),
              cpuewt_lag2 = weighted.mean(cpue, effort),
              sdwt_lag2 = sqrt(wtd.var(cpue, effort)),
              cvrwt_lag2 = sdwt_lag2/cpue_lag2,
              n=n()) %>%
    ungroup() %>%
    mutate(effort_lag2_prop = effort_lag2/sum(effort_lag2,na.rm=T),
           vessel = vess_tmp,
           date_seq = as.Date(pull(trad_loop_table[i,2])),
           last_date_tmp = as.Date(last_date_tmp),
           first_date_tmp=as.Date(first_date_tmp))
  
  #congestion
  cpue_tmp_fleet_cong <- cpue_dat_station %>% filter(date_seq < last_date_tmp  & date_seq > first_date_tmp) %>%
    # then get single values for each stat area fished for past 2 weeks
    group_by(stat_area) %>%
    summarise(fleet_effort_lag2 = sum(effort, na.rm=T))
  cpue_tmp = cpue_tmp %>% left_join(cpue_tmp_fleet_cong) %>% mutate(cong_lag2=fleet_effort_lag2-effort_lag2)
  
  # log data
  trad_table_to_join <- rbind(trad_table_to_join,cpue_tmp)
}
trad_table_to_join = trad_table_to_join %>% mutate_at(c('sdwt_lag2',"cvrwt_lag2"), ~na_if_in(., c(0,NaN)))
write.csv(trad_table_to_join,"data/year0week0to2_lag_table_vessel_2022.csv")

# trad_table_to_join = read.csv("data/year0week0to4_lag_table_vessel_2022.csv")
# sum(trad_table_to_join$n > 1)/nrow(trad_table_to_join)

#=== Calculate Tradition, CPUE lag, and Variance lag for Previous 2-week records for fleet wide
# slow for loop but it works 
trad_loop_table <- cpue_dat_station %>% select(date_seq) %>% distinct() %>% arrange(date_seq)
trad_table_to_join_fleetwide <- NULL
for(i in 1:nrow(trad_loop_table)){
  # vess_tmp = pull(trad_loop_table[i,1])
  # Toggle these on and off depending on time frame of tradition
  # past 2 weeks
  last_date_tmp = as.Date(pull(trad_loop_table[i,1]))
  first_date_tmp = as.Date(last_date_tmp)-15
  # past 4 weeks
  # last_date_tmp = as.Date(pull(trad_loop_table[i,2]))
  # first_date_tmp = as.Date(last_date_tmp)-29
  # past 2 weeks of last year
  # last_date_tmp = as.Date(pull(trad_loop_table[i,2]))-365
  # first_date_tmp = as.Date(last_date_tmp)-15
  # past 4 weeks of last year
  # last_date_tmp = as.Date(pull(trad_loop_table[i,2]))-365
  # first_date_tmp = as.Date(last_date_tmp)-29
  
  # filter down to previous two weeks of fishing of the fleet
  cpue_tmp <- cpue_dat_station %>% filter(first_date_tmp < date_seq & date_seq < last_date_tmp) %>%
    # then get single values for each stat area fished for past 2 weeks
    group_by(stat_area) %>%
    summarise(effort_lag2 = sum(effort, na.rm=T),
              cpue_lag2 = mean(cpue,na.rm=T),
              cpuewt_lag2 = weighted.mean(cpue, effort),
              sdwt_lag2 = sqrt(wtd.var(cpue, effort)),
              cvrwt_lag2 = sdwt_lag2/cpue_lag2,
              n=n()) %>%
    ungroup() %>%
    mutate(effort_lag2_prop = effort_lag2/sum(effort_lag2,na.rm=T),
           date_seq = as.Date(pull(trad_loop_table[i,1])),
           last_date_tmp = as.Date(last_date_tmp),
           first_date_tmp=as.Date(first_date_tmp))
  trad_table_to_join_fleetwide <- rbind(trad_table_to_join_fleetwide,cpue_tmp)
}
trad_table_to_join_fleetwide = trad_table_to_join_fleetwide %>% mutate_at(c('sdwt_lag2',"cvrwt_lag2"), ~na_if_in(., c(0,NaN)))
write.csv(trad_table_to_join_fleetwide,"data/year0week0to2_lag_table_fleet_2022.csv")

trad_table_to_join_fleetwide = read.csv("data/year0week0to2_lag_table_fleet_2022.csv")
1-sum(trad_table_to_join_fleetwide$n > 1)/nrow(trad_table_to_join_fleetwide)
