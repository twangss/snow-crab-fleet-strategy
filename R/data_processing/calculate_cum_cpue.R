library(tidyverse)
library(Hmisc)
library(fauxnaif)

cpue_dat <-read.csv("data/cpue_snow_clean_2022.csv")
cpue_dat <- cpue_dat %>%
  #clean out strays
  filter(lat < 64 & lat > 53 & lon > -180 & lon < -155)

#==== Vessel Cumulative mean CPUE
cpue_vessel <- cpue_dat %>%
  # group_by(stat_area,season,week_season) %>% TOGGLE ON FOR BY SEASON
  group_by(season,week_season,vessel) %>%
  # stat_area almost redundant but some vessels fished one area 2x in a week (20 instances)
  dplyr::summarize(effort = sum(effort,na.rm=T),
            number=sum(number,na.rm=T),
            season = mean(season),
            week_season = mean(week_season),
            week = mean(week),
            n=n()) %>%
  ungroup()


vessel_cum_cpue_mean = cpue_vessel %>% group_by(vessel,season) %>% arrange(week_season) %>%
  mutate(cum_cpue = dplyr::lag(cumsum(number),n=1) / dplyr::lag(cumsum(effort),n=1)) %>% # add lag 
  group_by(vessel,season,week_season) %>% mutate(cpue_week1=sum(number)/sum(effort)) %>% # then add 1st fishing event data
  mutate(cum_cpue_fill =if_else(is.na(cum_cpue), cpue_week1, cum_cpue)) %>% # fill in lag with 1st fishing event data wit NA
  arrange(vessel,season,week_season) %>% select(-c(cum_cpue,cpue_week1))

write.csv(vessel_cum_cpue_mean,"data/vessel_cum_cpue_mean_2022.csv")

#==== Fleet-wide Cumulative mean CPUE
cpue_fleet <- cpue_dat %>%
  # group_by(stat_area,season,week_season) %>% TOGGLE ON FOR BY SEASON
  group_by(season,week_season) %>%
  # stat_area almost redundant but some vessels fished one area 2x in a week (20 instances)
  dplyr::summarize(effort = sum(effort,na.rm=T),
            number=sum(number,na.rm=T),
            season = mean(season),
            week_season = mean(week_season),
            week = mean(week),
            n=n()) %>%
  ungroup()

fleet_cum_cpue_mean = cpue_fleet %>% group_by(season) %>% arrange(week_season) %>%
  mutate(cum_cpue = stats::lag(cumsum(number),k=1) / stats::lag(cumsum(effort),k=1)) %>% # add lag 
  group_by(season,week_season) %>% mutate(cpue_week1=sum(number)/sum(effort)) %>% # then add 1st fishing event data
  mutate(cum_cpue_fill =if_else(is.na(cum_cpue), cpue_week1, cum_cpue)) %>% # fill in lag with 1st fishing event data wit NA
  arrange(season,week_season) %>% select(-c(cum_cpue,cpue_week1))

write.csv(fleet_cum_cpue_mean,"data/fleet_cum_cpue_mean_2022.csv")

#== Vessel Rolling Variance of CPUE
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


#=== Calculate Variance Cumulative Season for each vessel
# slow for loop but it works 
var_loop_table <- cpue_dat_station %>% select(vessel,date_seq,season) %>% distinct() %>% arrange(vessel,date_seq)
var_table_to_join <- NULL
for(i in 1:nrow(var_loop_table)){
  vess_tmp = pull(var_loop_table[i,1])
  # Beginning of season up to now
  last_date_tmp = as.Date(pull(var_loop_table[i,2]))
  first_date_season = as.Date(paste(var_loop_table[i,"season"]-1,"-10-01",sep=""),'%Y-%m-%d')
  
  # filter down to previous two weeks of fishing of the vessel
  cpue_tmp <- cpue_dat_station %>% filter(date_seq < last_date_tmp  & date_seq > first_date_season & 
                                            vessel == vess_tmp) %>%
    # then get single values for each stat area fished for past 2 weeks
    dplyr::summarise(cpuewt_cum = weighted.mean(cpue, effort,na.rm=T),
              sdwt_cum = sqrt(wtd.var(cpue, effort,na.rm=T)),
              cvrwt_cum = sdwt_cum/weighted.mean(cpue, effort,na.rm=T),
              n=n()) %>%
    mutate(vessel = vess_tmp,
           date_seq = as.Date(last_date_tmp),
           last_date_tmp = as.Date(last_date_tmp),
           first_date_season=as.Date(first_date_season))
  var_table_to_join <- rbind(var_table_to_join,cpue_tmp)
}
# var_table_to_join=trad_table_to_join
# for NaN and 0 variance, set as NA because there is either no value, or 0 means only 1 obs which cannot calculate var
var_table_to_join = var_table_to_join %>% mutate_at(c('sdwt_cum',"cvrwt_cum"), ~na_if_in(., c(0,NaN)))
# fill NA values up with most recent values previous (forward is better for model but ensure only previous data is known)
var_table_to_join = var_table_to_join %>% fill(c("sdwt_cum","cvrwt_cum"), .direction = "down")

write.csv(var_table_to_join,"data/cum_var_table_vessel_2022.csv")

#=== Calculate Variance Cumulative Season for fleet-wide
# slow for loop but it works 
var_loop_table <- cpue_dat_station %>% select(date_seq,season) %>% distinct() %>% arrange(date_seq)
var_table_to_join_fleetwide <- NULL
for(i in 1:nrow(var_loop_table)){
  # Beginning of season up to now
  last_date_tmp = as.Date(pull(var_loop_table[i,"date_seq"]))
  first_date_season = as.Date(paste(var_loop_table[i,"season"]-1,"-10-01",sep=""),'%Y-%m-%d')
  
  # filter down to previous two weeks of fishing of the vessel
  cpue_tmp <- cpue_dat_station %>% filter(date_seq < last_date_tmp  & date_seq > first_date_season) %>%
    # then get single values for each stat area fished for past 2 weeks
    dplyr::summarise(cpuewt_cum = weighted.mean(cpue, effort,na.rm=T),
              sdwt_cum = sqrt(wtd.var(cpue, effort,na.rm=T)),
              cvrwt_cum = sdwt_cum/weighted.mean(cpue, effort,na.rm=T),
              n=n()) %>%
    mutate(date_seq = as.Date(last_date_tmp),
           last_date_tmp = as.Date(last_date_tmp),
           first_date_season=as.Date(first_date_season))
  var_table_to_join_fleetwide <- rbind(var_table_to_join_fleetwide,cpue_tmp)
}
# for NaN and 0 variance, set as NA because there is either no value, or 0 means only 1 obs which cannot calculate var
var_table_to_join_fleetwide = var_table_to_join_fleetwide %>% mutate_at(c('sdwt_cum',"cvrwt_cum"), ~na_if_in(., c(0,NaN)))
# fill NA values up with most recent values previous (forward is better for model but ensure only previous data is known)
var_table_to_join_fleetwide = var_table_to_join_fleetwide %>% fill(c("sdwt_cum","cvrwt_cum"), .direction = "down")
write.csv(var_table_to_join_fleetwide,"data/cum_var_table_fleetwide_2022.csv")



