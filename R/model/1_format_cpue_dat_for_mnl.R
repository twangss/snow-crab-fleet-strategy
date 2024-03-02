library(tidyverse)

#==== Set up data 
cpue_dat <-read.csv("data/cpue_snow_clean_2022.csv") %>% select(-X)
cpue_dat <- cpue_dat %>%
  #clean out strays
  filter(lat < 64 & lat > 53 & lon > -180 & lon < -155)

#==== Set up CPUE data for analysis per year per stat area
cpue_dat_station <- cpue_dat %>%
  # group_by(stat_area,season,week_season) %>% TOGGLE ON FOR BY SEASON
  group_by(stat_area,season,week_season,vessel) %>%
  # stat_area almost redundant but some vessels fished one area 2x in a week (20 instances)
  summarise(effort = sum(effort,na.rm=T),
            season = mean(season),
            week_season = mean(week_season),
            cpue = mean(cpue),
            week = mean(week),
            date_seq = as.Date(min(as.Date(date_seq))),
            lat=mean(lat),
            lon=mean(lon),
            n=n()) %>%
  ungroup() %>% arrange(vessel,stat_area,week_season)

TheData = cpue_dat_station %>% filter(season > 2006) %>% arrange(vessel,season,week_season)


# subset to only high frequency stat_areas that are fished >5% of the time
low_area_freqs <- TheData %>%
  group_by(stat_area) %>%
  tally() %>%
  mutate(freq = n / sum(n)) %>%
  filter(freq <= 0.005)

cpue_small <- TheData %>%
  filter(!stat_area %in% low_area_freqs$stat_area)

# OR subset to vessels that fished for all seasons!

vessels_keep = TheData %>% ungroup() %>% distinct(vessel,season) %>%group_by(vessel) %>%
  summarise(n=n()) %>% filter(n>=max(n)-1)

cpue_small <- TheData %>%
  # filter(vessel %in% vessels_keep$vessel) %>%
  filter(!stat_area %in% low_area_freqs$stat_area)

unique(cpue_small$stat_area)
unique(cpue_small$vessel)

#=== Expand data for alternative choices
# summarise cpue for every stat_area-time combo because different vessels may have different cpue 
# this will be used to join to the expanded table

#===Calculate 2-week rolling mean CPUE for each stat area to fill in empty NA values
date_seq <- unique(as.Date(cpue_small$date_seq))
stat_area_list <- unique(cpue_small$stat_area)
cpue_rolling_mean <- NULL

for(i in as.list(date_seq)){
  for(j in stat_area_list){
    tmp <- cpue_small %>% filter(date_seq < i & date_seq > i - 15 & stat_area == j)
    cpue_mean_tmp <- mean(tmp$cpue,na.rm=T)
    effort_mean_tmp <- mean(tmp$effort,na.rm=T)
    row_tmp <- data.frame(j,i,cpue_mean_tmp,effort_mean_tmp,nrow(tmp))
    cpue_rolling_mean<-rbind(cpue_rolling_mean,row_tmp)
  }
}
colnames(cpue_rolling_mean) <- c("stat_area","date_seq","cpue_stat_m","effort_stat_m","n")

# add the weeks to rolling mean df
cpue_station_alt <- cpue_small %>% select(date_seq,season,week_season)%>%distinct()%>%right_join(cpue_rolling_mean,keep=F)

# combos to anti join (remove) observed instances
combos <- cpue_small %>% distinct(stat_area,week_season,season,vessel,date_seq)

# Toggle this for all potential fishing areas for that were fished in that time step 
# cpue_expand <- cpue_small %>% dplyr::group_by(season,week_season,cpue_date) %>% expand(stat_area,vessel) %>% ungroup() %>%
#   # this is to create all potential fishing areas for were fished in that step in time
#   inner_join(cpue_station_alt,by=c("stat_area"="stat_area","season"="season","week_season"="week_season","cpue_date"="cpue_date")) %>% 
#   arrange(vessel,season,week_season,stat_area) %>%
#   # set the not chosen alts as 0
#   mutate(choice = 0) %>% anti_join(combos) 

# Toggle this to get ALL other potneital non-fished areas' CPUEs for every vessel fishing event
all_stat_area = unique(cpue_small$stat_area)
cpue_expand <- cpue_small %>% dplyr::group_by(season,week_season,date_seq,vessel) %>% 
  expand(all_stat_area) %>% rename(stat_area = all_stat_area) %>% ungroup() %>%
  # this is to create all potential fishing areas for all time
  left_join(cpue_station_alt,by=c("stat_area"="stat_area","season"="season","week_season"="week_season","date_seq"="date_seq")) %>% 
  arrange(vessel,season,week_season,stat_area) %>%
  # set the not chosen alts as 0
  mutate(choice = 0) %>% anti_join(combos) 
# %>% replace(is.na(.), 0)

# alternates that are actually chosen (aka observed)
cpue_join <- cpue_small %>% select(stat_area,week_season,season,vessel,effort,cpue,date_seq) %>% mutate(choice = 1)

# Join potential area-time to observed area-time, replace empty cpue values with mean of other vessels
cpue_alt_expand <- cpue_expand %>% 
  full_join(cpue_join, 
   by = c("stat_area"="stat_area","season"="season","week_season"="week_season","vessel"="vessel","date_seq"="date_seq")) %>%
  mutate(cpue_x=if_else(is.na(cpue), cpue_stat_m, cpue),
         effort_x=ifelse(is.na(effort), effort_stat_m, effort),
         choice = ifelse(is.na(choice.x), choice.y, choice.x)) %>%
  # select(-c(cpue,cpue_m,effort,choice.x,choice.y)) %>%
  # # you removed effort_m from above
  # arrange(vessel,season,week_season,stat_area) %>%
  # # add index for choice opportunities
  group_by(vessel,date_seq) %>%
  mutate(choice_id = cur_group_id()) %>% arrange(choice_id)

#### JOIN
#==== CPUE & Covariance vessel
trad_table_to_join = read.csv("data/year0week0to4_lag_table_vessel_2022.csv") %>% select(-X) %>%
  rename(effort_lag2_v = effort_lag2,effort_lag2_prop_v = effort_lag2_prop, 
         cpue_lag2_v = cpue_lag2,cpuewt_lag2_v=cpuewt_lag2,
         cvrwt_lag2_v=cvrwt_lag2) %>%
  select(date_seq,stat_area,vessel,cpue_lag2_v,cpuewt_lag2_v,cvrwt_lag2_v) %>% 
  mutate(date_seq = as.Date(date_seq))

cpue_alt_expand_v = cpue_alt_expand %>% left_join(trad_table_to_join, c("stat_area"="stat_area","date_seq"="date_seq","vessel"="vessel"))
sum(is.na(cpue_alt_expand_v$cpuewt_lag2_v))/length(cpue_alt_expand_v$cpuewt_lag2_v)
# EH only 10% of records are filled out with vessel specific CPUE at 4 weeks with 32 areas

# fill in with cumulative CPUE
# NO NEED RIGHT???
vessel_cum_cpue_mean = read.csv("data/vessel_cum_cpue_mean_2022.csv") %>% select(-X) %>% select(season,week_season,vessel,cum_cpue_fill)
cpue_alt_expand_v_fill = cpue_alt_expand_v %>% left_join(vessel_cum_cpue_mean,c("season"="season","week_season"="week_season","vessel"="vessel")) %>%
  # you updated cpue_lag2_v to be cpuewt_lag2_v, maybe change back if model estimate gets worse
  mutate(cpue_lag2_v_fill=if_else(is.na(cpuewt_lag2_v), cum_cpue_fill, cpuewt_lag2_v)) %>% select(-c(cum_cpue_fill,cpue_lag2_v))
sum(is.na(cpue_alt_expand_v_fill$cpue_lag2_v_fill))/length(cpue_alt_expand_v_fill$cpue_lag2_v_fill)

# fill in with cum covariance
vessel_cum_var = read.csv("data/cum_var_table_vessel_2022.csv") %>% select(date_seq,vessel,cvrwt_cum) %>% mutate(date_seq=as.Date(date_seq))
cpue_alt_expand_v_fill_cvr = cpue_alt_expand_v_fill %>% left_join(vessel_cum_var,c("date_seq"="date_seq","vessel"="vessel")) %>%
  mutate(cvrwt_lag2_v_fill=if_else(is.na(cvrwt_lag2_v), cvrwt_cum, cvrwt_lag2_v)) %>% select(-c(cvrwt_cum,cvrwt_lag2_v))
sum(is.na(cpue_alt_expand_v_fill_cvr$cvrwt_lag2_v_fill))/length(cpue_alt_expand_v_fill_cvr$cvrwt_lag2_v_fill)

#==== CPUE & Covariance fleet
trad_table_to_join_f = read.csv("data/year0week0to2_lag_table_fleet_2022.csv") %>% select(-X) %>%
  rename(effort_lag2_f = effort_lag2,effort_lag2_prop_f = effort_lag2_prop, 
         cpue_lag2_f = cpue_lag2, cpuewt_lag2_f=cpuewt_lag2,
         cvrwt_lag2_f=cvrwt_lag2) %>%
  mutate(date_seq = as.Date(date_seq))

cpue_alt_expand_f = cpue_alt_expand_v_fill_cvr %>% left_join(trad_table_to_join_f, c("stat_area"="stat_area","date_seq"="date_seq"))
# 50% records are filled...
sum(is.na(cpue_alt_expand_f$cpue_lag2_f))/length(cpue_alt_expand_f$cpue_lag2_f)

# fill in with cumulative CPUE
fleet_cum_cpue_mean = read.csv("data/fleet_cum_cpue_mean_2022.csv") %>% select(-X) %>% select(season,week_season,cum_cpue_fill)
cpue_alt_expand_f_fill = cpue_alt_expand_f %>% left_join(fleet_cum_cpue_mean,c("season"="season","week_season"="week_season")) %>%
  mutate(cpue_lag2_f_fill=if_else(is.na(cpuewt_lag2_f), cum_cpue_fill, cpuewt_lag2_f))%>% select(-c(cum_cpue_fill,cpuewt_lag2_f))
sum(is.na(cpue_alt_expand_f_fill$cpue_lag2_f_fill))/length(cpue_alt_expand_f_fill$cpue_lag2_f_fill)

# fill in with cum covariance
fleet_cum_var = read.csv("data/cum_var_table_fleetwide_2022.csv") %>% select(date_seq,cvrwt_cum) %>% mutate(date_seq=as.Date(date_seq))
cpue_alt_expand_f_fill_cvr = cpue_alt_expand_f_fill %>% left_join(fleet_cum_var,c("date_seq"="date_seq")) %>%
  mutate(cvrwt_lag2_f_fill=if_else(is.na(cvrwt_lag2_f), cvrwt_cum, cvrwt_lag2_f)) %>% select(-c(cvrwt_cum,cvrwt_lag2_f))
sum(is.na(cpue_alt_expand_f_fill_cvr$cvrwt_lag2_f_fill))/length(cpue_alt_expand_f_fill_cvr$cvrwt_lag2_f_fill)

#==== Tradition
trad_lag2 = read.csv("data/year0week0to2_lag_table_vessel_2022.csv")%>% select(-X)
trad_join = trad_lag2 %>% select(stat_area,vessel,effort_lag2_prop,date_seq) %>% mutate(date_seq=as.Date(date_seq))
cpue_alt_trad <- cpue_alt_expand_f_fill_cvr %>% left_join(trad_join) %>% 
  mutate_at(vars(effort_lag2_prop), ~replace_na(., 0))
# the replace na with 0

#==== Ice
ice_area_grid <- read.csv("data/weekly_ice_area_per_grid_cell.csv")%>% select(-X)
ice_area_grid <- ice_area_grid %>% rename(ice_date = date) %>% mutate(ice_date = as.Date(ice_date,format="%m/%d/%y"))

library(data.table)
# coerce to data.table and append join columns to preserve the original columns 
setDT(ice_area_grid)[, join_date := ice_date]
setDT(cpue_alt_trad)[, join_date := date_seq]
# rolling join
cpue_trad_ice <- ice_area_grid[cpue_alt_trad, on = .(stat_area, join_date), roll = Inf]

# set ice prop as NA if the ice date is over 3 weeks away
cpue_trad_ice$ice_time_diff <- abs(cpue_trad_ice$ice_date - cpue_trad_ice$date_seq)
cpue_trad_ice[cpue_trad_ice$ice_time_diff > 28*2,"ice_prop"] <- NA
cpue_trad_ice$ice_time_diff
# Set late summer to 0s
cpue_trad_ice$join_month = as.numeric(format(cpue_trad_ice$date_seq,"%m"))

cpue_trad_ice[cpue_trad_ice$join_month > 4,"ice_prop"] <- 0

sum(is.na(cpue_trad_ice$ice_prop))/length(cpue_trad_ice$ice_prop)

#==== Sorting
sorting_season = read.csv("data/sorting_by_season.csv") %>% select(-X) %>% select(statarea,season,legal_prop)
cpue_sorting = cpue_trad_ice %>% left_join(sorting_season, by = c("stat_area"="statarea","season"="season"))
# deal with NAs by filling in with average of the season
sorting_season_avg = sorting_season %>% group_by(season) %>% summarise(season_sort_avg = mean(legal_prop,na.rm=T))
cpue_sorting_na = cpue_sorting %>% left_join(sorting_season_avg) %>% 
  mutate(legal_prop_fill=if_else(is.na(legal_prop), season_sort_avg, legal_prop))%>%
  select(-c(legal_prop,season_sort_avg,legal_prop))

#==== Survey Density
cpue_survey_tbl=read.csv("data/cpue_survey_season.csv") %>% select(-X)
cpue_survey_tbl_season=cpue_survey_tbl %>% mutate(season=year+1) %>% select(-year)
cpue_sort_surv = cpue_sorting_na %>% left_join(cpue_survey_tbl_season,by = c("stat_area"="stat_area","season"="season"))

#==== Previous Season CPUE
cpue_lagseason1_df =cpue_dat_station %>% group_by(season,stat_area) %>%
  summarise(cpue_lagseason1_with_NA=weighted.mean(cpue, effort,na.rm=T),
            season=season+1) %>% distinct()
# fill NA with mean of the season
cpue_lagseason1_to_fill_NA = cpue_lagseason1_df %>% group_by(season) %>% summarise(cpue_lagseason1_NA_value = mean(cpue_lagseason1_with_NA,na.rm=T))

cpue_sort_surv_lag=cpue_sort_surv %>% left_join(cpue_lagseason1_df,by = c("stat_area", "season")) %>%
  left_join(cpue_lagseason1_to_fill_NA,by = c("season")) %>% 
  mutate(cpue_lagseason1=if_else(is.na(cpue_lagseason1_with_NA), cpue_lagseason1_NA_value, cpue_lagseason1_with_NA)) %>%
  select(-c(cpue_lagseason1_NA_value,cpue_lagseason1_with_NA))

sum(is.na(cpue_sort_surv_lag$cpue_lagseason1))/length(cpue_sort_surv_lag$cpue_lagseason1)

#==== Congestion
n_other_vess_seas = TheData %>% group_by(season) %>%
  summarise(n_other_vess_seas = length(unique(vessel))-1)

# congestion = cpue_dat_station %>% group_by(season,stat_area,date_seq) %>%
#   summarise(cong=n()-1,
#             tot_eff=sum(effort,na.rm=T)) 
# 

# cpue_trad_ice_cpue_cong = cpue_sort_surv_lag %>% 
#   left_join(congestion,by = c("stat_area", "season","date_seq")) %>%
#   left_join(n_other_vess_seas,by = c("season")) %>%
#   mutate_at(vars(effort,cong,tot_eff), ~replace_na(., 0)) %>%
#   mutate(other_eff_per_vess = (tot_eff-effort)/n_other_vess_seas)

#=== Congestion Lag
cong_lag2 =read.csv("data/year0week0to2_lag_table_vessel_2022.csv") %>% 
  select(stat_area,date_seq,vessel,cong_lag2) %>% mutate(date_seq=as.Date(date_seq))

cpue_trad_ice_cpue_cong=cpue_sort_surv_lag%>% 
  left_join(cong_lag2,by = c("stat_area"="stat_area","vessel"="vessel","date_seq"="date_seq")) %>%
  #replace cong with cong_lag 
  left_join(n_other_vess_seas,by = c("season")) %>%
  mutate(other_eff_per_vess = cong_lag2/n_other_vess_seas) %>%
  mutate_at(vars(other_eff_per_vess), ~replace_na(., 0))

#==== Distance to Port
# read in landing port data
landing_port = readxl::read_excel("data/ifq_crab_landings_port_01182022.xlsx", sheet = "data") %>% filter(CRAB_FISHERY_CODE=="BSS")
landing_port_date <- landing_port %>% rename(fish_date = FISHING_START_DATE, land_date = DATE_OF_LANDING,vessel=VESSEL_NAME) %>% 
  mutate(fish_date = as.Date(fish_date),land_date = as.Date(land_date))

port_coord = read.csv("data/port_coord.csv")

landing_port_sum = landing_port_date %>% group_by(vessel,land_date,PORT_NAME) %>% 
  summarise(LANDED_LBS=sum(LANDED_LBS,na.rm=T),fish_date=min(fish_date)) %>% left_join(port_coord)

landing_highest_port = landing_port_sum %>% group_by(vessel,land_date) %>% 
  
  # Either calculate the weighted mean
  # summarise(port_n=n(), 
  #           fish_date=fish_date,
  #           port_lat=weighted.mean(lat,w=LANDED_LBS),
  #           port_lon=weighted.mean(lon,w=LANDED_LBS))
  
  # Or just choose the highest landing port
  top_n(1, LANDED_LBS) %>% ungroup()%>%
  select(vessel,fish_date,land_date,PORT_NAME,port_lat,port_lon)

library(data.table)
# coerce to data.table and append join columns to preserve the original columns 
setDT(landing_highest_port)[, join_date := land_date]
setDT(cpue_trad_ice_cpue_cong)[, join_date := date_seq]
# rolling join
cpue_port <- landing_highest_port[cpue_trad_ice_cpue_cong, on = .(vessel, join_date), roll = "nearest"]

# calculate difference in catch 
cpue_port$catch_land_diff <- abs(cpue_port$land_date - cpue_port$date_seq)

cpue_port_clean_diff = cpue_port %>% filter(catch_land_diff<22)

# Calculate distance
library(geosphere)
stat_area_coord <- cpue_dat %>%group_by(stat_area) %>% summarise(lat=mean(lat),lon=mean(lon)) 
cpue_port_clean = cpue_port_clean_diff %>% left_join(stat_area_coord)
cpue_port_clean$port_dist = distHaversine(cpue_port_clean[, c('lon', 'lat')], cpue_port_clean[, c('port_lon', 'port_lat')])/1000
cpue_port_clean_sfp = cpue_port_clean %>% mutate(port_dist = case_when(PORT_NAME == "Stationary Floating Processor" ~ 250,
                                                                       PORT_NAME == "Catcher/processor" ~ 0,
                                                                       TRUE ~ port_dist))
prop_trips_to_port = cpue_port_clean_sfp %>% filter(choice==1) %>% select(PORT_NAME,choice_id) %>%
  distinct() %>% group_by(PORT_NAME) %>% summarise(n=n())

#==== Windspeed 
# read in landing port data
wind_area_grid = read.csv("data/weekly_wind_area_per_grid_cell_2022.csv") %>% mutate(last_date=as.Date(last_date))
cpue_port_clean_sfp_wind = cpue_port_clean_sfp %>% left_join(wind_area_grid,by=c("date_seq" = "last_date", "stat_area"="stat_area"))

#==== Survey Offset 
# read in landing port data
survey_offset_df = read.csv("data/cpue_survey_season_offset.csv") %>% mutate(Season_of_survey = Year + 1)
cpue_port_clean_sfp_wind_survoff_with_NA =cpue_port_clean_sfp_wind %>% left_join(survey_offset_df,by=c("season" = "Season_of_survey", "stat_area"="stat_area"))

# fill NA with mean of the season
surv_offset_to_fill_NA = survey_offset_df %>% group_by(Season_of_survey) %>% summarise(surv_offset_NA_value = mean(surv_offset,na.rm=T))

cpue_port_clean_sfp_wind_survoff_fill=cpue_port_clean_sfp_wind_survoff_with_NA %>% left_join(surv_offset_to_fill_NA,by = c("season" = "Season_of_survey")) %>%
  mutate(surv_offset_fill=if_else(is.na(surv_offset), surv_offset_NA_value, surv_offset)) %>%
  select(-c(surv_offset,surv_offset_NA_value))

sum(is.na(cpue_port_clean_sfp_wind_survoff_fill$surv_offset_fill))/length(cpue_port_clean_sfp_wind_survoff_fill$surv_offset_fill)
  
#==== Save this all out before wide pivoting
write.csv(cpue_port_clean_sfp_wind_survoff_fill,"data/cpue_alt_covariate_prewide_2023_0229.csv")

#==== Aggregate Data to Spatial Grid, IGNORE if STAT_AREA and skip to next chunk, otherwise STOP HERE
# cpue_grid_k = read.csv("multinomial_logit/cpue_grid_clusterk_df.csv")
# for (k in 3:10){
# num_clust=k
# grid_k=cpue_grid_k %>% filter(k==num_clust) %>% select(stat_area,cluster_id)
# cpue_set_right0_k = cpue_set_right0 %>% left_join(grid_k)
# 
# cpue_set_right0_k_grid =cpue_set_right0_k %>% group_by(cluster_id,choice_id) %>%
#   summarise(vessel=vessel,
#             date_seq=date_seq,
#             effort_lag2_prop=sum(effort_lag2_prop,na.rm=T),
#             cpue_lag2=mean(cpue_lag2,na.rm=T),
#             cpue_x=mean(cpue_x,na.rm=T),
#             ice_prop=sum(ice_area,na.rm=T)/sum(grid_cell_area,na.rm=T),
#             other_eff_per_vess=sum(other_eff_per_vess,na.rm=T),
#             cpue_lagseason1=mean(cpue_lagseason1,na.rm=T)) %>% arrange(choice_id) %>%
#   distinct() # you need this since vessel and date_seq are strings or something
# 
# #==== Pivot out
# 
# # pivots covariate values to columns-per-stat_area
# # does not keep the multiple stat_area choices, need to join after this chunk
# cpue_wide <- cpue_set_right0_k_grid %>% ungroup() %>% 
#   select(date_seq,vessel,cluster_id,choice_id,effort_lag2_prop,cpue_lag2,cpue_x,ice_prop,other_eff_per_vess,
#          cpue_lagseason1) %>%
#   pivot_wider(names_from = cluster_id,
#               values_from = c(effort_lag2_prop,cpue_x,ice_prop,cpue_lag2,
#                               cpue_lagseason1,other_eff_per_vess))
# 
# # head(cpue_trad_ice$stat_area,25)
# 
# # Add a column that shows which of the multiple areas vessels were chosen, the wide table already has all the areas 
# choice_join <- cpue_alt_trad %>% filter(choice == 1) %>% select(choice_id,stat_area) %>%
#   # below is for different grid
#   left_join(grid_k) %>% select(choice_id,cluster_id) %>% distinct() 
# 
# cpue_wide_choice <- choice_join %>% left_join(cpue_wide)
# 
# #==== Add Effort Weighting to each choice
# weight_eff=TheData %>% select(date_seq,vessel,stat_area,effort) %>%
#   # below is for diffeent grid
#   left_join(grid_k) %>% group_by(cluster_id,vessel,date_seq) %>% summarise(effort=sum(effort,na.rm=T))
# 
# cpue_wide_choice=cpue_wide_choice %>% left_join(weight_eff,by=c("vessel"="vessel","date_seq"="date_seq","cluster_id"="cluster_id"),keep=F)
# 
# #==== Set vessels names as numeric 
# # vessel_fac = as.factor(cpue_wide_choice$vessel)
# # cpue_wide_choice$vessel_num = unclass(vessel_fac)
# 
# # drop rows with any NA values
# # cpue_wide_choice_drop_na=cpue_wide_choice %>% drop_na(-c(cpue_x_675530,cpue_x_675600,cpue_x_685600,cpue_x_685630,cpue_x_715630,
# #                                 cpue_x_715700,cpue_x_725630,cpue_x_725700,cpue_x_725730,cpue_x_735700,
# #                                 cpue_x_735730,cpue_x_735800,cpue_x_745830))
# # sort(all_stat_area)
# # cpue_wide_choice_drop_na <- cpue_wide_choice[rowSums(is.na(cpue_wide_choice)) == 0,]
# file_name = paste("multinomial_logit/clustered_mnl_wide/cpue_mnl_wide_13areas_",num_clust,"clust.csv",sep="")
# write.csv(cpue_wide_choice,file_name)
# }

#==== Pivot out FOR STAT AREA ONLY
# pivots covariate values to columns-per-stat_area
# does not keep the multiple stat_area choices, need to join after this chunk

# cpue_port_clean_sfp_wind_survoff_fill = read.csv("data/cpue_alt_covariate_prewide_2023_0922.csv") %>% select(-X)

cpue_wide <- cpue_port_clean_sfp_wind_survoff_fill %>% ungroup() %>% 
  select(season,date_seq,vessel,stat_area,choice_id,effort_lag2_prop,cpue_lag2_f_fill,cpue_lag2_v_fill,
         cvrwt_lag2_f_fill,cvrwt_lag2_v_fill, cpue_lagseason1,
         legal_prop_fill,ice_prop,other_eff_per_vess,cpue_lagseason1, port_dist,mean_surv,windspeed_grid_m,surv_offset_fill) %>%
  pivot_wider(names_from = stat_area,
              values_from = c(effort_lag2_prop,cpue_lag2_f_fill,cpue_lag2_v_fill,
                              cpue_lagseason1,legal_prop_fill,ice_prop,other_eff_per_vess,cpue_lagseason1, port_dist,mean_surv,
                              cvrwt_lag2_f_fill,cvrwt_lag2_v_fill,windspeed_grid_m,surv_offset_fill))

# head(cpue_trad_ice$stat_area,25)

# Add a column that shows which of the multiple areas vessels were chosen, the wide table already has all the areas 
choice_join <- cpue_alt_trad %>% filter(choice == 1) %>% select(choice_id,stat_area) %>%
  distinct() 

cpue_wide_choice <- choice_join %>% left_join(cpue_wide)

#==== Add Effort Weighting to each choice
weight_eff=TheData %>% select(date_seq,vessel,stat_area,effort) %>%
 group_by(stat_area,vessel,date_seq) %>% summarise(effort=sum(effort,na.rm=T))

cpue_wide_choice_eff=cpue_wide_choice %>% 
  left_join(weight_eff,by=c("vessel"="vessel","date_seq"="date_seq","stat_area"="stat_area"),keep=F) %>%
  group_by(choice_id) %>% mutate(prop_eff_cid = effort/sum(effort,na.rm=T))

#==== Set vessels names as numeric 
vessel_fac = as.factor(cpue_wide_choice_eff$vessel)
cpue_wide_choice_eff$vessel_num = unclass(vessel_fac)

# drop rows with any NA values
# cpue_wide_choice_drop_na=cpue_wide_choice %>% drop_na(-c(cpue_x_675530,cpue_x_675600,cpue_x_685600,cpue_x_685630,cpue_x_715630,
#                                 cpue_x_715700,cpue_x_725630,cpue_x_725700,cpue_x_725730,cpue_x_735700,
#                                 cpue_x_735730,cpue_x_735800,cpue_x_745830))
# sort(all_stat_area)
# cpue_wide_choice_drop_na <- cpue_wide_choice[rowSums(is.na(cpue_wide_choice)) == 0,]
file_name = paste("data/cpue_mnl_wide_",length(unique(cpue_wide_choice$stat_area)),"areas_",
                  length(unique(cpue_wide_choice$vessel)),"vessels_statarea_2023_0922.csv",sep="")
write.csv(cpue_wide_choice_eff,file_name)

TheD_full=read.csv("data/cpue_mnl_wide_32areas_108vessels_statarea.csv")
