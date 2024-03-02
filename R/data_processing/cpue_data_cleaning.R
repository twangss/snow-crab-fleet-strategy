library(tidyverse)
library(stringr)

#### Read in cpue data
filepath <- "data/cpue_snow.csv" #rename col names to match script
cpue_dat<-read.csv(filepath)
colnames(cpue_dat)<-c("season","stat_area","effort","weight","number","cpue","avg_wt","week","vessel")

#==remove slashes for seasons...check this
#==the problem is that the season can straddle two years, so making sense of dynamics will need to consider that straddle
#==might need to check how early seasons are treated

cpue_dat <- cpue_dat %>%
  mutate(year = case_when(
    nchar(season) == 4 ~ as.numeric(season),
    nchar(season) != 4 & week < 40 ~ as.numeric(substr(season,1,4))+1,
    nchar(season) != 4 & week >= 40 ~ as.numeric(substr(season,1,4)), 
    TRUE ~ 9999))

cpue_dat$season[nchar(cpue_dat$season)>4]<-as.numeric(substr(cpue_dat$season[nchar(cpue_dat$season)>4],1,4))+1
cpue_dat$season<-as.numeric(cpue_dat$season)

#==== DATES
# build a table with dates and match this with ADFG definition of stat week
date_seq = seq(as.Date("1980-01-01"), as.Date("2023-12-31"), "day")
week_seq = as.numeric(strftime(date_seq, format = "%U")) 
month_seq = strftime(date_seq, format = "%m")
year_seq = strftime(date_seq, format = "%Y")
date_df = data.frame(date_seq=date_seq,week=week_seq,month=month_seq,year=year_seq) %>%
  # numeric col
  type.convert(as.is = TRUE) 

# find which years in which jan 1 is on a sunday and thus no need to add 1 to week bc there is no week 0
date_year_seq = seq(as.Date("1980-01-01"), as.Date("2023-12-31"), "year")
year_year_seq = as.numeric(strftime(date_year_seq, format = "%Y"))
wday_year_seq = wday(date_year_seq)
years_jan1_sunday = data.frame(date_year_seq,year_year_seq,wday_year_seq) %>% filter(wday_year_seq==1) %>% pull(year_year_seq)

# correct  strftime week to ADFG week
date_df_corrected = date_df %>% mutate(week = case_when(year %in% years_jan1_sunday ~ week,
                                    TRUE  ~ week + 1)) %>% 
                    # add 1 bc ADFG defines Jan 1 as week 1 while strftime defines first Sunday as week 1
  # get the first date of each week
  group_by(year,week) %>%  filter(row_number()==1)

# Join date table to weeks of CPUE data
cpue_dat = cpue_dat %>% left_join(date_df_corrected, by = c("week"="week","year"="year"))

# ALL THIS CODE IS OLD WEEK CALCULATIONS
# converts week-year to date
# cpue_dat$date_seq <- as.Date(paste(cpue_dat$year, cpue_dat$week, 1, sep="-"), "%Y-%U-%u")
# 
# 
# # except there a few NAs, why? because of week 53. Some week 53s are filled, but not all. 
# # I think the data collection mistakenly added week 53s for years that didn't have week 53s
# # Of the NAs, I changed the week 53s to 52s and then rereun the date_seq
# cpue_dat[is.na(cpue_dat$date_seq),"week"] <- 52
# cpue_dat$date_seq <- as.Date(paste(cpue_dat$year, cpue_dat$week, 1, sep="-"), "%Y-%U-%u") #2nd time around wont have NAs
# cpue_dat$month <- as.numeric(format(as.Date(cpue_dat$date_seq, format="%d/%m/%Y"),"%m"))
# 
# # these week calculations will have week 53 and week 1 to both become week_season 13, this creates duplicate week-month combos, 
# # join future tables with week_season-week-season to get month
# # you do this week conversion after the dates bc you altered weeks in the previous week chunk
cpue_dat <- cpue_dat %>%
  mutate(week_season = case_when(
      week >= 40 ~ week-40,
      week < 40 ~ week+12))

#==== SPATIAL: coordinates calculations
long <- round(as.numeric(str_c('-1',str_sub(cpue_dat$stat_area,1,2))),1)-0.5
# Lat are in sec : so I transform sec in degre (0.3sec=0.5 degree)
lat <-round(as.numeric(str_c(str_sub(cpue_dat$stat_area,3,4),'.',(round(as.numeric(str_sub(cpue_dat$stat_area,5,6))/6)*10))),1)+0.25
cpue_dat$lat<-plyr::round_any(lat,0.25)
cpue_dat$lon<-plyr::round_any(long, 0.5)

# set numeric col as numeric
cpue_dat = cpue_dat %>% mutate(number = as.numeric(number),weight = as.numeric(weight))

write.csv(cpue_dat, "data/cpue_snow_clean_2022.csv")


# The following is very clunky old code to get dates and months, I believe I found a better way up top but saving just in case
# #== add months to cpue_dat, this is very long code to do this....
# date_seq <- seq(as.Date("1985-01-01"), as.Date("2021-12-31"), "day")
# day <- strftime(date_seq, format = "%a")
# week <- strftime(date_seq, format = "%V")
# month <- strftime(date_seq, format = "%m")
# year <- strftime(date_seq, format = "%Y")
# 
# month_week_table <- as.data.frame((bind_cols(as.Date(date_seq),day, week, month, year)))
# colnames(month_week_table) <- c("date_seq","day","week","month","year")
# month_week_table$week <- as.numeric(month_week_table$week)
# month_week_table$month <- as.numeric(month_week_table$month)
# month_week_table$year <- as.numeric(month_week_table$year)
# # only get 1 day per week otherwise join table will have duplicates, this will still miss some week 1s and 53s
# month_week_table <- month_week_table %>% 
#   filter(day == "Wed")
# # I cannot anti-filter by 2 columns???? this is a tedious workaround that works
# bad_weeks <- month_week_table %>%
#   subset(week == 1 & month == 12)
# month_week_table <- month_week_table %>% 
#   anti_join(bad_weeks, by = c("year" = "year", "week" = "week"))
# 
# # finally add months to cpue_dat
# cpue_dat_month_test <- cpue_dat %>%
#   left_join(month_week_table, by = c("year" = "year", "week" = "week")) %>% arrange(year,week)
# cpue_dat_month$month[cpue_dat_month$week >= 52] <- 12
# cpue_dat_month$month[cpue_dat_month$week == 1] <- 1
# # READ NOTE about week_season=13 with both week 53 and week 1 thus both month 12 and month 1 above 


