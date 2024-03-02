library(tidyverse)
library(sf)
BKC = read.csv("data/observer_data/BKC-1990-2021_potsum.csv") %>% 
  select(fishery,sampdate,statarea,female,sublegal,legal_ret,legal_nr,legal_ur,tot_legal,latitude,longitude,trip) %>%
  mutate(sp = "bkc")
BST = read.csv("data/observer_data/TANNER-1990-2021_potsum.csv") %>% 
  select(fishery,sampdate,statarea,female,sublegal,legal_ret,legal_nr,legal_ur,tot_legal,latitude,longitude,trip) %>%
  mutate(sp = "bst")
BSS = read.csv("data/observer_data/SNOWCRAB-1990-2021_potsum.csv") %>% 
  select(fishery,sampdate,statarea,female,sublegal,legal_ret,legal_nr,legal_ur,tot_legal,latitude,longitude,trip) %>%
  mutate(sp = "bss")

BSAI = rbind(BKC,BST,BSS)

# convert fishery codes and filter only snow crab fishery
x<-str_sub(BSAI$fishery, 1,2)
sp_fishery<-ifelse(x=="TR", "BBRKC", ifelse(x=="CK", "Pribilof king", ifelse(x=="QT", "Tanner", ifelse(x=="EI", "Tanner", ifelse(x=="EO", "snow", ifelse(x=="QR", "Pribilof king", ifelse(x=="XR", "BBRKC_TF", ifelse(x=="CO", "snow", ifelse(x=="CR", "BBRKC", ifelse(x=="QO", "snow", ifelse(x=="TT", "Tanner","")))))))))))
BSAI_bss<-cbind(BSAI, sp_fishery) %>% filter(sp_fishery=="snow")

# convert dates to season and get post-ratz
BSAI_bss$year <-  as.numeric(str_sub(BSAI_bss$sampdate, 7,10))
BSAI_bss$sampdate <-  lubridate::mdy(BSAI_bss$sampdate)
BSAI_bss$year_day <- lubridate::yday(BSAI_bss$sampdate)

BSAI_bss_ratz = BSAI_bss %>% mutate(season = case_when(year_day >= 288 ~ year+1,
                                                       TRUE ~ year)) %>%
  filter(season > 2005)

BSAI_bss_ratz=BSAI_bss_ratz %>% mutate(legal_prop = tot_legal/(female+sublegal+legal_ret+legal_nr+legal_ur))

# plot to see whether presence of fishing tends to be at high legal % areas
ggplot(BSAI_bss_ratz) + geom_density(aes(x=legal_prop))

# summarise by stat_area and season
BSAI_bss_ratz_sum = BSAI_bss_ratz %>% 
  group_by(season,statarea) %>%
  summarise(tot_legal_year = sum(tot_legal),
            tot_all_year = sum(female+sublegal+legal_ret+legal_nr+legal_ur)) %>%
  mutate(legal_prop = tot_legal_year/tot_all_year) %>% filter(statarea>0 & statarea<999999)

# plot to see how sorting % per area changes over season
ggplot(BSAI_bss_ratz_sum) + geom_line(aes(x=season,y=legal_prop,col=statarea,group=statarea))+
  geom_point(aes(x=season,y=legal_prop,col=statarea,group=statarea))

# a lot of variation across season, areas aren't exactly sticky, season effect > area
# fill in NAs with average of season sounds like the right way to go
write.csv(BSAI_bss_ratz_sum,"data/shapefiles/sorting_by_season.csv")

#==== Below is to diaganose how this interacts with CPUE data====

#=== STOP HERE IF YOU JUST WANT THE DATA SAVED FOR JOINING LATER====
# test to see how many fished areas are accounted for
cpue_dat <-read.csv("data/cpue_snow_clean.csv")
cpue_join_index = cpue_dat %>% filter(season>2005)%>%group_by(stat_area,season)%>%
  summarise(tot_effort = sum(effort))

index_join = cpue_join_index %>% left_join(BSAI_bss_ratz_sum, by =c("season"="season","stat_area"="statarea"))
# % of fishing observations that dont have sorting
mean(is.na(index_join$legal_prop)) # so 40%, probably even more when we expand all of alternatives

# plot to see whether effort tends to be at high legal % areas
ggplot(index_join) + geom_point(aes(x=season,y=legal_prop)) # relatively similar to presence of fishing effort

# expand out 
stat_area_season_expand <- cpue_dat %>% 
  filter(season > 2005) %>% na.omit() %>% expand(stat_area,season)

sorting_season_full=stat_area_season_expand %>% 
  left_join(BSAI_bss_ratz_sum, by=c("stat_area"="statarea","season"="season")) %>% arrange(stat_area,season)

ggplot(sorting_season_full) + geom_point(aes(x=legal_prop,y=tot_effort)) 

#==== Toggle on and off just to see what coverage will look like down the line
# subset to only high frequency stat_areas that are fished >10 of the time
# low_cat_freqs <- cpue_dat %>% filter(season>2005) %>%
#   group_by(stat_area) %>%
#   tally() %>%
#   mutate(freq = n / sum(n)) %>%
#   filter(n < 20)
# sorting_season <- sorting_season_full %>%
#   filter(!stat_area %in% low_cat_freqs$stat_area) 

# what is the coverage?
mean(is.na(sorting_season_full$legal_prop)) # so less than 50% coverage...how to fill?

# option 1) fill NA w/ latest data from most recent season DO THIS ONE, THEN FILL IN NAs WITH AVG of SEASON
# sorting_season_down = sorting_season_full %>% group_by(stat_area) %>% fill(legal_prop, .direction = 'down') 
# mean(is.na(sorting_season_down$legal_prop)) # fills up to 52%

# # option 2) fill NA w/ future data after?
# sorting_season_down_up = sorting_season %>% group_by(stat_area) %>% fill(legal_prop, .direction = 'downup') 
# mean(is.na(sorting_season_down_up$legal_prop)) # fills up to 90%
# 
# # option 3) fill remaining NA with neighboring values?
# sorting_all = sorting_season_down_up %>% group_by(stat_area) %>% summarise(mean_prop = mean(legal_prop))
# cpue_grid=st_read("data/shapefiles/cpue_grid.shp")

# SAVE NOW, STILL HAVE TO FILL IN NAs with AVG of SEASON
write.csv(sorting_season_down,"data/shapefiles/sorting_by_season.csv")

sorting_sp = cpue_grid %>% inner_join(sorting_all)
ggplot() + geom_sf(data=sorting_sp,aes(fill=mean_prop))

#### make map

library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(rnaturalearthdata)
library(rnaturalearth)
library(sf)

# coord for all maps
lon_1<- -180
lon_2<- -165
lat_1<-52
lat_2<-62

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

## Make CPUE grid Map
cpue_grid=st_read("data/shapefiles/cpue_grid.shp")
BSAI_bss_22 = BSAI_bss_ratz %>% filter(trip==3383)

p=ggplot() + geom_sf(data=cpue_grid,fill=NA) + 
  geom_sf(data=world) +
  # geom_point(data=BSAI_bss_22,aes(x=longitude,y=latitude,col=as.factor(trip),alpha=0.5))+
  # geom_path(data=BSAI_bss_22,aes(x=longitude,y=latitude,col=as.factor(trip),alpha=0.5))+
  geom_point(data=BSAI_bss_ratz,aes(x=longitude,y=latitude,col=as.factor(statarea),alpha=0.5))+
  theme_bw() +
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2))+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none")

pdf("plots/delete.pdf", width = 20, height = 20)
print(p)
dev.off()

BSAI_bss_ratz_sp <- as.data.frame(BSAI_bss_ratz) %>% 
  st_as_sf(coords=c("longitude","latitude"), crs=4326, remove=FALSE)  

isd_ca_co_pts <- st_join(BSAI_bss_ratz_sp, left = FALSE, cpue_grid["stat_area"]) 

mean(isd_ca_co_pts$statarea == isd_ca_co_pts$stat_area)

cpue_dat <-read.csv("data/cpue_snow_clean.csv")
cpue_dat_sp = cpue_dat %>% filter(season>2006)%>%group_by(date_seq,stat_area,lat,lon,season)%>%
  st_as_sf(coords=c("lon","lat"), crs=4326, remove=FALSE) 
