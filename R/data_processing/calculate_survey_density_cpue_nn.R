library(ggplot2)
library(tidyverse)
library(sf)

#===== Get Survey density within 100 mi radius around stat_area
#==== Set Up Grid Cells based on Point Data
cpue_dat<-read.csv("data/cpue_snow_clean.csv")
cpue_dat <- cpue_dat %>%
  #clean out strays
  filter(lat < 64 & lat > 53 & lon > -180 & lon < -155) 

# get polar ICE crs, UNITS = METERS
ice_crs <- st_read(
  "data/USNIC_ice/USNIC_daily_2012_03_12/ARCTIC120312.shp") %>%
  st_crs() 

# convert cpue dat to sf object
cpue_dat_sf <- st_as_sf(cpue_dat, coords=c('lon','lat'), crs=4326)%>%
  st_transform(ice_crs)
st_crs(cpue_dat_sf)$units
# set up station index
cpue_stat_area_points <- cpue_dat %>% distinct(stat_area, lat, lon) %>% 
  st_as_sf(coords=c('lon','lat'), crs=4326)%>%
  st_transform(ice_crs)

cpue_buffer = cpue_stat_area_points %>% st_buffer(160934)
ggplot() + geom_sf(data=cpue_buffer, fill=NA) + geom_sf(data=cpue_dat_sf)

#==== Survey data
M101_survey_join <- read.csv("data/M101_survey.csv")
M101_survey_join[is.na(M101_survey_join)] <- 0

survey_filt <- M101_survey_join %>% filter(Year == 2005) %>% as.data.frame() %>% drop_na()

survey_sf = survey_filt %>% st_as_sf(coords=c('long','lat'), crs=4326)%>%
  st_transform(ice_crs)

#== PLot it to see what's up
ggplot() + geom_sf(data=cpue_buffer, fill=NA,aes(color=stat_area)) + geom_sf(data=survey_sf) +
  geom_sf(data=cpue_stat_area_points, aes(color=stat_area))
# so how to deal with the SW cpue points that dont get any points? 
# maybe nearest 75 neighbors and avg them? the buffers with most points is 100

#=== Test run a 75 nn and summarise mean density for each CPUE
library(nngeo)
cpue_survey_join=st_join(cpue_stat_area_points, survey_sf,join = st_nn, k = 75, maxdist = 1000000)

tbl = cpue_survey_join %>% st_drop_geometry() %>% group_by(stat_area)%>%
  summarise(mean_surv = mean(density,na.rm=T))

#=== Now loop it
cpue_survey_tbl = NULL
survey_years = M101_survey_join %>% filter(Year>2004) %>% dplyr::select(Year) %>% distinct()

for(i in 1:nrow(survey_years)){
year = survey_years[i,]
survey_filt <- M101_survey_join %>% filter(Year == year) %>% as.data.frame() %>% drop_na()

survey_sf = survey_filt %>% st_as_sf(coords=c('long','lat'), crs=4326)%>%
  st_transform(ice_crs)

cpue_survey_join=st_join(cpue_stat_area_points, survey_sf,join = st_nn, k = 75, maxdist = 1000000)

tbl_df = cpue_survey_join %>% st_drop_geometry() %>% group_by(stat_area)%>%
  summarise(mean_surv = mean(density,na.rm=T)) %>% mutate(year=year)
cpue_survey_tbl = rbind(tbl_df,cpue_survey_tbl)
}

# set the missing 2020 season as 2019 survey
cpue_survey_tbl_fake2020 = cpue_survey_tbl %>% filter(year==2019) %>% mutate(year=2020)
cpue_survey_tbl = rbind(cpue_survey_tbl,cpue_survey_tbl_fake2020)

# write.csv(cpue_survey_tbl,"data/cpue_survey_season.csv")

#===== Offset survey density with shift in COG between fishery and survey from preceding year
survey.COG = M101_survey_join %>% group_by(Year) %>%
  summarise(surv.COG.lat = weighted.mean(lat,density,na.rm=T),
           surv.COG.long = weighted.mean(long,density,na.rm=T)) %>%
  mutate(season = Year+1)

fishery.COG = cpue_dat %>% group_by(season) %>% drop_na(c(lat, lon, number)) %>%
  summarise(fish.COG.lat = weighted.mean(lat,number,na.rm=T),
            fish.COG.long = weighted.mean(lon,number,na.rm=T))

diff.COG = survey.COG %>% left_join(fishery.COG) %>% mutate(diff.COG.lat = fish.COG.lat-surv.COG.lat,
                                                 diff.COG.long = fish.COG.long-surv.COG.long,
                                                 season_offset = season) %>%
  select(season_offset,diff.COG.lat,diff.COG.long) %>% drop_na(diff.COG.lat) 

# add 2 years of offset vector for 2021 and 2022 (you dont have 2022 season fishery data yet though)
diff.COG.21.22 = diff.COG %>%slice(rep(nrow(diff.COG),2)) %>%
  mutate(season_offset = c(2021,2022))
  
diff.COG.full = rbind(diff.COG,diff.COG.21.22)

# join to raw survey data and offset 
# we join season of fishery to the year of survey, which means offset from previous survey to fishery will be applied to current survey
M101_survey_offset = M101_survey_join %>% left_join(diff.COG.full,by=c("Year" = "season_offset")) %>%
  mutate(offset.lat = lat+diff.COG.lat,
         offset.long = long+diff.COG.long)

# turn into sf object
survey_offset_sf <- M101_survey_offset %>% select(offset.lat,offset.long,Year,density) %>% drop_na() %>%
  st_as_sf(coords=c('offset.long','offset.lat'), crs=4326)

cpue_grid <- st_read("data/shapefiles/cpue_grid.shp") 

#map to make sure it works, yes
# ggplot() + geom_sf(data = cpue_grid) + geom_sf(data = survey_offset_sf) 

survey_grid_join = survey_offset_sf %>% st_join(cpue_grid) %>% st_drop_geometry() %>% 
  drop_na() %>% # drop no stat areas
  group_by(Year,stat_area) %>%
  summarise(surv_offset = mean(density,na.rm=T))

# add 2020 as copy of 2019 bc of missing covid year
add_year2020 = survey_grid_join %>% filter(Year==2019) %>%
  mutate(Year=2020)

survey_grid_join_full = rbind(survey_grid_join,add_year2020)

# write.csv(survey_grid_join_full,"data/cpue_survey_season_offset.csv",row.names = F)

#### Plot for supplementary figure
M101_survey_join

# use year 2010 as an example
survey_2010 <- M101_survey_join %>% filter(Year == 2010) %>% as.data.frame() %>% drop_na()
survey_sf_2010 = survey_2010 %>% st_as_sf(coords=c('long','lat'), crs=4326)

surv2010_COG = survey_2010 %>% 
  summarise(surv.COG.lat = weighted.mean(lat,density,na.rm=T),
            surv.COG.long = weighted.mean(long,density,na.rm=T),
            name="surv") %>% 
  st_as_sf(coords=c('surv.COG.long','surv.COG.lat'), crs=4326)

# use season 2011 as an example
cpue_2011 = cpue_dat %>% filter(season == 2011) %>% group_by(lat, lon) %>% drop_na(c(lat, lon, number)) %>%
  summarise(sum_number = sum(number,na.rm=T))

cpue_sf_2011 = cpue_2011 %>% st_as_sf(coords=c('lon','lat'), crs=4326)

cpue2010_COG = cpue_dat %>% filter(season == 2011) %>% drop_na(c(lat, lon, number)) %>%
  summarise(fish.COG.lat = weighted.mean(lat,number,na.rm=T),
          fish.COG.long = weighted.mean(lon,number,na.rm=T),
          name="cpue") %>% 
  st_as_sf(coords=c('fish.COG.long','fish.COG.lat'), crs=4326)

# add year 2011 for shifted 2nd plot
survey_2011 <- M101_survey_join %>% filter(Year == 2011) %>% as.data.frame() %>% drop_na()
survey_sf_2011 = survey_2011 %>% st_as_sf(coords=c('long','lat'), crs=4326)

offset_x = -171.4622  - -169.4825 
offset_y =  56.98056 - 58.19262

survey_off_2011 <- M101_survey_join %>% filter(Year == 2011) %>% as.data.frame() %>% drop_na() %>%
  mutate(lat = lat+offset_y, long = long+offset_x)
survey_sf_off_2011 = survey_off_2011 %>% st_as_sf(coords=c('long','lat'), crs=4326)

surv_no.off_off_sf = rbind(survey_sf_2011 %>% mutate(label="Survey 2011"),survey_sf_off_2011 %>% mutate(label="Survey Offset 2011"))

# offset vector if you need it, right now you use ggarchery and manually set it up
cbind(surv2010_COG, cpue2010_COG) -> df

vector_offset = st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, df$geometry, df$geometry.1, SIMPLIFY=FALSE)) %>% 
  st_set_crs(4326)

# plot it

lon_1<- -180
lon_2<- -159
lat_1<-52 
lat_2<-63

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


library(ggsflabel)
p1 = ggplot() + 
  geom_sf(data=survey_sf_2010, mapping = aes(size=density),color="#2E6171",lwd=0,alpha=0.7) + 
  guides(size = guide_legend(title = "Survey Density 2010")) +
  ggnewscale::new_scale("fill") +
  ggnewscale::new_scale("size") +
  geom_sf(data=cpue_sf_2011, mapping = aes(size=sum_number),color="#8EA604",alpha=0.7) +
  
  guides(size = guide_legend(title = "Fishery Density 2011")) +
  geom_sf(data=surv2010_COG,size=7,color="#11576A",shape=18,stroke=5) +
  
  geom_sf_label_repel(data = surv2010_COG,aes(label = "Survey CoG"),
                     force = 10, nudge_x = 2,nudge_y=  c(1,-1,-1), seed = 10) + 
  
  geom_sf(data=cpue2010_COG,size=7,color="#414B09",shape=18,fill="black",stroke=5)+
  
geom_sf_label_repel(data = cpue2010_COG,aes(label = "Fishery CoG"),
                    force = 10, nudge_x = -2,nudge_y=  c(-1,1,1), seed = 10) +
  ggarchery::geom_arrowsegment(aes(x = -169.4825 , xend = -171.4622, y = 58.19262, yend = 56.98056), color = "red",
                    arrow_positions = c(0.25,0.5, 0.75,1),
                    arrows = list(arrow(angle = 20)),linewidth=0.5) +
  # geom_sf(data=vector_offset,size=2,fill="black",arrow = arrow(type = 'closed'),size=14) + 
  geom_sf(data=world) +
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2), expand = FALSE) + 
  theme_bw()+
  theme(legend.position = c(0, 0), 
        legend.justification = c(0, -.1),
        legend.box.background = element_rect(color = "black"),
        legend.title=element_text(size=12),
        legend.direction="horizontal",
        axis.title=element_blank())


p2 = ggplot() + 
  geom_sf(data=surv_no.off_off_sf, mapping = aes(size=density,color=label),lwd=0,alpha=0.7) + 
  # geom_sf(data=survey_sf_off_2011, mapping = aes(size=density,color="#8EA604"),color="#2E6141",alpha=0.7) +
  ggarchery::geom_arrowsegment(aes(x = -169.4825 , xend = -171.4622, y = 58.19262, yend = 56.98056), color = "red",
                               arrow_positions = c(0.25,0.5, 0.75,1),
                               arrows = list(arrow(angle = 20)),linewidth=0.5) +
  # geom_sf(data=vector_offset,size=2,fill="black",arrow = arrow(type = 'closed'),size=14) + 
  geom_sf(data=world) +
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2), expand = FALSE) + 
  scale_color_manual(values = c("#77DAFB", "#737A7C")) + 
  guides(size = guide_legend(title = "Survey Density"),color = guide_legend(title = "Type")) +
  theme_bw()+
  theme(legend.position = c(0, 0),
        legend.justification = c(0, -.1),
        legend.box.background = element_rect(color = "black"),
        legend.title=element_text(size=12),
        legend.direction="horizontal",
        axis.title=element_blank())
  
library(patchwork)

survey_offset_p = p1+p2+
  plot_layout(ncol=1, byrow = T)

pdf("plots/survey_offset_supp.pdf",width=8,height=11.5)
survey_offset_p
dev.off()


# plot survey stations vs cpue grids
cpue_grid <- st_read("data/shapefiles/cpue_grid.shp")
survey_4326_sf = survey_filt %>% st_as_sf(coords=c('long','lat'), crs=4326)

p = ggplot() + geom_sf(data=survey_4326_sf) +
  geom_sf(data=cpue_grid,fill=NA) +
  geom_sf(data=world) +
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2), expand = FALSE) + 
  theme_bw()+
  theme(legend.position = c(0, 0),
        legend.justification = c(0, -.1),
        legend.box.background = element_rect(color = "black"),
        legend.title=element_text(size=12),
        legend.direction="horizontal",
        axis.title=element_blank())

pdf("plots/survey_grid_supp.pdf",width=8,height=11.5)
p
dev.off()
