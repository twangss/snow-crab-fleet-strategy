library(sf)
library(ggplot2)
library(tidyverse)

cpue_dat<-read.csv("data/cpue_snow_clean.csv")
cpue_dat <- cpue_dat %>%
  #clean out strays
  filter(lat < 64 & lat > 53 & lon > -180 & lon < -155) 

# convert cpue dat to sf object
cpue_dat_sf <- st_as_sf(cpue_dat, coords=c('lon','lat'), crs=4326)

# set up station index
cpue_stat_area_points <- cpue_dat %>% distinct(stat_area, lat, lon) %>% st_as_sf(coords=c('lon','lat'), crs=4326)

# create grid 
cpue_bbox <- st_bbox(cpue_dat_sf)
cpue_grid <- (cpue_bbox + 1/2*c(-1,-0.5,1,0.5)) %>%
  st_make_grid(cellsize=c(1, 0.5)) %>% st_sf() %>%
  st_join(cpue_stat_area_points, left = FALSE) # take only grid cells with fish ticket data

# check that this lines up well
ggplot() + geom_sf(data=cpue_grid) + geom_sf(data=cpue_stat_area_points) #+
  # geom_sf_text(data=cpue_grid,aes(label = stat_area),size=2)+  geom_sf(data=world) +
  # coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2))

st_write(cpue_grid, "data/shapefiles/cpue_grid.shp",append=F)
