library(rerddap)
library(sf)
library(dplyr)
library(ggplot2)
library(rerddapXtracto)
library(raster)

# read in cpue grid shpfile
cpue_grid <- st_read("data/shapefiles/cpue_grid.shp") 

# get the blended winds daily info from erddap
wind_daily = info("noaacwBlendedWindsDaily", url = "https://coastwatch.noaa.gov/erddap/")

#==== Show that this works for one week
# specify time and space extents
res = griddap(wind_daily, time = c('2015-01-02','2015-01-07'),
               latitude = c(52, 65),
               longitude = c(180, 205))
# convert degrees E to decimal degrees
res$data$longitude = -180 + (res$data$longitude - 180)

# not including time data bc i will mean per stat_area this later
res1 = res$data[,c("longitude","latitude","windspeed")] %>% 
  group_by(longitude,latitude) %>% summarise(windspeed_m = mean(windspeed,na.rm=T))

# convert to sf object with WGS84
wind_sf = st_as_sf(x = res1,                         
               coords = c("longitude", "latitude"),
               crs = '+init=EPSG:4326')

# join to CPUE grid
# this is not the best join bc some points lie on the grid cell borders and get double counted when avging per grid cell
# ideally use raster stars, but not worth the effort
wind_grid_join = st_join(cpue_grid,wind_sf) %>%drop_na(stat_area) %>%
  group_by(stat_area) %>% summarise(windspeed_grid_m = mean(windspeed_m,na.rm=T))

# Check that this looks good
ggplot() + theme_bw() + 
  geom_sf(data=wind_grid_join, aes(fill=windspeed_grid_m))

#===== Now loop all the above
# get list of dates you want 
cpue_dat <-read.csv("data/cpue_snow_clean_2022.csv") %>% dplyr::select(-X)
date_seq <- cpue_dat %>%
  #clean out strays
  filter(lat < 64 & lat > 53 & lon > -180 & lon < -155 & year > 2003) %>% pull(date_seq) %>% unique()

wind_area_grid <- NULL
for (i in 1:length(date_seq)){
  # get time info in loop
  first_date = date_seq[i] %>% as.Date() - 7 #get average of preceding week
  first_date = as.character(first_date)
  last_date = date_seq[i] %>% as.character()
  
  # specify time and space extents
  res = griddap(wind_daily, time = c(first_date,last_date),
                 latitude = c(52, 65),
                 longitude = c(180, 205))
  # convert degrees E to decimal degrees
  res$data$longitude = -180 + (res$data$longitude - 180)
  
  # not including time data bc i will mean per stat_area this later
  res1 = res$data[,c("longitude","latitude","windspeed")] %>% 
    group_by(longitude,latitude) %>% summarise(windspeed_m = mean(windspeed,na.rm=T))
  
  # convert to sf object with WGS84
  wind_sf = st_as_sf(x = res1,                         
                      coords = c("longitude", "latitude"),
                      crs = '+init=EPSG:4326')
  
  # join to CPUE grid
  # this is not the best join bc some points lie on the grid cell borders and get double counted when avging per grid cell
  # ideally use raster stars, but not worth the effort
  wind_grid_join = st_join(cpue_grid,wind_sf) %>%drop_na(stat_area) %>%
    group_by(stat_area) %>% summarise(windspeed_grid_m = mean(windspeed_m,na.rm=T))
  
  wind_tmp = wind_grid_join  %>% st_drop_geometry() %>% mutate(last_date = last_date)
  wind_area_grid = rbind(wind_area_grid,wind_tmp)
  
}
write.csv(wind_area_grid, "data/weekly_wind_area_per_grid_cell_2022.csv", row.names = FALSE)


# CHLA example that may be useful later, uses rxtractogon
dataInfo <- rerddap::info('erdVH3chlamday')
parameter = 'chla'
tpos <- c("2014-09-01", "2014-10-01")
#tpos <-as.Date(tpos)
xpos <- mbnms$Longitude
ypos <- mbnms$Latitude

ypos <- c(52,52,65,65)
xpos <- c(-180,-150,-150,-180)


sanctchl <- rxtractogon(dataInfo, parameter = parameter, xcoord = xpos, ycoord = ypos,  tcoord = tpos)
str(sanctchl)

myFunc <- function(x) log(x)
sanctchl1 <- sanctchl
sanctchl1$chla <- sanctchl1$chla[, , 2]
sanctchl1$time <- sanctchl1$time[2]
sanctchlPlot <- plotBBox(sanctchl1, plotColor = 'algae', myFunc = myFunc)
sanctchlPlot


# Wind but with rxtractogon, save for later
wind_daily = info("noaacwBlendedWindsDaily", url = "https://coastwatch.noaa.gov/erddap/")
parameter = 'windspeed'
tpos <- c("1988-07-09")
#tpos <-as.Date(tpos)
xpos <- mbnms$Longitude
ypos <- mbnms$Latitude

ypos <- c(52,52,65,65)
xpos <- c(180,205,205,180)

sanctchl <- rxtractogon(wind_daily, parameter = parameter, xcoord = xpos, ycoord = ypos,  tcoord = tpos,
                        zcoord = 10.0,zName = 'zlev')

sanctchl1 <- sanctchl

sanctchlPlot <- plotBBox(sanctchl1, plotColor = 'algae', myFunc = myFunc)
sanctchlPlot





