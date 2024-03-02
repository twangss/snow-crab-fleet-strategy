library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(rnaturalearthdata)
library(rnaturalearth)
library(sf)

###########################
##### NSDIC MONTHLY ICE ###
###########################

# read in ice crs example and cpue grid shapefile
ice_crs <- st_read(
  "data/arctic_ice_extent/monthly_means/extent_N_202112_polygon_v3.0.shp") %>% 
  st_buffer(0.0) 

cpue_grid <- st_read("data/shapefiles/cpue_grid.shp") %>% st_transform(st_crs(ice_crs)) %>%
  mutate(cell_area = st_area(cpue_grid))
# loop it
year_loop <- 1985:2021
month_loop <- c("01","02","03","04","05","06","07","08","09","10","11","12")
ice_area_grid <- NULL
for (survey_year in year_loop){
  for(survey_month in month_loop){
    print(paste(survey_year,survey_month,sep = "&"))
    tryCatch({
      ice_path <- "data/arctic_ice_extent/monthly_means/extent_N_"
      ice_temp <- st_read(paste(ice_path,survey_year,survey_month,"_polygon_v3.0.shp",sep="")) %>%
        st_transform(st_crs(ice_crs)) %>% st_buffer(0.0) 
 
      #run the intersect function
      int_cpue_ice <- st_intersection(cpue_grid, ice_temp)
      int_cpue_ice$ice_area <- st_area(int_cpue_ice$geometry)
      
      ice_area_grid_tmp <- int_cpue_ice %>%
        dplyr::mutate(ice_area = st_area(geometry),
                      ice_prop = ice_area/cell_area) %>%
        st_as_sf() %>% st_drop_geometry() %>%
        mutate(year = survey_year,
               month = as.numeric(survey_month))
      
      ice_area_grid <- rbind(ice_area_grid, ice_area_grid_tmp)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
  }
}
# write.csv(ice_area_grid, "data/monthly_ice_area_per_grid_cell.csv")

# Plot as an example of one run of this loop
ice_path <- "data/arctic_ice_extent/monthly_means/extent_N_"
ice_temp <- st_read(paste(ice_path,2012,"03","_polygon_v3.0.shp",sep="")) %>%
  st_transform(st_crs(ice_crs)) %>% st_buffer(0.0) 

#run the intersect function
int_cpue_ice <- st_intersection(cpue_grid, ice_temp)
int_cpue_ice$ice_area <- st_area(int_cpue_ice$geometry)
ggplot() + geom_sf(data=ice_temp, color="blue") + geom_sf(data=cpue_grid) +
  geom_sf(data = int_cpue_ice, color = "red") +
  coord_sf(xlim = c(st_bbox(cpue_grid)$xmin,st_bbox(cpue_grid)$xmax), 
           ylim = c(st_bbox(cpue_grid)$ymin,st_bbox(cpue_grid)$ymax))


###########################
##### USNIC WEEKLY ICE ####
###########################

# read in ice crs example and cpue grid shapefile
ice_crs <- st_read(
  "data/USNIC_ice/USNIC_daily_2022_03_18/ARCTIC220318.shp") %>% 
  st_buffer(0.0) 

cpue_grid <- st_read("data/shapefiles/cpue_grid.shp") %>% st_transform(st_crs(ice_crs))
cpue_grid <- cpue_grid %>% mutate(cell_area = st_area(cpue_grid))
  

date_seq <- seq(as.Date("2022-01-01"), as.Date("2022-12-31"), "day")
d = strftime(date_seq, format = "%d")
m = strftime(date_seq, format = "%m")
y = strftime(date_seq, format = "%Y")

loop_table <- bind_cols(as.Date(date_seq),d,m,y,NA) 
colnames(loop_table) <- c("date_seq","d","m","y","usnic")

ice_area_grid <- NULL
# i = 6
# i = 1525

for (i in 1:nrow(loop_table)){
    tryCatch({
      y_tmp = loop_table[i,4]
      m_tmp = loop_table[i,3]
      d_tmp = loop_table[i,2]
      date_tmp = pull(loop_table[i,1])
      
      print(paste(y_tmp,m_tmp,d_tmp,sep="..."))
      
      temp_folder = paste("data/USNIC_ice/USNIC_daily_",y_tmp,"_",m_tmp,"_",d_tmp,sep="")
      file_name <- list.files(temp_folder, pattern="\\.shp$", full.names=TRUE)
      if(length(file_name)==0) next 
      
      ice_temp <- st_read(file_name)
      st_crs(ice_temp) <- st_crs(ice_crs)
      ice_temp <- ice_temp %>% st_buffer(0.0) %>% select(CT) %>%
        mutate(CT_num = as.numeric(CT)) %>%
        # take out polygons of CONCENTRATION > 10%
        filter(CT_num > 10) %>% st_union()
      ggplot()+geom_sf(data=ice_temp)
      #run the intersect function
      int_cpue_ice <- st_intersection(cpue_grid, ice_temp)
      int_cpue_ice$ice_area <- st_area(int_cpue_ice$geometry)
      
      ice_area_grid_tmp <- int_cpue_ice %>%
        dplyr::mutate(ice_area = st_area(geometry),
                      ice_prop = ice_area/cell_area) %>%
        st_as_sf() %>% st_drop_geometry() 
      
      # Join to cpue grid to get 0s
      # set up empty temporal-defined grid
      grid_tmp <- cpue_grid %>% st_drop_geometry() %>% select(stat_area) %>%
        mutate(year = as.numeric(y_tmp),
               month = as.numeric(m_tmp),
               day = as.numeric(d_tmp),
               date = as.Date(date_tmp)) %>%
        # join it to ice 
        left_join(ice_area_grid_tmp, keep = F)
      
      ice_area_grid <- rbind(ice_area_grid, grid_tmp)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
}
# replace NA values from join with 0
ice_area_grid <- ice_area_grid %>% replace(is.na(.), 0)
# add grid cell areas manually for better processing later on, MAKE SURE YOURE USING THE RIGHT PRJ FOR CPUE_GRID
ice_area_grid_cell=cpue_grid %>% st_drop_geometry() %>% rename(grid_cell_area = cell_area) %>% 
  right_join(ice_area_grid)

write.csv(ice_area_grid_cell, "data/weekly_ice_area_per_grid_cell_2022.csv")

#### Plot as an example of one run of this loop
ice_temp <- st_read("data/USNIC_ice/USNIC_daily_2012_03_12/ARCTIC120312.shp")
ice_temp <- ice_temp %>% st_buffer(0.0) %>% select(CT) %>%
  mutate(CT_num = as.numeric(CT)) %>%
  # take out polygons of CONCENTRATION > 10%
  filter(CT_num > 10) %>% st_union()
#run the intersect function
int_cpue_ice <- st_intersection(cpue_grid, ice_temp)
int_cpue_ice$ice_area <- st_area(int_cpue_ice$geometry)

ggplot()+geom_sf(data=cpue_grid,fill=NA)+
  geom_sf(data=int_cpue_ice,color="blue")+
  coord_sf(crs = 4326)

###########################
##### Join to CPUE data as A TEST, THIS IS DONE IN THE BIG COVARIATE JOIN CODE####
###########################
cpue_dat_month <- read.csv("data/cpue_snow_clean.csv")
cpue_dat_month <- cpue_dat_month %>% rename(cpue_date = date_seq) %>% mutate(cpue_date = as.Date(cpue_date))

ice_area_grid <- read.csv("data/weekly_ice_area_per_grid_cell.csv")
ice_area_grid <- ice_area_grid %>% rename(ice_date = date) %>% mutate(ice_date = as.Date(ice_date))

library(data.table)
# coerce to data.table and append join columns to preserve the original columns 
setDT(ice_area_grid)[, join_date := ice_date]
setDT(cpue_dat_month)[, join_date := cpue_date]
# rolling join
cpue_ice <- ice_area_grid[cpue_dat_month, on = .(stat_area, join_date), roll = "nearest"]


