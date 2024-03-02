library(NatParksPalettes)

cpue_grid=st_read("data/shapefiles/cpue_grid.shp")

#=== Assign stat areas to spatial strategy based on highest cluster group
set_cluster = spat_clust_kmeans_sum %>% group_by(stat_area) %>%
  filter(ar_ve_rand_m==max(ar_ve_rand_m)) %>%
  filter(stat_area != 786030) # drop the 0 area intercept

set_cluster_sf = cpue_grid %>% inner_join(set_cluster)  
p=ggplot() + geom_sf(data=set_cluster_sf,aes(fill=as.factor(spat_cluster))) + 
  geom_sf(data=set_cluster_sf,fill=NA) + 
  geom_sf(data=world) +
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2), expand = FALSE) + 
  theme_bw()  +
  geom_sf_text(data = set_cluster_sf, aes(label = stat_area),size=3)
p


#=== Assign stat areas based on manual "expert" interpretation
clust1 = c(665530,665600,675630,675600,675530,685630,685600,685530)
clust2 = c(705630,705600,715730,715700,715630,715600,725700,725630,735630)
clust3 = c(725800,725730,735830,735800,735730,735700,745930,745900,745830,745800,756000,755930,766000,765930,786030,755900)

clust3_v2 = c(725800,725730,735830,735800,735730,735700,745800)
clust4_v2 = c(745930,745900,745830,756000,755930,766000,765930,786030)

area_cluster_index = data.frame(stat_area = Areas) %>%
  mutate(spat_cluster = case_when(
    stat_area %in% clust1 ~ "South",
    stat_area %in% clust2 ~ "West",
    stat_area %in% clust3 ~ "North",
    # stat_area %in% clust4_v2 ~ "clust4"
  ))

set_cluster_sf = cpue_grid %>% inner_join(area_cluster_index) %>%
  mutate(spat_cluster = factor(spat_cluster, levels = c("North", "West", "South")))

p=ggplot() + geom_sf(data=set_cluster_sf,aes(fill=spat_cluster)) + 
  geom_sf(data=set_cluster_sf,fill=NA) + 
  geom_sf(data=world) +
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2), expand = FALSE) + 
  scale_fill_manual(values=rev(natparks.pals("Triglav"))) + 
  theme_bw() + theme(legend.position = "none")
  # geom_sf_text(data = set_cluster_sf, aes(label = stat_area),size=3)
p


#=== Assign vessels to clusters per season
TheD_spat_cluster = TheD %>% select(vessel, season, effort,stat_area) %>% 
  left_join(area_cluster_index) # change this depending on  which dataframe u use

TheD_spat_cluster_season = TheD_spat_cluster %>% group_by(vessel,season,spat_cluster) %>%
  summarise(spat_clust_eff_sum = sum(effort,na.rm=T)) %>%
  group_by(vessel,season) %>% mutate(prop_eff_cid = spat_clust_eff_sum/sum(spat_clust_eff_sum,na.rm=T))

# hist(TheD_spat_cluster_season$prop_eff_cid)

TheD_spat_cluster_season_max = TheD_spat_cluster_season%>%
  filter(spat_clust_eff_sum==max(spat_clust_eff_sum)) %>% 
  filter(row_number()==1) %>% select(-c(spat_clust_eff_sum,prop_eff_cid)) 

# hist(TheD_spat_cluster_season_max$prop_eff_cid)

sankey_df_input = TheD_spat_cluster_season_max %>% ungroup()%>% 
  arrange(season)%>% # make -season if you want to reverse order for coord flip
  pivot_wider(names_from = season,
              names_prefix = "S",
              values_from = spat_cluster,
              names_sort = F) %>%
  data.frame()

library(ggsankey)

sankey_plot = do.call(rbind, apply(sankey_df_input, 1, function(x) {
  x <- na.omit(x[-1])
  data.frame(x = sub(".", "", names(x)), node = x, 
             next_x = dplyr::lead(sub(".", "", names(x))), 
             next_node = dplyr::lead(x), row.names = NULL)
})) %>%
  mutate(x = factor(x, sub(".","",names(sankey_df_input)[-1])),
         next_x = factor(next_x, sub(".","",names(sankey_df_input)[-1]))) %>%
  dplyr::mutate(
    # This is the unnatural order, applied to node and next_node
    node = factor(node, levels = c("North", "West", "South")),
    next_node = factor(next_node, levels = c("North", "West", "South"))
  ) %>%
  # below chunks remove non consecutive fishing years to clean up small threads and aid clarity
  mutate(year_diff = as.numeric(next_x) - as.numeric(x)) %>%
  filter(year_diff<2 | is.na(year_diff)) %>%
    select(-year_diff) %>%
  # sankey plot
  ggplot(aes(x = x,
             next_x = next_x,
             node = node,
             next_node = next_node,
             fill = node,
             label = node)) +
  geom_sankey(flow.alpha = .7,
              node.color = NA,
              show.legend = TRUE) +
  scale_fill_manual(values=rev(natparks.pals("Triglav"))) + 
 theme_classic() + 
  theme(legend.position = "none") + 
  labs(x="Season",y="Number of Vessels") + 
  theme(text = element_text(size = 11)) +
  scale_y_continuous(breaks=seq(-100,100,10),labels=seq(-100,100,10)+50,position = "left") # or right if you want to flip
  

library(gridExtra)
sankey_p = grid.arrange(
  arrangeGrob(
    p, 
    sankey_plot,
    ncol=2 ,heights = c(1), widths = c(1,3)),
  nrow=1)

pdf("plots/spatial_archetype_sankey.pdf",width=12,height=4)
grid.arrange(
  arrangeGrob(
    p, 
    sankey_plot,
    ncol=2 ,heights = c(1), widths = c(1,3)),
  nrow=1)
dev.off()

# how much do vessels fish each area
# get proportion of vessels that have fished each spat_clust all time
prop_eff_spat_all_time = TheD_spat_cluster %>% group_by(vessel,spat_cluster) %>%
  summarise(eff_spat_alltime = sum(effort,na.rm=T)) %>%
  group_by(vessel) %>%
  mutate(prop_eff_spat_alltime = eff_spat_alltime/sum(eff_spat_alltime))
# count how many vessel fish 1-3 areas 
vessel_num_spat_tbl = prop_eff_spat_all_time %>% group_by(vessel) %>%summarise(n=n())
  
vessel_num_years_fished = TheD_spat_cluster %>% select(vessel,season) %>% distinct() %>% group_by(vessel) %>% summarise(n_year=n())

vessel_num_spat_tbl_years_fished = vessel_num_spat_tbl %>% left_join(vessel_num_years_fished) 

vessel_num_spat_tbl%>% group_by(n) %>%
  summarise(vess_count=n()) %>%
  mutate(vess_prop = vess_count/sum(vess_count))

# primay fishing area
vessel_num_spat_tbl_primary = TheD_spat_cluster_season_max %>% group_by(vessel) %>% select(-season) %>% distinct() %>% summarise(n=n())
vessel_num_spat_tbl_primary_years_fished = vessel_num_spat_tbl_primary %>% 
  left_join(vessel_num_years_fished) %>% 
  left_join(vessel_index_tbl) %>%
  left_join(spat_cluster_df_join,by=c("vessel_num"="vessel"))

ggplot(data=vessel_num_spat_tbl_primary_years_fished) + geom_boxplot(aes(x=spat_cluster,y=n,color=spat_cluster,weight=n_year))+
  viridis::scale_color_viridis(discrete=T)

ggplot(data=vessel_num_spat_tbl_primary_years_fished) + geom_point(aes(x=n_year,y=n,color=spat_cluster),position="jitter")+
  viridis::scale_color_viridis(discrete=T)

vessel_num_spat_tbl_primary_years_fished %>% group_by(n) %>%
  summarise(vess_count=n()) %>%
  mutate(vess_prop = vess_count/sum(vess_count))

# switch opportunities and counts
vessel_switch_spat_cluster = TheD_spat_cluster_season_max %>% group_by(vessel) %>% 
  mutate(switch_region = case_when(spat_cluster != lag(spat_cluster) ~ 1,
                                   spat_cluster == lag(spat_cluster) ~ 0,
                                   .default = NA )) %>%
  summarise(switch_region_count = sum(switch_region,na.rm=T),
            switch_opps = n()-1) %>%
  mutate(switch_prop = switch_region_count/switch_opps)

vessel_num_spat_tbl_primary_years_fished_switch_opps = vessel_num_spat_tbl_primary_years_fished %>% 
  left_join(vessel_switch_spat_cluster)

p = ggplot(data=vessel_num_spat_tbl_primary_years_fished_switch_opps,aes(x=AKFIN_LENGTH,y=switch_prop,color=spat_cluster)) + 
  geom_point(aes(size=switch_opps))+
  scale_color_manual(values=(natparks.pals("Volcanoes"))) + scale_fill_manual(values=(natparks.pals("Volcanoes"))) +
  # geom_smooth(method = 'lm',aes(weight=switch_opps))+
  theme_bw() +
  labs(x="Vessel Length (ft)",y="Proportion Switching to New Primary Region",colour = "Spatial Archetype",size="Seasons Fished") +
  theme(text = element_text(size = 15),
        legend.title = element_text(size = 10),
        legend.text= element_text(size = 10),
        legend.justification=c(1,1), 
        legend.position = c(0.99,0.99)) 

pdf("plots/spatial_archetype_vess_switch_region.pdf",width=8,height=8)
print(p)
dev.off()


glm_sum = glm(data=vessel_num_spat_tbl_primary_years_fished_switch_opps,
    switch_prop ~ AKFIN_LENGTH,
    family = binomial,
    weights=switch_opps)

summary(glm_sum)

ggplot(data=vessel_num_spat_tbl_primary_years_fished_switch_opps,aes(x=switch_prop,y=spat_cluster)) + 
  geom_boxplot(aes(weight=switch_opps))+
  viridis::scale_color_viridis(discrete=T)

hist(vessel_num_spat_tbl_primary_years_fished_switch_opps$switch_prop)
weighted.mean(vessel_num_spat_tbl_primary_years_fished_switch_opps$switch_prop,
              vessel_num_spat_tbl_primary_years_fished_switch_opps$switch_opps)

summary(lm(data = vessel_num_spat_tbl_primary_years_fished_switch_opps,switch_prop ~ AKFIN_FUEL_CAPACITY,
           weights=switch_opps))

TukeyHSD(aov(lm(data = vessel_num_spat_tbl_primary_years_fished_switch_opps,switch_prop ~ spat_cluster,
           weights=switch_opps)))



vessel_num_spat_tbl_primary %>% left_join(vessel_num_years_fished) %>% 
  group_by(n) %>% summarise(number=n()) %>% mutate(prop_number = number/sum(number))


prop_eff_spat_all_time %>% ggplot(aes(x=vessel,y=prop_eff_spat_alltime, fill = spat_cluster)) + geom_bar(stat='identity',position = position_stack())



         
