library(factoextra)
library(cluster)
library(sf)
library(NbClust)

# 1 Cluster vessels ----  
#==== Area-Vessel Intercept K means clustering  
area_vess_RE_FE_wide = area_par_vess_rand_df %>% select(stat_area,vessel,area_vess_total)%>%
  pivot_wider(names_from = "stat_area",values_from="area_vess_total")%>%
  select(-c(vessel,'786030'))

area_vess_RE_wide=t(report$Int_Area_Vessel_rand)%>% as.data.frame() # %>% scale()

# Explore the number of clusters recommended by different indices and methods
# # Elbow method
# fviz_nbclust(df_real, kmeans, method = "wss",k.max = 10) +
#   labs(subtitle = "Elbow method")
# 
# # Silhouette method
# fviz_nbclust(df_real, kmeans, method = "silhouette",k.max = 20)+
#   labs(subtitle = "Silhouette method")
# 
# # Gap statistic
# # nboot = 50 to keep the function speedy. 
# # recommended value: nboot= 500 for your analysis.
# # Use verbose = FALSE to hide computing progression.
# set.seed(123)
# fviz_nbclust(df_real, kmeans, k.max = 20,nstart = 25,  method = "gap_stat", nboot = 50)+
#   labs(subtitle = "Gap statistic method")

# Find Optimal # of Area-Vessel Clusters with 30 indices
res = NbClust(data = area_vess_RE_wide, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 30,
        method = "kmeans", index = "all", alphaBeale = 0.1)
p = res$Best.nc[1,] %>% # get number of clusters only, ignore index for now
  as.data.frame() %>% ggplot() + geom_bar(aes(x=.)) + labs(x="Number of Clusters",y="Count") + theme_bw() 
# 3 clusters is best with both area-vessel RE+FE and RE
median(res$Best.nc[1,])
# pdf("plots/number_of_clusters.pdf",height=4,width=6)
print(p)
# dev.off()

# Cluster and aggregate area-vessel RE 
set.seed(23)
#perform k-means clustering with k = 3 clusters
km_area <- kmeans(area_vess_RE_wide, centers = 3, nstart = 500)

#find means of each cluster
clus_agg = aggregate(area_vess_RE_wide, by=list(cluster=km_area$cluster), mean) %>% mutate("786030" = 0)
spat_cluster_df_join = data.frame(vessel = unique(TheD$vessel_num),
                                  # vessel = as.numeric(rownames(as.data.frame(km_area$cluster))), # old vessel numbers when they were sequetial but 96 is skipped now
                                  spat_cluster=as.numeric(km_area$cluster))

spat_clust_kmeans = area_par_vess_rand_df %>% left_join(spat_cluster_df_join) 

spat_clust_kmeans %>% ggplot()+
  geom_density_ridges(aes(x=Int_Area_Vess_rand,y=as.factor(stat_area),fill=as.factor(spat_cluster)),alpha=0.5)

spat_clust_kmeans %>% ggplot()+
  geom_boxplot(aes(x=Int_Area_Vess_rand,y=as.factor(stat_area),fill=as.factor(spat_cluster)),alpha=0.5)+
  geom_point(aes(x=Int_Area_Vess_rand,y=as.factor(stat_area),color=as.factor(spat_cluster),position = "jitter"),alpha=0.5)

spat_clust_kmeans_sum = spat_clust_kmeans %>%group_by(spat_cluster,stat_area) %>%
  summarise(ar_ve_rand_m = mean(Int_Area_Vess_rand,na.rm=T),
            sd = sd(Int_Area_Vess_rand),
            n=n())

# MAP plot it
cpue_grid=st_read("data/shapefiles/cpue_grid.shp")
spat_grid_kmeans=cpue_grid %>% inner_join(spat_clust_kmeans_sum)

lon_1<- -180
lon_2<- -158
lat_1<-52 
lat_2<-64

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


p=ggplot() + geom_sf(data=spat_grid_kmeans,aes(fill=ar_ve_rand_m)) + 
  # scale_fill_gradient2(midpoint = 0,high="darkgreen",low="darkred",mid="white") +
  scale_fill_gradientn("Mean Area Intercept",colours=c("darkred","white", "darkgreen"), na.value = "grey98",
                       limits = c(-.5, .5)) +
  geom_sf(data=spat_grid_kmeans,fill=NA) + 
  geom_sf(data=world) +
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2), expand = FALSE) + 
  theme_bw() + facet_wrap(~n,ncol=1,
                          # set manual lables to facet wrap
                          # labeller = labeller(n = c('25' = "South, 25 Vessels",
                          #   '28' = "North, 28 Vessels", '55' = "East, 55 Vessels"))) + 
  
  labeller = labeller(n = c('28' = "South, 25 Vessels",
                            '26' = "North, 31 Vessels", '43' = "West, 41 Vessels"))) + 
  # port points and labels
  geom_sf(data = port_sf, alpha = 1, size=1) +
  geom_sf_text_repel(data = port_sf,aes(label = names),
                     force = 10, nudge_x = 2,nudge_y=  c(1,-1,-1,-1), seed = 10,size=3)+
  # island labels
  geom_sf_text_repel(data = island_sf,aes(label = names),
                     force = 10, nudge_x = 2,nudge_y= c(1,1), seed = 10,size=3)+
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2), expand = FALSE) + 
  theme(legend.position = c(1, 1), 
        legend.justification = c(1.1, 1.1),
        legend.box.background = element_rect(color = "black"),
        legend.title=element_text(size=8),
        legend.text = element_text(size=8),
        legend.direction="horizontal")
p
# pdf("plots/cluster_area.pdf", width=4,height=12)
# print(p)
# dev.off()

# 2 Covariates w respect to clusters ----

library(NatParksPalettes)
# write.csv(vessel_RE_df,"vessel_RE_df_best_model_2022_rand.tune.csv")
# write.csv(spat_cluster_df_join,"spat_clust_3_best_clusters_2022_rand.tune.csv")

vessel_RE_df = read.csv("R/model/output/vessel_RE_df_best_model_2022_rand.tune.csv")[,-1] 
spat_cluster_df_join = read.csv("R/model/output/spat_clust_3_best_clusters_2022_rand.tune.csv")[,-1] %>% 
  # mutate(spat_cluster = case_when(spat_cluster == 1 ~ "West",
  #                                 spat_cluster == 2 ~ "South",
  #                                 spat_cluster == 3 ~ "North"))
  mutate(spat_cluster = case_when(spat_cluster == 1 ~ "West",
                                  spat_cluster == 2 ~ "North",
                                  spat_cluster == 3 ~ "South"))

vessel_RE_df_spat = vessel_RE_df %>% left_join(spat_cluster_df_join,by = c("vessel" = "vessel"))

#this is dumb 
vessel_RE_df_spat_fixed = vessel_RE_df_spat 
# %>% 
#   mutate(Trad_Vess_rand_par = Trad_Vess_rand_par+2.201269297,
# Cong_Vess_rand_par = Cong_Vess_rand_par+0.012465572,
# Port_Vess_rand_par = Port_Vess_rand_par+-0.002096175)
# CpueF_Vess_rand_par = CpueF_Vess_rand_par+0.001763137,
# Ice_Vess_rand_par = Ice_Vess_rand_par-0.568069219,
# CvV_Vess_rand_par = CvV_Vess_rand_par-0.520947953)


vessel_RE_df_spat_long = vessel_RE_df_spat_fixed %>% select(!c(Trad,Surv,Port,Cpuelag,CpueV)) %>%
  pivot_longer(!c(vessel,spat_cluster),
               names_to="RE",
               values_to="RE_value")
vessel_RE_df_spat_long_sum = vessel_RE_df_spat_long %>%
  group_by(spat_cluster, RE) %>%
  summarise(m = mean(RE_value),s = sd(RE_value)) 

vessel_RE_df_spat_long$spat_cluster <- factor(vessel_RE_df_spat_long$spat_cluster, levels = c("North","West","South"))

vessel_RE_df_spat_long %>%
  group_by(spat_cluster, RE) %>%
  summarise(m = mean(RE_value),s = 1.96*sd(RE_value)/sqrt(n())) %>%
  drop_na() %>%
  ggplot(aes(factor(spat_cluster),m)) + geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin=m-s, ymax=m+s), width=.2,position=position_dodge(.9)) +
  labs(y="Fixed + Random Effect Values") +
  # scale_fill_manual(values=c("#629cfe", "#f7776e", "#04bb3a")) +
  facet_wrap(~RE,scales="free") + theme_bw()

p = vessel_RE_df_spat_long %>% ggplot(aes(y=spat_cluster,x=RE_value,color=spat_cluster)) + geom_boxplot(outlier.shape = NA) +
  geom_jitter(height = 0.15,alpha=0.7)+
  facet_wrap(~RE,ncol=1, scales = "free",
             labeller = labeller(RE = c('Surv_Vess_rand_par' = "Survey",
                                        'Port_Vess_rand_par' = "Landing Port Distance",
                                        'Trad_Vess_rand_par' = "Previous Effort",
                                        'Cpuelag_Vess_rand_par' = "CPUE Season Lag",
                                        'CpueV_Vess_rand_par' = "CPUE-Vessel"))) +
  scale_color_manual(values=(natparks.pals("Volcanoes"))) + # rev before nat.park.pals if you want
  theme_bw() + 
  theme(legend.position = "none")+
  labs(x="Fixed + Random Effect Size",y="Spatial Strategy Archetype")
p
# pdf("plots/spatial_archetype_RE.pdf")
# print(p)
# dev.off()

# Tukey HSD 
Trad_tukey_input = vessel_RE_df_spat_long %>% filter(RE=="Trad_Vess_rand_par")
TukeyHSD(aov(RE_value ~ spat_cluster, Trad_tukey_input))

anova(lm(RE_value ~ spat_cluster, Trad_tukey_input))

Port_tukey_input = vessel_RE_df_spat_long %>% filter(RE=="Port_Vess_rand_par")
TukeyHSD(aov(RE_value ~ spat_cluster, Port_tukey_input))

anova(lm(RE_value ~ spat_cluster, Port_tukey_input))

Surv_tukey_input = vessel_RE_df_spat_long %>% filter(RE=="Surv_Vess_rand_par")
TukeyHSD(aov(RE_value ~ spat_cluster, Surv_tukey_input))

Cpuelag_tukey_input = vessel_RE_df_spat_long %>% filter(RE=="Cpuelag_Vess_rand_par")
TukeyHSD(aov(RE_value ~ spat_cluster, Cpuelag_tukey_input))

CpueV_tukey_input = vessel_RE_df_spat_long %>% filter(RE=="CpueV_Vess_rand_par")
TukeyHSD(aov(RE_value ~ spat_cluster, CpueV_tukey_input))

# 3 Vessel specs and ITQ with clusters ----

vess_AKFIN = read.csv("data/AKFIN_vess_specs.csv")

vessel_index_tbl = TheD %>% select(vessel,vessel_num) %>% distinct() %>% left_join(vess_AKFIN)

spat_cluster_df_join = read.csv("R/model/output/spat_clust_3_best_clusters_2022_rand.tune.csv")[,-1] %>% 
  # mutate(spat_cluster = case_when(spat_cluster == 1 ~ "West",
  #                                 spat_cluster == 2 ~ "South",
  #                                 spat_cluster == 3 ~ "North"))
  mutate(spat_cluster = case_when(spat_cluster == 1 ~ "West",
                                  spat_cluster == 2 ~ "North",
                                  spat_cluster == 3 ~ "South"))

vess_AKFIN_RE = vessel_index_tbl %>% left_join(spat_cluster_df_join,by=c("vessel_num"="vessel"))

p1 = ggplot(data=vess_AKFIN_RE,aes(x=AKFIN_LENGTH*0.3048,y=spat_cluster,color=spat_cluster)) + geom_boxplot(outlier.shape = NA) + 
  geom_jitter(height = 0.15,alpha=0.7)+
  scale_color_manual(values=(natparks.pals("Volcanoes"))) + # rev before nat.park.pals if you want
  labs(y="Spatial Strategy Archetype", x="Vessel Length (m)") + theme_bw() + 
  theme(legend.position = "none",
        text = element_text(size = 15))

# TUKEY HSD 
TukeyHSD(aov(AKFIN_LENGTH ~ spat_cluster, vess_AKFIN_RE))
TukeyHSD(aov(AKFIN_HPOWER ~ spat_cluster, vess_AKFIN_RE))
TukeyHSD(aov(AKFIN_FUEL_CAPACITY ~ spat_cluster, vess_AKFIN_RE))
TukeyHSD(aov(AKFIN_HOLD ~ spat_cluster, vess_AKFIN_RE))


# figure out how many vessels and which are in the E Geroge cluster that drives the NW focus
vess_AKFIN_spat_clust = spat_clust_kmeans %>% left_join(vessel_index_tbl,by=c("vessel"="vessel_num"))

vess_AKFIN_spat_clust_3 = vess_AKFIN_spat_clust %>% filter(spat_cluster==3) 

vess_AKFIN_spat_clust_3_sf = cpue_grid %>% inner_join(vess_AKFIN_spat_clust_3) %>%
  mutate(vessel_length_name = paste(vessel,"-",AKFIN_LENGTH)) %>% dplyr::arrange(AKFIN_LENGTH)

ggplot() + geom_sf(data=vess_AKFIN_spat_clust_3_sf,aes(fill=Int_Area_Vess_rand)) +
  geom_sf(data=vess_AKFIN_spat_clust_3_sf,fill=NA) +
  geom_sf(data=world) +
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2), expand = FALSE) +
  scale_fill_gradientn(colours=c("darkred","white", "darkgreen"), na.value = "grey98",
                       limits = c(-1.5, 1.5)) +
  theme_bw()  +
  facet_wrap(~as.factor(vessel_length_name))

landing_port_sum = cpue_port_clean_diff %>% left_join(vess_AKFIN_spat_clust,by=c("vessel"="vessel.y")) %>%
  filter(choice==1) %>%
  group_by(spat_cluster,PORT_NAME) %>%
summarise(delivery = sum(cpue_x*effort_x,na.rm=T)) %>%
group_by(spat_cluster) %>%
mutate(delivery_prop = delivery/sum(delivery)) %>% drop_na()


# spatial clusters and 
cpue_dat <-read.csv("data/cpue_snow_clean_2022.csv") %>% select(-X)
cpue_dat <- cpue_dat %>%
  #clean out strays
  filter(lat < 64 & lat > 53 & lon > -180 & lon < -155 & season > 2006)

vessel_ITQ_eff = cpue_dat %>% group_by(vessel,season) %>%
  summarise(sum_catch = sum(number,na.rm=T),
            sum_eff = sum(effort,na.rm=T)) %>%
  mutate(sum_cpue = sum_catch/sum_eff)

spat_clust_ITQ_eff =  vess_AKFIN_RE %>% left_join(vessel_ITQ_eff)

spat_clust_summarize = spat_clust_ITQ_eff %>% group_by(spat_cluster,season) %>%
  summarise(med_catch = median(sum_catch),
            mean_catch = mean(sum_catch),
            se_catch = sd(sum_catch)/sqrt(n()),
            med_cpue= median(sum_cpue),
            mean_cpue = mean(sum_cpue),
            se_cpue = sd(sum_cpue)/sqrt(n()),) 

p2 = spat_clust_summarize %>% ggplot() + 
  # geom_errorbar(aes(ymin=mean_catch-1.96*se_catch, ymax=mean_catch+1.96*se_catch, x= season,color=spat_cluster), width=1,lwd=1,
  #               position=position_dodge(0.05)) +
  geom_ribbon(aes(ymin=(mean_catch-1.96*se_catch)/1000, ymax=(mean_catch+1.96*se_catch)/1000, x= season,fill=spat_cluster),alpha=0.5) +
  geom_line(aes(x=season,y=mean_catch/1000,color=spat_cluster),lwd=1.5) +
  scale_color_manual(values=(natparks.pals("Volcanoes"))) + scale_fill_manual(values=(natparks.pals("Volcanoes"))) +
  labs(x="Season",y="Total Individuals Caught (1000s)",colour = "Spatial Archetype",fill="Spatial Archetype") +
  theme_bw() + 
  theme(text = element_text(size = 15),
        legend.title = element_text(size = 10),
        legend.text= element_text(size = 10),
        legend.justification=c(0,0), 
        legend.position = c(0.01,0.01)) + 
  ylim(-200,1400)

spat_clust_summarize %>% ggplot() + 
  # geom_errorbar(aes(ymin=mean_catch-1.96*se_catch, ymax=mean_catch+1.96*se_catch, x= season,color=spat_cluster), width=1,lwd=1,
  #               position=position_dodge(0.05)) +
  geom_ribbon(aes(ymin=(mean_cpue-1.96*se_cpue)/1, ymax=(mean_cpue+1.96*se_cpue)/1, x= season,fill=spat_cluster),alpha=0.5) +
  geom_line(aes(x=season,y=mean_cpue/1,color=spat_cluster),lwd=1.5) +
  scale_color_manual(values=(natparks.pals("Volcanoes"))) + scale_fill_manual(values=(natparks.pals("Volcanoes"))) +
  labs(x="Season",y="CPUE",colour = "Spatial Archetype",fill="Spatial Archetype") +
  theme_bw() 

library(patchwork)
p = p1+p2+
  plot_layout(ncol=2, byrow = T,widths=c(1,2))

# pdf("plots/spatial_archetype_vess_length_ITQ.pdf",width=12,height=4.5)
# print(p)
# dev.off()

#### Look at vessel specs and TAC vs covariates
spat_clust_ITQ_eff_RE = spat_clust_ITQ_eff %>% left_join(vessel_RE_df_spat, by=c("vessel_num"="vessel","spat_cluster"="spat_cluster"))

spat_clust_ITQ_eff_RE_noseason = spat_clust_ITQ_eff_RE %>% select(-c(season,sum_catch,sum_eff,sum_cpue)) %>% distinct()

spat_clust_ITQ_eff_RE_noseason %>% ggplot(aes(x=AKFIN_LENGTH,y=Port_Vess_rand_par,color=spat_cluster)) + geom_point() + 
  geom_smooth(method="lm",alpha=0.2)

summary(lm(data=spat_clust_ITQ_eff_RE_noseason,Cpuelag_Vess_rand_par~AKFIN_LENGTH))

spat_clust_ITQ_eff %>% ggplot() + geom_line(aes(x=season,y=sum_cpue, group = vessel,color=spat_cluster),alpha=0.9) 

# facet wrap on POrt RE and CPUE
p.vals = sapply(unique(spat_clust_ITQ_eff_RE$season), function(i) {
  coef(summary(lm(data=filter(spat_clust_ITQ_eff_RE, season== i), sum_cpue ~ Port_Vess_rand_par)))[2,4]
})
names(p.vals) = unique(spat_clust_ITQ_eff_RE$season)

ggplot() + 
  geom_point(data= spat_clust_ITQ_eff_RE,aes(x=Port_Vess_rand_par,y=sum_cpue)) +
  geom_smooth(method="lm",alpha=0.2) +
  geom_smooth(data=spat_clust_ITQ_eff_RE[spat_clust_ITQ_eff_RE$season %in% names(p.vals)[p.vals < 0.05],], 
              aes(Port_Vess_rand_par, sum_cpue), method='lm')+
  facet_wrap(~season, scales = "free")


sum(is.na(((spat_clust_ITQ_eff_RE$sum_cpue))))


# Select only values of z for which regression p-value is < 0.05   
plt + geom_smooth(data=d[d$z %in% names(p.vals)[p.vals < 0.05],], 
                  aes(x, y, colour=z), method='lm')

Surv_Vess_rand_par
Port_Vess_rand_par
CpueV_Vess_rand_par
Cpuelag_Vess_rand_par
Trad_Vess_rand_par



