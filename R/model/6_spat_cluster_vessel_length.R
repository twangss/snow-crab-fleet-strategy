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

vessel_RE_df_spat


p1 = ggplot(data=vess_AKFIN_RE,aes(x=AKFIN_LENGTH*0.3048,y=spat_cluster,color=spat_cluster)) + geom_boxplot(outlier.shape = NA) + 
  geom_jitter(height = 0.15,alpha=0.7)+
  scale_color_manual(values=(natparks.pals("Volcanoes"))) + # rev before nat.park.pals if you want
  labs(y="Spatial Strategy Archetype", x="Vessel Length (m)") + theme_bw() + 
  theme(legend.position = "none",
        text = element_text(size = 15))

# pdf("plots/spatial_archetype_vess_length.pdf",width=6,height=4)
# print(p)
# dev.off()

# TUKEY HSD FTW!!!
TukeyHSD(aov(AKFIN_LENGTH ~ spat_cluster, vess_AKFIN_RE))
TukeyHSD(aov(AKFIN_HPOWER ~ spat_cluster, vess_AKFIN_RE))
TukeyHSD(aov(AKFIN_FUEL_CAPACITY ~ spat_cluster, vess_AKFIN_RE))
TukeyHSD(aov(AKFIN_HOLD ~ spat_cluster, vess_AKFIN_RE))


# figure out how many vessels and which are in the E Geroge cluster that drives the NW focus
vess_AKFIN_spat_clust = spat_clust_kmeans%>% left_join(vessel_index_tbl,by=c("vessel"="vessel_num"))

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


# spatial clusters and 
  cpue_dat <-read.csv("data/cpue_snow_clean_2022.csv") %>% select(-X)
  cpue_dat <- cpue_dat %>%
    #clean out strays
    filter(lat < 64 & lat > 53 & lon > -180 & lon < -155 & season > 2006)
  
  vessel_ITQ_eff = cpue_dat %>% group_by(vessel,season) %>%
    summarise(sum_catch = sum(number*avg_wt,na.rm=T),
              sum_eff = sum(effort,na.rm=T)) %>%
    mutate(sum_cpue = sum_catch/sum_eff)
  
 spat_clust_ITQ_eff =  vess_AKFIN_RE %>% left_join(vessel_ITQ_eff)
 
 spat_clust_summarize = spat_clust_ITQ_eff %>% group_by(spat_cluster,season) %>%
   summarise(med_catch = median(sum_catch),
             mean_catch = mean(sum_catch),
             se_catch = sd(sum_catch)/sqrt(n()),
             med_cpue= median(sum_cpue),
             mean_cpue = mean(sum_cpue),
             se_cpue = sd(sum_cpue)/sqrt(n()),
             n_vessel = length(unique(vessel)),
             mean_catch_per_vess = mean_catch/n_vessel,
             se_catch_per_vess = se_catch/n_vessel) 
 
 p2 = spat_clust_summarize %>% ggplot() + 
   # geom_errorbar(aes(ymin=mean_catch-1.96*se_catch, ymax=mean_catch+1.96*se_catch, x= season,color=spat_cluster), width=1,lwd=1,
   #               position=position_dodge(0.05)) +
   geom_ribbon(aes(ymin=(mean_catch_per_vess-1.96*se_catch_per_vess), ymax=(mean_catch_per_vess+1.96*se_catch_per_vess), x= season,fill=spat_cluster),alpha=0.5) +
   geom_line(aes(x=season,y=mean_catch_per_vess,color=spat_cluster),lwd=1.5) +
   scale_color_manual(values=(natparks.pals("Volcanoes"))) + scale_fill_manual(values=(natparks.pals("Volcanoes"))) +
   labs(x="Season",y="Mean owned/leased quota per vessel \n (lbs/vessel)",colour = "Spatial Archetype",fill="Spatial Archetype") +
   theme_bw() + 
   theme(text = element_text(size = 15),
         legend.title = element_text(size = 10),
         legend.text= element_text(size = 10),
         legend.justification=c(0,0), 
         legend.position = c(0.01,0.7))
 
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


  