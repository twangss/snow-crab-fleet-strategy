library(ggplot2)
library(ggridges)
library(sf)
library(tidyverse)
library(ggsflabel)
library(NatParksPalettes)

#==== read in model outputs
# i think below should be the March outputs that made the nice 3 clusters
# fit <- readRDS("R/model/output/fit_arv_ars_cvs_cfs_cov_trv_cfv.rds")
# model_full_fleet <- readRDS("R/model/output/model_arv_ars_cvs_cfs_cov_trv_cfv.rds")
# sd_error_rep <- readRDS("R/model/output/sd_error_rep_arv_ars_cvs_cfs_cov_trv_cfv.rds")

load("R/model/output/pars_best_model_sel_22.97vess.rand.tune.rda")
load("R/model/output/sd_error_best_model_sel_22.97vess.rand.tune.rda")
load("R/model/output/report_best_model_sel_22.97vess.rand.tune.rda")
load("R/model/output/model_best_model_sel_22.97vess.rand.tune.rda")
load("R/model/output/fit_best_model_sel_22.97vess.rand.tune.rda")

#===== Fleet-wide strategies
#==== Fixed
#== Area Params
AreaPar = report$AreaPar
AreaPar=append(AreaPar,0)
area_par_df = bind_cols(stat_area = sort(unique(TheD$stat_area)),AreaPar=AreaPar)

cpue_grid=st_read("data/shapefiles/cpue_grid.shp")

grid_param=cpue_grid %>% inner_join(area_par_df)

lon_1<- -180
lon_2<- -159
lat_1<-52 
lat_2<-63

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# port labels
names <- c("St. Paul", "Dutch Harbor", "Akutan Bay","King Cove")
lat <- c(57.125,53.902917,54.1325,55.059720)
lon <- c(-170.284167,-166.518389, -165.775,-162.31987)

nudge_y <- c(1,-1,-1,1)
port_sf <- data.frame(names,lat,lon,nudge_y) %>% st_as_sf(coords = c("lon", "lat")) %>% st_set_crs(4326)

# island labels
names <- c("St. George", "St. Matthew Island")
lat <- c(56.605546,60.408611)
lon <- c(-169.559584, -172.72)
nudge_y <- c(-1,1)
island_sf <- data.frame(names,lat,lon,nudge_y) %>% st_as_sf(coords = c("lon", "lat")) %>% st_set_crs(4326)
rm(nudge_y)

p=ggplot() + geom_sf(data=grid_param,aes(fill=AreaPar)) +
  viridis::scale_fill_viridis(option = "G",direction = 1,limits = c(-3, 2.1), 
                              oob = scales::squish, # this gets really low values below limits to be just the legend
                              )+
  geom_sf(data=world) +
  # port points and labels
  geom_sf(data = port_sf, alpha = 1, size=3) +
  geom_sf_text_repel(data = port_sf,aes(label = names),
                      force = 10, nudge_x = 2,nudge_y=  c(1,-1,-1,1), seed = 10)+
  # island labels
  geom_sf_text_repel(data = island_sf,aes(label = names),
                     force = 10, nudge_x = 2,nudge_y= c(1,1), seed = 10)+
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2), expand = FALSE) + 
  theme_bw()+
  labs(fill='Area Intercept') +
  theme(legend.position = c(0, 0), 
        legend.justification = c(-.1, -.1),
        legend.box.background = element_rect(color = "black"),
        legend.title=element_text(size=12))

print(p)

# pdf("plots/fleet_area_intercepts.pdf",width=8,height=8)
# print(p)
# dev.off()

#== Covariate Params
# Effect Size
fit_par = fit$par[which(names(fit$par) %in% c("CpueV_par","CpueF_par","Ice_par","Trad_par",
                                    "Sort_par","Cong_par","Port_par",
                                    # "Surv_par","SurvOff_C",
                                    "CvF_par","CvV_par","Wind_par","Cpuelag_par"))] 
fit_par_sort = fit_par[sort(names(fit_par))]

par_sd = c(sd(unlist(c(CpueV_C))),sd(unlist(c(CpueF_C))),
  sd(unlist(c(Ice_C))),sd(unlist(c(Trad_C))),
  sd(unlist(c(Sort_C))),sd(unlist(c(Cong_C))),
  sd(unlist(c(Port_C))),
  # sd(unlist(c(Surv_C))),sd(unlist(c(SurvOff_C))),
  sd(unlist(c(CvF_C))),sd(unlist(c(CvV_C))),
  sd(unlist(c(Wind_C))),
  sd(unlist(c(Cpuelag_C))))

names(par_sd) = c("CpueV_sd","CpueF_sd","Ice_sd","Trad_sd",
                  "Sort_sd","Cong_sd","Port_sd",
                  # "Surv_sd","SurvOff_sd",
                  "CvF_sd","CvV_sd","Wind_sd","Cpuelag_sd")

par_sd_sort = par_sd[sort(names(par_sd))]

effect_size = fit_par_sort*par_sd_sort

# Effect SD
se=summary(sd_error_rep,"fixed")[,2]

sd_par = se[which(names(se) %in% c("CpueV_par","CpueF_par","Ice_par","Trad_par",
                                     "Sort_par","Cong_par","Port_par",
                                     # "Surv_par","SurvOff_par",
                                     "CvF_par","CvV_par","Wind_par","Cpuelag_par"))]

sd_par_sort = sd_par[sort(names(sd_par))]

effect_sd = sd_par_sort*par_sd_sort

param_effect_size_df=data.frame(effect_size,effect_sd) %>% rownames_to_column("Parameter") %>%
  mutate(CI95min=effect_size-1.96*effect_sd,
         CI95max=effect_size+1.96*effect_sd) 

effect_plot=data.frame(effect_size,effect_sd) %>% mutate(CI95min=effect_size-1.96*effect_sd,
                                                         CI95max=effect_size+1.96*effect_sd) %>%
  rownames_to_column("var") %>% 
  arrange(-effect_size)

effect_join_tbl = data.frame(var=c("CpueV_par","CpueF_par","Ice_par","Trad_par",
                 "Sort_par","Cong_par","Port_par",
                 "CvF_par","CvV_par","Wind_par","Cpuelag_par"),
           name=c("CPUE Vessel","CPUE Fleet","Ice","Previous Effort",
                 "Retained Catch","Other Vessel Pots","Landing Port Distance",
                 "Variability of CPUE Fleet","Variability of CPUE Vessel","Wind","CPUE Lag"),
           category = c("Crab Density","Sociality","Environment","Spatial Preference",
                        "Crab Density","Sociality","Cost",
                        "Sociality","Crab Density","Environment","Crab Density"))

effect_plot_join = effect_plot %>% left_join(effect_join_tbl)

p=ggplot(effect_plot_join,aes(x=reorder(name, -effect_size),y=effect_size,fill=category))+ 
  geom_bar(stat="identity", color="black", position=position_dodge())+
  # coord_flip()+
  geom_errorbar(aes(ymin=CI95min, ymax=CI95max), width=.2,
                position=position_dodge(.9)) +
  labs(x="Parameter",y="Effect Size")+
  geom_hline(yintercept = 0,color="black",linetype="solid",linewidth=1.1)+ 
  theme_bw() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.title=element_blank(),
        legend.position = c(0.8, 0.8),
  ) +
  scale_fill_manual(values=natparks.pals("Torres"))

print(p)

# pdf("plots/fleet_covariate_parms.pdf",width=8,height=8)
# print(p)
# dev.off()

#==== Random Fleet Structure
#== Area-Vessel Rndom Effects
area_vessel_rand = report$Int_Area_Vessel_rand
area_vessel_rand_full = rbind(area_vessel_rand,0)
# rownames(area_vessel_rand_full) = sort(unique(TheD$stat_area))
# colnames(area_vessel_rand_full) = seq(1,length(Vessels))
# 
# area_vessel_rand_melt=reshape2::melt(area_vessel_rand_full)
# area_vess_df=area_vessel_rand_melt%>%as.data.frame()%>%magrittr::set_colnames(c("stat_area","vessel","param"))
# area_par_vess_rand_df = area_vess_df %>% left_join(area_par_df, by="stat_area") %>%
#   mutate(area_vess_total = param+AreaPar)

#== Area-Season Random Effects
pars = model_full_fleet$env$last.par.best

Int_Area_Season_rand=pars[which(names(pars) %in% c("Int_Area_Season_rand"))]

Int_Area_Season_rand_df = matrix(Int_Area_Season_rand,nrow = Narea-1,ncol = Nseason) %>% 
  as.data.frame() %>% rbind(0)
colnames(Int_Area_Season_rand_df) = Seasons
Int_Area_Season_rand_df$stat_area = sort(unique(TheD$stat_area))
area_season_df = Int_Area_Season_rand_df %>% pivot_longer(!stat_area,names_to = "season", values_to = "Int_Area_Season_rand")%>%
  mutate(season=as.numeric(season))

#== CpueV-Season & CpueF-Season Random Effects
# CpueV_Season_rand_par=pars[which(names(pars) %in% c("CpueV_Season_rand_par"))] 
# CpueF_Season_rand_par=pars[which(names(pars) %in% c("CpueF_Season_rand_par"))] 
# CpueV_Season_rand_par_df=data.frame(Seasons,CpueV_Season_rand_par,CpueF_Season_rand_par)
# no relation with TAC

#===== Unique Vessel Strategies
#== Area-Vessel Random Effects
area_vessel_rand = report$Int_Area_Vessel_rand %>% 
  as.data.frame()%>%rbind(0)
colnames(area_vessel_rand) = unique(TheD$vessel_num)
area_vessel_rand$stat_area = sort(unique(TheD$stat_area))
area_vessel_rand_df=area_vessel_rand %>% 
  pivot_longer(!stat_area, names_to="vessel",
               names_transform = list(vessel = as.integer),
               values_to = "Int_Area_Vess_rand")

#== Area Int FE + Area-Vessel RE 
area_par_vess_rand_df = area_vessel_rand_df %>% left_join(area_par_df, by="stat_area") %>%
  mutate(area_vess_total = Int_Area_Vess_rand+AreaPar)

#== Covariate RE 
pars = model_full_fleet$env$last.par.best

## Tradition Vessel Random Effects 
Trad_Vess_rand_par=pars[which(names(pars) %in% c("Trad_Vess_rand_par"))]*sd(unlist(c(Trad_C)))
# save this explicitly to add to big RE table
Trad_par = pars[which(names(pars) %in% c("Trad_par"))]*sd(unlist(c(Trad_C)))
hist(Trad_Vess_rand_par)

## Surv Vessel Random Effects 
Surv_Vess_rand_par=pars[which(names(pars) %in% c("Surv_Vess_rand_par"))]*sd(unlist(c(Surv_C)))
# save this explicitly to add to big RE table
Surv_par = pars[which(names(pars) %in% c("Surv_par"))]*sd(unlist(c(Surv_C)))
hist(Surv_Vess_rand_par)

## Congestion Random Effects 
# Cong_Vess_rand_par=pars[which(names(pars) %in% c("Cong_Vess_rand_par"))] 
# Cong_par = pars[which(names(pars) %in% c("Cong_par"))] 
# hist(Cong_Vess_rand_par)

## CPUE V Vessel Random Effects 
CpueV_Vess_rand_par=pars[which(names(pars) %in% c("CpueV_Vess_rand_par"))]*sd(unlist(c(CpueV_C)))
CpueV_par = pars[which(names(pars) %in% c("CpueV_par"))]*sd(unlist(c(CpueV_C)))
hist(CpueV_Vess_rand_par)

## CpueLAg Vessel Random Effects 
Cpuelag_Vess_rand_par=pars[which(names(pars) %in% c("Cpuelag_Vess_rand_par"))]*sd(unlist(c(Cpuelag_C)))
Cpuelag_par = pars[which(names(pars) %in% c("Cpuelag_par"))]*sd(unlist(c(Cpuelag_C)))
hist(Cpuelag_Vess_rand_par)

## Ice Vessel Random Effects 
# Ice_Vess_rand_par=pars[which(names(pars) %in% c("Ice_Vess_rand_par"))] 
# # save this explicitly to add to big RE table
# Ice_par = pars[which(names(pars) %in% c("Ice_par"))] 
# hist(Ice_Vess_rand_par)

## Port Vessel Random Effects 
Port_Vess_rand_par=pars[which(names(pars) %in% c("Port_Vess_rand_par"))]*sd(unlist(c(Port_C)))
# save this explicitly to add to big RE table
Port_par = pars[which(names(pars) %in% c("Port_par"))]*sd(unlist(c(Port_C)))
hist(Port_Vess_rand_par)


# Compile everything into big RE table
vessel_RE_df=data.frame(vessel=Vessels,
                        Trad=Trad_par+Trad_Vess_rand_par,
                        Trad_Vess_rand_par=Trad_Vess_rand_par,
                        Surv=0+Surv_Vess_rand_par,
                        Surv_Vess_rand_par=Surv_Vess_rand_par,
                        # Cong=Cong_par+Cong_Vess_rand_par,
                        # Cong_Vess_rand_par=Cong_Vess_rand_par,
                        CpueV=CpueV_par+CpueV_Vess_rand_par,
                        CpueV_Vess_rand_par=CpueV_Vess_rand_par,
                        Cpuelag=Cpuelag_par+Cpuelag_Vess_rand_par,
                        Cpuelag_Vess_rand_par=Cpuelag_Vess_rand_par,
                        # Ice=Ice_par+Ice_Vess_rand_par,
                        # Ice_Vess_rand_par=Ice_Vess_rand_par,
                        Port=Port_par+Port_Vess_rand_par,
                        Port_Vess_rand_par=Port_Vess_rand_par)

# pivot wide the Area Int FE + Area-Vessel RE table to join and cluster better
area_par_vess_rand_df_wide = area_par_vess_rand_df %>% select(stat_area,vessel,area_vess_total)%>%
  pivot_wider(names_from=stat_area,values_from=area_vess_total)
vessel_RE_area_df = vessel_RE_df %>% left_join(area_par_vess_rand_df_wide)

