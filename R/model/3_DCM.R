library(TMB) 
library(tidyverse)

compile("R/model/mnl_fixed_random.cpp", flags="-Wno-ignored-attributes")
dyn.load(dynlib("R/model/mnl_fixed_random"))

# Run TMB model 
TheD=read.csv("data/cpue_mnl_wide_33areas_109vessels_statarea_2023_0922.csv") 
# TheD=read.csv("data/cpue_mnl_wide_32areas_108vessels_statarea_2023_0329.csv")

vess_keep = TheD %>% select(vessel,season) %>% unique() %>% group_by(vessel) %>% summarise(n=n()) %>%
  filter(n>1) %>% pull(vessel)

TheD = TheD %>% filter(vessel %in% vess_keep)

# TheD = TheD %>% group_by(choice_id) %>% filter(prop_eff_cid == max(prop_eff_cid))
TheD = TheD %>% filter(prop_eff_cid >0.1)

# TheD=read.csv(file_name)
# drop the NA vessels real quick
TheD=TheD %>% drop_na(c(vessel_num))

# filter out
k=length(unique(TheD$stat_area))
col_start = 7
col_na = c((col_start+5*k):(col_start+6*k-1),(col_start+7*k):(col_start+8*k-1),(col_start+8*k):(col_start+9*k-1),
           (col_start+9*k):(col_start+10*k-1),(col_start+10*k):(col_start+11*k-1),(col_start+11*k):(col_start+12*k-1),(col_start+12*k):(col_start+13*k-1))

TheD = TheD[complete.cases(TheD[ , col_na]),] 

# Area Num
Areas <- sort(unique(TheD$stat_area)) 
Narea <- length(Areas)
ActArea <- rep(0,length(TheD$stat_area))
for (II in 1:length(ActArea)) ActArea[II] <- which(TheD$stat_area[II] == Areas)
# starts arrays at 0
ActArea <- ActArea - 1   

# Vessel Num
Vessels <- unique(TheD$vessel_num)
Nvessel <- length(Vessels)
ActVess <- rep(0,length(TheD$vessel_num))
for (II in 1:length(ActVess)) ActVess[II] <- which(TheD$vessel_num[II] == Vessels)
ActVess <- ActVess - 1  

# Season Num
Seasons <- unique(TheD$season)
Nseason <- length(Seasons)
ActSeason <- rep(0,length(TheD$season))
for (II in 1:length(ActSeason)) ActSeason[II] <- which(TheD$season[II] == Seasons)
ActSeason <- ActSeason - 1 

# define matrices for covariates
k=Narea
col_start = 7
Trad_C = TheD[,col_start:(col_start+(k-1))]
CpueF_C = TheD[,(col_start+k):(col_start+2*k-1)] 
CpueV_C = TheD[,(col_start+2*k):(col_start+3*k-1)] 
Cpuelag_C = TheD[,(col_start+3*k):(col_start+4*k-1)]
Sort_C = TheD[,(col_start+4*k):(col_start+5*k-1)]
Ice_C = TheD[,(col_start+5*k):(col_start+6*k-1)] # this has natural NAs to remove
Cong_C = TheD[,(col_start+6*k):(col_start+7*k-1)]
Port_C = TheD[,(col_start+7*k):(col_start+8*k-1)] # this has natural NAs to remove
Surv_C = TheD[,(col_start+8*k):(col_start+9*k-1)]/1000
CvF_C = TheD[,(col_start+9*k):(col_start+10*k-1)]
CvV_C = TheD[,(col_start+10*k):(col_start+11*k-1)]
Wind_C = TheD[,(col_start+11*k):(col_start+12*k-1)]
SurvOff_C = TheD[,(col_start+12*k):(col_start+13*k-1)]

# Fit full model
data_full<- list(# Structure
                 Narea=Narea,ActArea=ActArea,ActVess=ActVess,Nvessel=Nvessel,ActSeason=ActSeason,Nseason=Nseason,
                 # Data 
                 CpueV_C=as.matrix(CpueV_C),CpueF_C=as.matrix(CpueF_C),Trad_C=as.matrix(Trad_C),
                 Ice_C=as.matrix(Ice_C),Cpuelag_C=as.matrix(Cpuelag_C),Sort_C=as.matrix(Sort_C),
                 Cong_C=as.matrix(Cong_C),Port_C=as.matrix(Port_C),Surv_C=as.matrix(Surv_C),
                 CvF_C=as.matrix(CvF_C),CvV_C=as.matrix(CvV_C),Wind_C=as.matrix(Wind_C),SurvOff_C=as.matrix(SurvOff_C),
                 # Flags for Random Effects
                 # Fleet-wide
                 Int_AreaSeason_rand_flag=0,
                 CpueV_Season_rand_flag=0,
                 CpueF_Season_rand_flag=0,
                 # Unique Vessel
                 Int_AreaVess_rand_flag=1,
                 Ice_Vess_rand_flag=0,Cong_Vess_rand_flag=0,Port_Vess_rand_flag=1,
                 Trad_Vess_rand_flag=1,
                 CpueF_Vess_rand_flag=0,CpueV_Vess_rand_flag=1,
                 CvF_Vess_rand_flag=0,CvV_Vess_rand_flag=0,
                 Wind_Vess_rand_flag=0,
                 SurvOff_Vess_rand_flag=0,
                 Surv_Vess_rand_flag=1,
                 Cpuelag_Vess_rand_flag=1,
                 # Vessel-Season
                 Ice_Vess_Season_rand_flag=0)
# parameter
parameters_fleet <- list(# Fixed - Fleetwide
                        AreaPar=rep(0,Narea-1),CpueV_par=0.0,CpueF_par=0.0,Trad_par=0.0,
                        Ice_par=0.0,Cong_par=0.0,Cpuelag_par=0.0,Sort_par=0.0,Port_par=0.0,Surv_par=0.0,CvF_par=0.0,CvV_par=0.0,
                        Wind_par=0.0,SurvOff_par=0.0,
                         # Random - Fleet 
                         Int_Area_Season_rand=matrix(0,nrow=Narea-1,ncol=Nseason),logSigma_Area_Season_rand=-0.5,
                         CpueV_Season_rand_par=rep(0.0,Nseason),logSigma_CpueV_Season_rand=0.0,
                        CpueF_Season_rand_par=rep(0.0,Nseason),logSigma_CpueF_Season_rand=0.0,
                        # Random - Unique Vessel 
                        Int_Area_Vessel_rand=matrix(0,nrow=Narea-1,ncol=Nvessel),logSigma_Area_Vessel_rand=-0.5,
                        Ice_Vess_rand_par=rep(0.0,Nvessel),logSigma_Ice_Vessel_rand=0.0,
                        Port_Vess_rand_par=rep(0.0,Nvessel),logSigma_Port_Vessel_rand=0.0,
                        Cong_Vess_rand_par=rep(0.0,Nvessel),logSigma_Cong_Vessel_rand=0.0,
                        Trad_Vess_rand_par=rep(0.0,Nvessel),logSigma_Trad_Vessel_rand=0.0,
                        CpueF_Vess_rand_par=rep(0.0,Nvessel),logSigma_CpueF_Vessel_rand=0.0,
                        CpueV_Vess_rand_par=rep(0.0,Nvessel),logSigma_CpueV_Vessel_rand=0.0,
                        CvF_Vess_rand_par=rep(0.0,Nvessel),logSigma_CvF_Vessel_rand=0.0,
                        CvV_Vess_rand_par=rep(0.0,Nvessel),logSigma_CvV_Vessel_rand=0.0,
                        Wind_Vess_rand_par=rep(0.0,Nvessel),logSigma_Wind_Vessel_rand=0.0,
                        SurvOff_Vess_rand_par=rep(0.0,Nvessel),logSigma_SurvOff_Vessel_rand=0.0,
                        Surv_Vess_rand_par=rep(0.0,Nvessel),logSigma_Surv_Vessel_rand=0.0,
                        Cpuelag_Vess_rand_par=rep(0.0,Nvessel),logSigma_Cpuelag_Vessel_rand=0.0,
                        
                        # Random - Unique Vessel-Season
                        Ice_Vess_Season_rand_par=matrix(0,nrow=Nvessel,ncol=Nseason),logSigma_Ice_Vessel_Season_rand=0.0)

map_full_fleet=list(# Fixed
                    # CpueV_par=factor(NA),
                    # CpueF_par=factor(NA),Ice_par=factor(NA),Trad_par=factor(NA),
                    # Cpuelag_par=factor(NA), #Sort_par=factor(NA),Port_par=factor(NA),Cong_par=factor(NA),
                    Surv_par=factor(NA),
                    # CvF_par=factor(NA),
                    # CvV_par=factor(NA),
                    # Wind_par=factor(NA),
                    SurvOff_par=factor(NA),
                    
                    # Random - Fleet 
                    Int_Area_Season_rand=rep(factor(NA),length=(Narea-1)*Nseason),logSigma_Area_Season_rand=factor(NA),
                    CpueV_Season_rand_par=rep(factor(NA),Nseason),logSigma_CpueV_Season_rand=factor(NA),
                    CpueF_Season_rand_par=rep(factor(NA),Nseason),logSigma_CpueF_Season_rand=factor(NA),
                    
                    # Random - Unique Vessel 
                    # Int_Area_Vessel_rand=rep(factor(NA),length=((Narea-1)*Nvessel)),logSigma_Area_Vessel_rand=factor(NA),
                    Ice_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Ice_Vessel_rand=factor(NA),
                    # Port_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Port_Vessel_rand=factor(NA),
                    Cong_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Cong_Vessel_rand=factor(NA),
                    # Trad_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Trad_Vessel_rand=factor(NA),
                    CpueF_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_CpueF_Vessel_rand=factor(NA),
                    # CpueV_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_CpueV_Vessel_rand=factor(NA),
                    CvF_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_CvF_Vessel_rand=factor(NA),
                    CvV_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_CvV_Vessel_rand=factor(NA),
                    Wind_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Wind_Vessel_rand=factor(NA),
                    SurvOff_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_SurvOff_Vessel_rand=factor(NA),
                    # Surv_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Surv_Vessel_rand=factor(NA),
                    # Cpuelag_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Cpuelag_Vessel_rand=factor(NA),

                    # Random - Unique Vessel-Season
                    Ice_Vess_Season_rand_par=rep(factor(NA),length=Nvessel*Nseason),logSigma_Ice_Vessel_Season_rand=factor(NA)
                    )


model_full_fleet <- MakeADFun(data=data_full,parameters_fleet,map=map_full_fleet,DLL="mnl_fixed_random",
                              control=list(eval.max=10000,iter.max=1000,rel.tol=1e-15),silent=T,
                              random=c(
                                # Fleet
                                # "Int_Area_Season_rand",
                                # "CpueV_Season_rand_par",
                                # "CpueF_Season_rand_par",
                                
                                # # Vessel
                                "Int_Area_Vessel_rand",
                                # "Ice_Vess_rand_par"
                                "Port_Vess_rand_par",
                                # "Cong_Vess_rand_par",
                                "Trad_Vess_rand_par",
                                # "CpueF_Vess_rand_par"
                                "CpueV_Vess_rand_par",
                                # "CvF_Vess_rand_par",
                                # "CvV_Vess_rand_par"
                                # "Wind_Vess_rand_par"
                                # "SurvOff_Vess_rand_par"
                                "Surv_Vess_rand_par",
                                "Cpuelag_Vess_rand_par"
                                
                                # # # Vessel-Season
                                # "Ice_Vess_Season_rand_par"
                              ))
start.time <- Sys.time()
fit <- nlminb(model_full_fleet$par,model_full_fleet$fn,model_full_fleet$gr,control=list(eval.max=100000,iter.max=100000,rel.tol=1e-15))
report=model_full_fleet$report()
sd_error_rep <- sdreport(model_full_fleet)
last.par.best=model_full_fleet$env$last.par.best
end.time <- Sys.time()
end.time - start.time

save(fit,file="R/model/output/fit_best_model_sel_22.97vess.rand.tune.rda")
save(model_full_fleet,file="R/model/output/model_best_model_sel_22.97vess.rand.tune.rda")
save(report,file="R/model/output/report_best_model_sel_22.97vess.rand.tune.rda")
save(sd_error_rep,file="R/model/output/sd_error_best_model_sel_22.97vess.rand.tune.rda")
save(last.par.best,file="R/model/output/pars_best_model_sel_22.97vess.rand.tune.rda")


load(file="R/model/output/fit_best_model_sel_22.97vess.rand.surv.trad.port.rda")
load(file="R/model/output/model_best_model_sel_22.97vess.rand.surv.trad.port.rda")
load("R/model/output/report_best_model_sel_22.97vess.rand.surv.trad.port.rda")
load("R/model/output/sd_error_best_model_sel_22.97vess.rand.surv.trad.port.rda")
load("R/model/output/pars_best_model_sel_22.97vess.rand.surv.trad.port.rda")

print(model_full_fleet$env$last.par.best)
print(model_full_fleet$par)
# Look at the report

# Extract the report
rep3 <-  model_full_fleet_2$report()
# Lets look at the gradients
print(model_full_fleet$gr())

# Tuning by restarting minimization from last best value (needs a trick for the random effects)
for (Itune in 1:5)
{
  cat("Itune",Itune,"\n")
  nameV <- names(model_full_fleet$env$last.par.best)
  `%ni%` <- Negate(`%in%`)
  model_full_fleet$p2 <- model_full_fleet$env$last.par.best[which(nameV %ni%
                                    c("Int_Area_Vessel_rand","Int_Area_Season_rand","Ice_Vess_rand_par","Ice_Vess_Season_rand_par",
                                      "CpueV_Season_rand_par","CpueF_Season_rand_par","Cong_Vess_rand_par","Trad_Vess_rand_par",
                                      "CpueF_Vess_rand_par","CpueV_Vess_rand_par","Cong_Vess_rand_par","Port_Vess_rand_par",
                                      "CvF_Vess_rand_par","CvV_Vess_rand_par","Wind_Vess_rand_par","SurvOff_Vess_rand_par","Surv_Vess_rand_par",
                                      "Cpuelag_Vess_rand_par"))]
  
  fit <- nlminb(model_full_fleet$p2,model_full_fleet$fn,model_full_fleet$gr,control=list(eval.max=10000,iter.max=1000,rel.tol=1e-15)) 
  # singular convergence
  print(fit)
  cat("gradient",max(abs(model_full_fleet$gr())),"\n")
  
}

report=model_full_fleet$report()
sd_error_rep <- sdreport(model_full_fleet)
last.par.best=model_full_fleet$env$last.par.best

tlike = model_full_fleet$fn(fit$par) # 22589.86

# get AICc
tlike= model_full_fleet$fn(fit$par)
AICc=2*length(model_full_fleet$par)+2*tlike + 
  (2*length(model_full_fleet$par)^2+2*length(model_full_fleet$par))/(nrow(CpueV_C)-length(model_full_fleet$par)-1)
print(AICc)

# get preds and AUCs
report <-  model_full_fleet$report()
preds <- report$preds
colnames(preds) <- sort(unique(TheD$stat_area)) 
preds_choice <- as.numeric(colnames(preds)[apply(preds,1,which.max)])

library(pROC)
roc.multi <- multiclass.roc(response=as.factor(TheD$stat_area),predictor= preds)
rs <- roc.multi[['rocs']]
plot.roc(rs[[1]])
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))
round(auc(roc.multi),2)

crf_eval = crfsuite::crf_evaluation(pred = preds_choice, obs = TheD$stat_area, labels = unique(TheD$stat_area))

