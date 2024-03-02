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


# check to see if all vessels and stat areas are present in the training_df, yes!
for (i in 2007:2022){
  print(i)
  training_df = TheD %>% filter(season != i)
  print(length(unique(training_df$vessel))==97)
  print(length(unique(training_df$stat_area))==33)
}

leave.season.out.tab = data.frame(season=NA,AUC=NA,accuracy=NA,precision=NA,train_auc=NA,train_acc=NA,train_prec=NA)
season_vec = 2007:2022

for (i in 1:length(season_vec)){
  season_tmp = season_vec[i]
  print(season_tmp)
  
  leave.season.out.tab[i,"season"] = season_tmp
  
  training_df = TheD %>% filter(season != season_tmp)
  testing_df = TheD %>% filter(season == season_tmp)
  
  # Area Num
  Areas <- sort(unique(training_df$stat_area)) 
  Narea <- length(Areas)
  ActArea <- rep(0,length(training_df$stat_area))
  for (II in 1:length(ActArea)) ActArea[II] <- which(training_df$stat_area[II] == Areas)
  # starts arrays at 0
  ActArea <- ActArea - 1   
  
  # Vessel Num
  Vessels <- unique(training_df$vessel_num)
  Nvessel <- length(Vessels)
  ActVess <- rep(0,length(training_df$vessel_num))
  for (II in 1:length(ActVess)) ActVess[II] <- which(training_df$vessel_num[II] == Vessels)
  ActVess <- ActVess - 1  
  
  # Season Num
  Seasons <- unique(training_df$season)
  Nseason <- length(Seasons)
  ActSeason <- rep(0,length(training_df$season))
  for (II in 1:length(ActSeason)) ActSeason[II] <- which(training_df$season[II] == Seasons)
  ActSeason <- ActSeason - 1 
  
  # define matrices for covariates
  k=Narea
  col_start = 7
  Trad_C = training_df[,col_start:(col_start+(k-1))]
  CpueF_C = training_df[,(col_start+k):(col_start+2*k-1)] 
  CpueV_C = training_df[,(col_start+2*k):(col_start+3*k-1)] 
  Cpuelag_C = training_df[,(col_start+3*k):(col_start+4*k-1)]
  Sort_C = training_df[,(col_start+4*k):(col_start+5*k-1)]
  Ice_C = training_df[,(col_start+5*k):(col_start+6*k-1)] # this has natural NAs to remove
  Cong_C = training_df[,(col_start+6*k):(col_start+7*k-1)]
  Port_C = training_df[,(col_start+7*k):(col_start+8*k-1)] # this has natural NAs to remove
  Surv_C = training_df[,(col_start+8*k):(col_start+9*k-1)]/1000
  CvF_C = training_df[,(col_start+9*k):(col_start+10*k-1)]
  CvV_C = training_df[,(col_start+10*k):(col_start+11*k-1)]
  Wind_C = training_df[,(col_start+11*k):(col_start+12*k-1)]
  SurvOff_C = training_df[,(col_start+12*k):(col_start+13*k-1)]
  
  # Fit full model
  data_train <- list(# Structure
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
    CpueF_Vess_rand_flag=0,CpueV_Vess_rand_flag=0,
    CvF_Vess_rand_flag=0,CvV_Vess_rand_flag=0,
    Wind_Vess_rand_flag=0,
    SurvOff_Vess_rand_flag=0,
    Surv_Vess_rand_flag=1,
    Cpuelag_Vess_rand_flag=0,
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
    CpueV_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_CpueV_Vessel_rand=factor(NA),
    CvF_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_CvF_Vessel_rand=factor(NA),
    CvV_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_CvV_Vessel_rand=factor(NA),
    Wind_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Wind_Vessel_rand=factor(NA),
    SurvOff_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_SurvOff_Vessel_rand=factor(NA),
    # Surv_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Surv_Vessel_rand=factor(NA),
    Cpuelag_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Cpuelag_Vessel_rand=factor(NA),
    
    # Random - Unique Vessel-Season
    Ice_Vess_Season_rand_par=rep(factor(NA),length=Nvessel*Nseason),logSigma_Ice_Vessel_Season_rand=factor(NA)
  )
  
  
  # model_train <- MakeADFun(data=data_train,parameters_fleet,map=map_full_fleet,DLL="mnl_fixed_random",
  #                          control=list(eval.max=10000,iter.max=1000,rel.tol=1e-15),silent=T,
  #                          random=c(
  #                            # Fleet
  #                            # "Int_Area_Season_rand"
  #                            # "CpueV_Season_rand_par",
  #                            # "CpueF_Season_rand_par",
  # 
  #                            # # Vessel
  #                            "Int_Area_Vessel_rand",
  #                            # "Ice_Vess_rand_par"
  #                            "Port_Vess_rand_par",
  #                            # "Cong_Vess_rand_par",
  #                            "Trad_Vess_rand_par",
  #                            # "CpueF_Vess_rand_par"
  #                            # "CpueV_Vess_rand_par"
  #                            # "CvF_Vess_rand_par",
  #                            # "CvV_Vess_rand_par"
  #                            # "Wind_Vess_rand_par"
  #                            # "SurvOff_Vess_rand_par"
  #                            "Surv_Vess_rand_par"
  #                            # "Cpuelag_Vess_rand_par"
  # 
  #                            # # # Vessel-Season
  #                            # "Ice_Vess_Season_rand_par"
  #                          ))
  # start.time <- Sys.time()
  # fit_train <- nlminb(model_train$par,model_train$fn,model_train$gr,control=list(eval.max=100000,iter.max=100000,rel.tol=1e-15))
  # report_train=model_train$report()
  # sd_error_rep_train <- sdreport(model_train)
  # last.par.best.train =model_train$env$last.par.best
  # end.time <- Sys.time()
  # end.time - start.time
  
load(file=paste("R/model/output/fit_train_",season_tmp,".rda",sep=""))
load(file=paste("R/model/output/model_train_",season_tmp,".rda",sep=""))
load(file=paste("R/model/output/report_train_",season_tmp,".rda",sep=""))
load(file=paste("R/model/output/sd_train_",season_tmp,".rda",sep=""))
  
  # save(fit_train,file=paste("R/model/output/fit_train_",season_tmp,".rda",sep=""))
  # save(model_train,file=paste("R/model/output/model_train_",season_tmp,".rda",sep=""))
  # save(report_train,file=paste("R/model/output/report_train_",season_tmp,".rda",sep=""))
  # save(sd_error_rep_train,file=paste("R/model/output/sd_train_",season_tmp,".rda",sep=""))

  # get preds and AUCs
  report_train <-  model_train$report()
  preds <- report_train$preds
  colnames(preds) <- sort(unique(TheD$stat_area)) 
  preds_choice <- as.numeric(colnames(preds)[apply(preds,1,which.max)])
  
  library(pROC)
  roc.multi <- multiclass.roc(response=as.factor(training_df$stat_area),predictor= preds)
  # rs <- roc.multi[['rocs']]
  # plot.roc(rs[[1]])
  # sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))
  round(auc(roc.multi),2)
  
  leave.season.out.tab[i,"train_auc"] = round(auc(roc.multi),2)
  
  crf_eval = crfsuite::crf_evaluation(pred = preds_choice, obs = training_df$stat_area, labels = unique(training_df$stat_area))
  leave.season.out.tab[i,"train_acc"] = crf_eval$overall["accuracy"]
  leave.season.out.tab[i,"train_prec"] = crf_eval$overall["precision"]
  
  #==== Testing 
  # Area Num
  Areas <- sort(unique(training_df$stat_area)) 
  Narea <- length(Areas)
  ActArea <- rep(0,length(testing_df$stat_area))
  for (II in 1:length(ActArea)) ActArea[II] <- which(testing_df$stat_area[II] == Areas)
  # starts arrays at 0
  ActArea <- ActArea - 1   
  
  # Vessel Num
  Vessels <- unique(training_df$vessel_num)
  Nvessel <- length(Vessels)
  ActVess <- rep(0,length(testing_df$vessel_num))
  for (II in 1:length(ActVess)) ActVess[II] <- which(testing_df$vessel_num[II] == Vessels)
  ActVess <- ActVess - 1  
  
  # Season Num
  Seasons <- unique(TheD$season)
  Nseason <- length(Seasons)
  ActSeason <- rep(0,length(testing_df$season))
  for (II in 1:length(ActSeason)) ActSeason[II] <- which(testing_df$season[II] == Seasons)
  ActSeason <- ActSeason - 1 
  
  
  # redefine matrices for covariates
  k=Narea
  col_start = 7
  Trad_C = testing_df[,col_start:(col_start+(k-1))]
  CpueF_C = testing_df[,(col_start+k):(col_start+2*k-1)] 
  CpueV_C = testing_df[,(col_start+2*k):(col_start+3*k-1)] 
  Cpuelag_C = testing_df[,(col_start+3*k):(col_start+4*k-1)] %>% replace(is.na(.), 0) # CPUE_lag set to null in map for now
  Sort_C = testing_df[,(col_start+4*k):(col_start+5*k-1)]
  Ice_C = testing_df[,(col_start+5*k):(col_start+6*k-1)] # this has natural NAs to remove
  Cong_C = testing_df[,(col_start+6*k):(col_start+7*k-1)]
  Port_C = testing_df[,(col_start+7*k):(col_start+8*k-1)] # this has natural NAs to remove
  Surv_C = testing_df[,(col_start+8*k):(col_start+9*k-1)]/1000
  CvF_C = testing_df[,(col_start+9*k):(col_start+10*k-1)]
  CvV_C = testing_df[,(col_start+10*k):(col_start+11*k-1)]
  Wind_C = testing_df[,(col_start+11*k):(col_start+12*k-1)]
  SurvOff_C = testing_df[,(col_start+12*k):(col_start+13*k-1)]
  model_full_fleet$env$last.par.best
  # Data structure
  data_test <- list(# Structure
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
    CpueF_Vess_rand_flag=0,CpueV_Vess_rand_flag=0,
    CvF_Vess_rand_flag=0,CvV_Vess_rand_flag=0,
    Wind_Vess_rand_flag=0,
    SurvOff_Vess_rand_flag=0,
    Surv_Vess_rand_flag=1,
    Cpuelag_Vess_rand_flag=0,
    # Vessel-Season
    Ice_Vess_Season_rand_flag=0)
  
  test1= matrix(report_train$Int_Area_Vessel_rand,nrow=Narea-1,ncol=Nvessel)
  test2 = report_train$Int_Area_Vessel_rand
  #== pull fitted parameters from training model
  pars_train = model_train$env$last.par.best
  Area_par_train = pars_train[which(names(pars_train) %in% c("AreaPar"))]
  Trad_Vess_rand_par_train =pars_train[which(names(pars_train) %in% c("Trad_Vess_rand_par"))]
  Surv_Vess_rand_par_train =pars_train[which(names(pars_train) %in% c("Surv_Vess_rand_par"))]
  Port_Vess_rand_par_train =pars_train[which(names(pars_train) %in% c("Port_Vess_rand_par"))]
  
  # parameter
  parameters_fleet <- list(# Fixed - Fleetwide
    AreaPar=report_train$AreaPar,CpueV_par=pars_train["CpueV_par"],CpueF_par=pars_train["CpueF_par"],Trad_par=pars_train["Trad_par"],
    Ice_par=pars_train["Ice_par"],Cong_par=pars_train["Cong_par"],Cpuelag_par=pars_train["Cpuelag_par"],Sort_par=pars_train["Sort_par"],Port_par=pars_train["Port_par"],Surv_par=0,CvF_par=pars_train["CvF_par"],CvV_par=pars_train["CvV_par"],
    Wind_par=pars_train["Wind_par"],SurvOff_par=0.0,
    # Random - Fleet 
    Int_Area_Season_rand=matrix(0,nrow=Narea-1,ncol=Nseason),logSigma_Area_Season_rand=-0.5,
    CpueV_Season_rand_par=rep(0.0,Nseason),logSigma_CpueV_Season_rand=0.0,
    CpueF_Season_rand_par=rep(0.0,Nseason),logSigma_CpueF_Season_rand=0.0,
    # Random - Unique Vessel 
    Int_Area_Vessel_rand=matrix(report_train$Int_Area_Vessel_rand,nrow=Narea-1,ncol=Nvessel),logSigma_Area_Vessel_rand=pars_train["logSigma_Area_Vessel_rand"],
    Ice_Vess_rand_par=rep(0.0,Nvessel),logSigma_Ice_Vessel_rand=0.0,
    Port_Vess_rand_par=Port_Vess_rand_par_train,logSigma_Port_Vessel_rand=pars_train["logSigma_Port_Vessel_rand"],
    Cong_Vess_rand_par=rep(0.0,Nvessel),logSigma_Cong_Vessel_rand=0.0,
    Trad_Vess_rand_par=Trad_Vess_rand_par_train,logSigma_Trad_Vessel_rand=pars_train["logSigma_Trad_Vessel_rand"],
    CpueF_Vess_rand_par=rep(0.0,Nvessel),logSigma_CpueF_Vessel_rand=0.0,
    CpueV_Vess_rand_par=rep(0.0,Nvessel),logSigma_CpueV_Vessel_rand=0.0,
    CvF_Vess_rand_par=rep(0.0,Nvessel),logSigma_CvF_Vessel_rand=0.0,
    CvV_Vess_rand_par=rep(0.0,Nvessel),logSigma_CvV_Vessel_rand=0.0,
    Wind_Vess_rand_par=rep(0.0,Nvessel),logSigma_Wind_Vessel_rand=0.0,
    SurvOff_Vess_rand_par=rep(0.0,Nvessel),logSigma_SurvOff_Vessel_rand=0.0,
    Surv_Vess_rand_par=Surv_Vess_rand_par_train,logSigma_Surv_Vessel_rand=pars_train["logSigma_Surv_Vessel_rand"],
    Cpuelag_Vess_rand_par=rep(0.0,Nvessel),logSigma_Cpuelag_Vessel_rand=0.0,
    
    # Random - Unique Vessel-Season
    Ice_Vess_Season_rand_par=matrix(0,nrow=Nvessel,ncol=Nseason),logSigma_Ice_Vessel_Season_rand=0.0)
  
  #=== MAP should ALL BE NA, except the variables we want
  map_fix = list(# Fixed
    # CpueV_par=factor(NA),
    # CpueF_par=factor(NA),Ice_par=factor(NA),Trad_par=factor(NA),
    # Cpuelag_par=factor(NA), Sort_par=factor(NA),Port_par=factor(NA),Cong_par=factor(NA),
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
    Int_Area_Vessel_rand=rep(factor(NA),length=((Narea-1)*Nvessel)),logSigma_Area_Vessel_rand=factor(NA),
    Ice_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Ice_Vessel_rand=factor(NA),
    Port_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Port_Vessel_rand=factor(NA),
    Cong_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Cong_Vessel_rand=factor(NA),
    Trad_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Trad_Vessel_rand=factor(NA),
    CpueF_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_CpueF_Vessel_rand=factor(NA),
    CpueV_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_CpueV_Vessel_rand=factor(NA),
    CvF_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_CvF_Vessel_rand=factor(NA),
    CvV_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_CvV_Vessel_rand=factor(NA),
    Wind_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Wind_Vessel_rand=factor(NA),
    SurvOff_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_SurvOff_Vessel_rand=factor(NA),
    Surv_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Surv_Vessel_rand=factor(NA),
    Cpuelag_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Cpuelag_Vessel_rand=factor(NA),
    
    # Random - Unique Vessel-Season
    Ice_Vess_Season_rand_par=rep(factor(NA),length=Nvessel*Nseason),logSigma_Ice_Vessel_Season_rand=factor(NA)
  )
  
  
  # testing the model
  model_full_fleet_test <- MakeADFun(data=data_test,parameters_fleet,map=map_fix,DLL="mnl_fixed_random",
                                     control=list(eval.max=10000,iter.max=1000,rel.tol=1e-15),silent=T,
                                     random=c(
                                       # Fleet
                                       # "Int_Area_Season_rand"
                                       # "CpueV_Season_rand_par",
                                       # "CpueF_Season_rand_par",
                                       
                                       # # Vessel
                                       "Int_Area_Vessel_rand",
                                       # "Ice_Vess_rand_par"
                                       "Port_Vess_rand_par",
                                       # "Cong_Vess_rand_par",
                                       "Trad_Vess_rand_par",
                                       # "CpueF_Vess_rand_par"
                                       # "CpueV_Vess_rand_par"
                                       # "CvF_Vess_rand_par",
                                       # "CvV_Vess_rand_par"
                                       # "Wind_Vess_rand_par"
                                       # "SurvOff_Vess_rand_par"
                                       "Surv_Vess_rand_par"
                                       # "Cpuelag_Vess_rand_par"
                                       
                                       # # # Vessel-Season
                                       # "Ice_Vess_Season_rand_par"
                                     ))
  

  
  fitted_test <- nlminb(model_full_fleet_test$par,model_full_fleet_test$fn,model_full_fleet_test$gr,control=list(eval.max=0,iter.max=0,rel.tol=1e-15))
  # report_test <-  model_full_fleet_test$report()
  
  # get preds and AUCs
  report_test <-  model_full_fleet_test$report()
  report_test_int_area = report_test$Int_Area_Vessel_rand
  pars_test = model_full_fleet_test$env$last.par.best
  
  preds <- report_test$preds
  colnames(preds) <- sort(unique(TheD$stat_area)) 
  preds_choice <- as.numeric(colnames(preds)[apply(preds,1,which.max)])
  
  library(pROC)
  # roc.multi <- multiclass.roc(testing_df$stat_area, preds_choice)
  roc.multi <- multiclass.roc(response=as.factor(testing_df$stat_area),predictor= preds)
  # rs <- roc.multi[['rocs']]
  # plot.roc(rs[[1]])
  # sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))
  # round(auc(roc.multi),2)
  
  leave.season.out.tab[i,"AUC"] = round(auc(roc.multi),2)
  sum(testing_df$stat_area == preds_choice)/140
  
  crf_eval = crfsuite::crf_evaluation(pred = preds_choice, obs = testing_df$stat_area, labels = unique(TheD$stat_area))
  leave.season.out.tab[i,"accuracy"] = crf_eval$overall["accuracy"]
  leave.season.out.tab[i,"precision"] = crf_eval$overall["precision"]
}



# write.csv(leave.season.out.tab, "R/model/output/season_cs.csv",row.names = F)
leave.season.out.tab = read.csv("R/model/output/season_cs.csv")

ggplot() + geom_line(data=leave.season.out.tab,aes(x=season,y=accuracy,color="accuracy")) +
  geom_line(data=leave.season.out.tab,aes(x=season,y=precision,color="precision")) +
  geom_hline(yintercept = 0.2929297,color="#F8766D")+ 
  geom_hline(yintercept = 0.3035960,color="#00BFC4") +
  guides(color=guide_legend(title="Performance metric")) + 
  labs(y="Score",x="Season") + 
  theme_bw()

library(scales)
show_col(hue_pal()(4))

ggplot() + geom_point(data=leave.season.out.tab,aes(x=season,y=precision,color="Tested")) +
  geom_point(data=leave.season.out.tab,aes(x=season,y=train_prec,color="Training")) + theme_bw()

ggplot() + geom_point(data=leave.season.out.tab,aes(x=season,y=AUC,color="Tested")) +
  geom_point(data=leave.season.out.tab,aes(x=season,y=train_auc,color="Training")) + theme_bw()


geom_hline(yintercept = 0.2929297)+ 
  geom_hline(yintercept = 0.3035960) 
