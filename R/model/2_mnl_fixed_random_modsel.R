#===== RANDOM EFFECT Selection Process
# Make this data frame to know what flag corresponds to map
flag_map_df=data.frame(
  flag=c("CpueV_Season_rand_flag",
         "CpueF_Season_rand_flag","Int_AreaSeason_rand_flag",
         # Unique Vessel
         "Int_AreaVess_rand_flag","Ice_Vess_rand_flag","Port_Vess_rand_flag",
         "Cong_Vess_rand_flag","Trad_Vess_rand_flag","CpueF_Vess_rand_flag","CpueV_Vess_rand_flag",
         "CvF_Vess_rand_flag","CvV_Vess_rand_flag",
         "Wind_Vess_rand_flag","SurvOff_Vess_rand_flag","Surv_Vess_rand_flag","Cpuelag_Vess_rand_flag",
         # Vessel-Season
         "Ice_Vess_Season_rand_flag"
         ),
  map=c(# Random - Fleet 
    "CpueV_Season_rand_par=rep(factor(NA),Nseason),logSigma_CpueV_Season_rand=factor(NA)",
    "CpueF_Season_rand_par=rep(factor(NA),Nseason),logSigma_CpueF_Season_rand=factor(NA)",
    "Int_Area_Season_rand=rep(factor(NA),length=(Narea-1)*Nseason),logSigma_Area_Season_rand=factor(NA)",
    # Random - Unique Vessel 
    "Int_Area_Vessel_rand=rep(factor(NA),length=((Narea-1)*Nvessel)),logSigma_Area_Vessel_rand=factor(NA)",
    "Ice_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Ice_Vessel_rand=factor(NA)",
    "Port_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Port_Vessel_rand=factor(NA)",
    "Cong_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Cong_Vessel_rand=factor(NA)",
    "Trad_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Trad_Vessel_rand=factor(NA)",
    "CpueF_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_CpueF_Vessel_rand=factor(NA)",
    "CpueV_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_CpueV_Vessel_rand=factor(NA)",
    "CvF_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_CvF_Vessel_rand=factor(NA)",
    "CvV_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_CvV_Vessel_rand=factor(NA)",
    "Wind_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Wind_Vessel_rand=factor(NA)",
    "SurvOff_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_SurvOff_Vessel_rand=factor(NA)",
    "Surv_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Surv_Vessel_rand=factor(NA)",
    "Cpuelag_Vess_rand_par=rep(factor(NA),Nvessel),logSigma_Cpuelag_Vessel_rand=factor(NA)",
    
    # Random - Unique Vessel-Season
    "Ice_Vess_Season_rand_par=rep(factor(NA),length=Nvessel*Nseason),logSigma_Ice_Vessel_Season_rand=factor(NA)"
    ),
  
    random=c(
      # Fleet
    "CpueV_Season_rand_par","CpueF_Season_rand_par",
    "Int_Area_Season_rand",
    # Vessel
    "Int_Area_Vessel_rand","Ice_Vess_rand_par","Port_Vess_rand_par",
    "Cong_Vess_rand_par","Trad_Vess_rand_par",
    "CpueF_Vess_rand_par","CpueV_Vess_rand_par",
    "CvF_Vess_rand_par","CvV_Vess_rand_par",
    "Wind_Vess_rand_par","SurvOff_Vess_rand_par",
    "Surv_Vess_rand_par","Cpuelag_Vess_rand_par",
    # Vessel-Season
    "Ice_Vess_Season_rand_par"
  ))

# define model terms
mod.terms <- c(
  # Unique Vessel
  "Int_AreaVess_rand_flag",
  "Trad_Vess_rand_flag",
  "CpueF_Vess_rand_flag",
  "Surv_Vess_rand_flag",
  "CvV_Vess_rand_flag",
  "Cong_Vess_rand_flag",
  "CvF_Vess_rand_flag",
  "Ice_Vess_rand_flag",
  "CpueV_Vess_rand_flag",
  "Wind_Vess_rand_flag","SurvOff_Vess_rand_flag",
"Cpuelag_Vess_rand_flag",
"Port_Vess_rand_flag",
  
# Fleet Wide
"CpueV_Season_rand_flag",
"CpueF_Season_rand_flag","Int_AreaSeason_rand_flag",
# Vessel-Season
"Ice_Vess_Season_rand_flag"
)

# Data structure
fixed_dat = "list(# Structure
    Narea=Narea,ActArea=ActArea,ActVess=ActVess,Nvessel=Nvessel,ActSeason=ActSeason,Nseason=Nseason,
    # Data 
    CpueV_C=as.matrix(CpueV_C),CpueF_C=as.matrix(CpueF_C),Trad_C=as.matrix(Trad_C),
    Ice_C=as.matrix(Ice_C),Cpuelag_C=as.matrix(Cpuelag_C),Sort_C=as.matrix(Sort_C),
    Cong_C=as.matrix(Cong_C),Port_C=as.matrix(Port_C),Surv_C=as.matrix(Surv_C),
    CvF_C=as.matrix(CvF_C),CvV_C=as.matrix(CvV_C),Wind_C=as.matrix(Wind_C),SurvOff_C=as.matrix(SurvOff_C),"

# Create a table of indicators as to whether that predictor is include
mod.ind <- matrix(F, ncol = length(mod.terms), nrow = 1)
mod.tab <- data.frame(mod.ind)
names(mod.tab) <- mod.terms

# Define a column for storing model selection
mod.tab$AICc <- mod.tab$mean_AUC <- mod.tab$tlike <- mod.tab$accuracy<-mod.tab$precision <- NA
# model.forms <- matrix(ncol=1,nrow=nrow(mod.tab))
k_counter <- 1 # which AICc to compare to previous
# Loop through all combinations of Random Effects
# for(i in 2:length(mod.terms)){
for(i in 17:17){
  print(i)
  model_i <- i
  ind_on=as.matrix(mod.tab[i,1:length(mod.terms)])
  ind_off=as.matrix(!mod.tab[i,1:length(mod.terms)])

  # ind_off <- !mod.ind[model_i, ]
  # ind_on <- mod.ind[model_i, ]
  
  # Set which data flags should be turned on
  if (length(mod.terms[ind_on])>0){
    flags_1=paste(c(mod.terms[ind_on]),"=1",sep="",collapse=",")
  }else{flags_1=NULL}
  
  if (length(mod.terms[ind_off])>0){
    flags_0=paste(c(mod.terms[ind_off]),"=0",sep="",collapse=",")
  }else{flags_0=NULL}
  
  dat.form=paste(flags_1,flags_0,sep=",")
  # removes leading or trailing commas 
  dat.form=gsub('^\\,|\\,$', '', dat.form)

  data = eval(parse(text = paste(fixed_dat,dat.form,")",sep="")))
  
  # Define map based on flags that are OFF
  map.form=paste(flag_map_df$map[flag_map_df$flag %in% mod.terms[ind_off]],sep="",collapse=",")
  # model.forms[i,1] <- mod.form
  
  # Set map to include certain parameters
  # if else statement to set map=NULL when all params should be included
  if(all(ind_off == TRUE)){
    map = eval(parse(text = paste("list(",map.form,")",sep="")))
  } else {
    map = eval(parse(text = paste("list(",map.form,")",sep="")))
  }
  
  # Define random structure
 rand.form=paste(flag_map_df$random[flag_map_df$flag %in% mod.terms[ind_on]],sep="")
  
  # fit the model
  model_full_fleet <- MakeADFun(data=data,parameters_fleet,map=map,DLL="mnl_fixed_random",
                                control=list(eval.max=10000,iter.max=1000,rel.tol=1e-15),silent=T,
                                random=c(rand.form))
  fit <- nlminb(model_full_fleet$par,model_full_fleet$fn,model_full_fleet$gr)
  
  # hone in 
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
  
  
  # get AICc
  tlike= model_full_fleet$fn(fit$par)
  AICc=2*length(model_full_fleet$par)+2*tlike + 
    (2*length(model_full_fleet$par)^2+2*length(model_full_fleet$par))/(nrow(CpueV_C)-length(model_full_fleet$par)-1)
  mod.tab$tlike[i] = tlike
  mod.tab$AICc[i] = AICc
  
  # get preds and AUCs
  report <-  model_full_fleet$report()
  preds <- report$preds
  colnames(preds) <- sort(unique(TheD$stat_area)) 
  preds_choice <- as.numeric(colnames(preds)[apply(preds,1,which.max)])
  
  library(pROC)
  # roc.multi <- multiclass.roc(TheD$stat_area, preds_choice)
  roc.multi <- multiclass.roc(response=as.factor(TheD$stat_area),predictor= preds)
  # rs <- roc.multi[['rocs']]
  # plot.roc(rs[[1]])
  # sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))
  
  round(auc(roc.multi),2)
  mod.tab$mean_AUC[i] <- round(auc(roc.multi),2)
  
  crf_eval = crfsuite::crf_evaluation(pred = preds_choice, obs = TheD$stat_area, labels = unique(TheD$stat_area))
  mod.tab$accuracy[i] = crf_eval$overall["accuracy"]
  mod.tab$precision[i] = crf_eval$overall["precision"]
  
  # add new row and reset the tlike, AUC, and AICc
  mod.tab[i+1,] = mod.tab[i,]
  mod.tab[i+1,]$AICc <- mod.tab[i+1,]$mean_AUC <- mod.tab[i+1,]$tlike <- NA
  mod.tab$precision[i+1] <- mod.tab$precision[i+1]<- NA
  # turn the next random effect on for next model
  mod.tab[i+1,i] = T
  
  saveRDS(mod.tab,file="R/model/output/mod_tab_forward_stepwise_regression_2024_0227.rds")
  
  if (i>1){ # don't perform below to populate the 2nd row and 1st random effect
  # if model is better than previous version, keep the current random effect on for next model
  if ((mod.tab$AICc[i]-mod.tab$AICc[i-k_counter])< -10){
    mod.tab[i+1,i-1] = T # redundant but just set up the code
    k_counter = 1
  } else { # if not better
    mod.tab[i+1,i-1] = F # turn off current random effect for next model
    k_counter=k_counter+1
  }
  }
}
# i=7
mod.tab[i+1,i-1] = F
k_counter=2

mod.tab[16,c(14,15,17)] = F
mod.tab[16,c(16)] = T


# saveRDS(mod.tab,file="R/model/output/mod_tab_forward_stepwise_regression_2023_1025_backup.rds")
mod.tab <- readRDS("R/model/output/pars_best_model_sel_22.97vess.rand.tune.rda")
mod.tab.trad <- readRDS("R/model/output/mod_tab_forward_stepwise_regression_trad.first_2023_1009.rds")

# trad <- readRDS("R/model/output/mod_tab_forward_stepwise_regression_trad.first.rds")
# write.csv(mod.tab,"R/model/output/random_effects_mod_tab_rand.tune.csv", row.names = F)
# sd_error_rep <- sdreport(model_full_fleet)
mod.tab.clean = mod.tab[1:15,-c(14,15,17)]
mod.tab.clean.AICc = mod.tab.clean %>% mutate(delta_AICc = AICc-min(AICc)) 
# write.csv(mod.tab,"R/model/output/mod_tab_forward_stepwise_regression_2023_0809.csv",row.names = F)
write.csv(mod.tab.clean.AICc,"R/model/output/mod_tab_forward_stepwise_regression_2023_1009.csv",row.names = F)

#======= FIXED EFFECTS selection process
# Choose the random effects model you want to proceed with
mod.tab = mod.tab[,1:17]
i= 14
ind_on=as.matrix(mod.tab[i,1:length(mod.terms)])
ind_off=as.matrix(!mod.tab[i,1:length(mod.terms)])

# Get the data form first - will not change with fixed effects
if (length(mod.terms[ind_on])>0){
  flags_1=paste(c(mod.terms[ind_on]),"=1",sep="",collapse=",")
}else{flags_1=NULL}

if (length(mod.terms[ind_off])>0){
  flags_0=paste(c(mod.terms[ind_off]),"=0",sep="",collapse=",")
}else{flags_0=NULL}

dat.form=paste(flags_1,flags_0,sep=",")
# removes leading or trailing commas 
dat.form=gsub('^\\,|\\,$', '', dat.form)
data = eval(parse(text = paste(fixed_dat,dat.form,")",sep="")))

# Get RANDOM EFFECT map form next
map.form=paste(flag_map_df$map[flag_map_df$flag %in% mod.terms[ind_off]],sep="",collapse=",")

# Get random structure
rand.form=paste(flag_map_df$random[flag_map_df$flag %in% mod.terms[ind_on]],sep="")

# set up fixed effects table
# define model terms
fixed.mod.terms <- c(
  # Unique Vessel
  "Port_par",
  "Cong_par",
  "Surv_par",
  "Sort_par",
  "Trad_par",
  "CvV_par",
  "CvF_par",
  "Ice_par",
  "CpueF_par","CpueV_par",
  "Wind_par","SurvOff_par","Cpuelag_par")

# Create a table of indicators as to whether that predictor is include
fixed.mod.ind <- matrix(F, ncol = length(fixed.mod.terms), nrow = 1)
fixed.mod.tab <- data.frame(fixed.mod.ind)
names(fixed.mod.tab) <- fixed.mod.terms

# Define a column for storing model selection
fixed.mod.tab$AICc <- fixed.mod.tab$mean_AUC <- fixed.mod.tab$tlike <- fixed.mod.tab$accuracy<-fixed.mod.tab$precision <- NA
fixed.model.forms <- matrix(ncol=1,nrow=nrow(fixed.mod.tab))

k_counter <- 1 # which AICc to compare to previous
# Loop through all combinations of Random Effects
for(fixed_i in 11:(length(fixed.mod.terms)+1)){
  print(fixed_i)
  fixed_ind_on=as.matrix(fixed.mod.tab[fixed_i,1:length(fixed.mod.terms)])
  fixed_off=as.matrix(!fixed.mod.tab[fixed_i,1:length(fixed.mod.terms)])
  
  # Define map based on flags that are OFF
  fixed.map.form=paste(fixed.mod.terms[fixed_off],"=factor(NA)",sep="",collapse=",")
  # model.forms[i,1] <- mod.form
  
  # Set map to include certain parameters
  # if else statement to set map=NULL when all params should be included
  if(all(fixed_off == FALSE)){ # yes this ind_off refers to the random effects indicators off
    fixed.map=eval(parse(text = paste("list(",map.form,")",sep="")))
  } else {
    fixed.map = eval(parse(text = paste("list(",fixed.map.form,",",map.form,")",sep="")))
  }
  # fit the model
  model_full_fleet <- MakeADFun(data=data,parameters_fleet,map=fixed.map,DLL="mnl_fixed_random",
                                control=list(eval.max=10000,iter.max=1000,rel.tol=1e-15),silent=T,
                                random=c(rand.form))
  fit <- nlminb(model_full_fleet$par,model_full_fleet$fn,model_full_fleet$gr)
  
  # hone in 
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
  
  
  # get AICc
  tlike= model_full_fleet$fn(fit$par)
  AICc=2*length(model_full_fleet$par)+2*tlike + 
    (2*length(model_full_fleet$par)^2+2*length(model_full_fleet$par))/(nrow(CpueV_C)-length(model_full_fleet$par)-1)
  fixed.mod.tab$tlike[fixed_i] = tlike
  fixed.mod.tab$AICc[fixed_i] = AICc
  
  # get preds and AUCs
  report <-  model_full_fleet$report()
  preds <- report$preds
  colnames(preds) <- sort(unique(TheD$stat_area)) 
  preds_choice <- as.numeric(colnames(preds)[apply(preds,1,which.max)])
  
  library(pROC)
  roc.multi <- multiclass.roc(TheD$stat_area, preds_choice)
  rs <- roc.multi[['rocs']]
  plot.roc(rs[[1]])
  sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))
  
  round(auc(roc.multi),2)
  fixed.mod.tab$mean_AUC[fixed_i] <- round(auc(roc.multi),2)
  
  crf_eval = crfsuite::crf_evaluation(pred = preds_choice, obs = TheD$stat_area, labels = unique(TheD$stat_area))
  fixed.mod.tab$accuracy[fixed_i] = crf_eval$overall["accuracy"]
  fixed.mod.tab$precision[fixed_i] = crf_eval$overall["precision"]
  
  
  # add new row and reset the tlike, AUC, and AICc
  fixed.mod.tab[fixed_i+1,] = fixed.mod.tab[fixed_i,]
  fixed.mod.tab[fixed_i+1,]$AICc <- fixed.mod.tab[fixed_i+1,]$mean_AUC <- fixed.mod.tab[fixed_i+1,]$tlike <- NA
  fixed.mod.tab$precision[fixed_i+1] <- fixed.mod.tab$precision[fixed_i+1]<- NA
  # turn the next random effect on for next model
  fixed.mod.tab[fixed_i+1,fixed_i] = T

  if (fixed_i>1){ # don't perform below to populate the 2nd row and 1st random effect
    # if model is better than previous version, keep the current random effect on for next model
    if ((fixed.mod.tab$AICc[fixed_i]-fixed.mod.tab$AICc[fixed_i-k_counter])< -4){
      fixed.mod.tab[fixed_i+1,fixed_i-1] = T # redundant but just set up the code
      k_counter = 1
    } else { # if not better
      fixed.mod.tab[fixed_i+1,fixed_i-1] = F # turn off current random effect for next model
      k_counter=k_counter+1
    }
  }
  # saveRDS(fixed.mod.tab,file="R/model/output/fixed.map.form_2023_1107.6RE.rds")
}

# saveRDS(fixed.mod.tab,file="R/model/output/fixed.map.form_2023_1014.trad.first.rds")
fixed.mod.tab=readRDS("R/model/output/fixed.map.form_2023_1014.trad.first.rds")
fixed.mod.tab=readRDS("R/model/output/fixed.map.form_2023_1107.6RE.rds")
fixed.mod.tab=fixed.mod.tab[1:14,]
fixed.mod.tab = fixed.mod.tab %>% mutate(delta_AICc = AICc-min(AICc)) %>% arrange(delta_AICc)
write.csv(fixed.mod.tab,"R/model/output/fixed.map.form_2023_1107.csv",row.names = F) 

# ====== Cross validation
# split the data
# pred.err <- rep(NA,4)
# ok = TheD %>% group_by(vessel,stat_area) %>% mutate(group_id = cur_group_id()) %>% select(X,vessel,stat_area,group_id) %>%
#   arrange(group_id)
# 
# set.seed(rnorm(1, mean=12345, sd=12345))
# cross.fold.tab$MAE <- NA

library(tidyverse)
library(splitstackshape)
library(caret)
library(randomForest)

## split data into train and test using stratified sampling
# This NEEDS WORK
training_prop = 0.5
testing_prop = 1- training_prop

# sample up function
sample_up <- function(.data, frac) {
  sample_n(.data, ceiling({{frac}} * n()) )
}

training_df = TheD %>% group_by(stat_area) %>% sample_up(sqrt(training_prop)) %>% group_by(vessel) %>% sample_up(sqrt(training_prop)) 
testing_df = TheD %>% group_by(stat_area) %>% sample_up(sqrt(testing_prop)) %>% group_by(vessel) %>% sample_up(sqrt(testing_prop)) 

length(unique(TheD$vessel))
length(unique(training_df$vessel))
length(unique(testing_df$vessel))

length(unique(TheD$stat_area))
length(unique(training_df$stat_area))
length(unique(testing_df$stat_area))

# define matrices for covariates
k=Narea
col_start = 7
Trad_C = training_df[,col_start:(col_start+(k-1))]
CpueF_C = training_df[,(col_start+k):(col_start+2*k-1)] 
CpueV_C = training_df[,(col_start+2*k):(col_start+3*k-1)] 
Cpuelag_C = training_df[,(col_start+3*k):(col_start+4*k-1)] %>% replace(is.na(.), 0) # CPUE_lag set to null in map for now
Sort_C = training_df[,(col_start+4*k):(col_start+5*k-1)]
Ice_C = training_df[,(col_start+5*k):(col_start+6*k-1)] # this has natural NAs to remove
Cong_C = training_df[,(col_start+6*k):(col_start+7*k-1)]
Port_C = training_df[,(col_start+7*k):(col_start+8*k-1)] # this has natural NAs to remove
Surv_C = training_df[,(col_start+8*k):(col_start+9*k-1)]/1000
CvF_C = training_df[,(col_start+9*k):(col_start+10*k-1)]
CvV_C = training_df[,(col_start+10*k):(col_start+11*k-1)]
Wind_C = training_df[,(col_start+11*k):(col_start+12*k-1)]
SurvOff_C = training_df[,(col_start+12*k):(col_start+13*k-1)]

#=== Model to test on training data

# Random Effects part
mod.tab <- readRDS("R/model/output/mod_tab_forward_stepwise_regression_2023_0811.rds")
# Choose the random effects model you want to proceed with
i= 5
ind_on=as.matrix(mod.tab[i,1:length(mod.terms)])
ind_off=as.matrix(!mod.tab[i,1:length(mod.terms)])

# Get the data form first - will not change with fixed effects
if (length(mod.terms[ind_on])>0){
  flags_1=paste(c(mod.terms[ind_on]),"=1",sep="",collapse=",")
}else{flags_1=NULL}

if (length(mod.terms[ind_off])>0){
  flags_0=paste(c(mod.terms[ind_off]),"=0",sep="",collapse=",")
}else{flags_0=NULL}

dat.form=paste(flags_1,flags_0,sep=",")
# removes leading or trailing commas 
dat.form=gsub('^\\,|\\,$', '', dat.form)
data = eval(parse(text = paste(fixed_dat,dat.form,")",sep="")))

# Get RANDOM EFFECT map form next
map.form=paste(flag_map_df$map[flag_map_df$flag %in% mod.terms[ind_off]],sep="",collapse=",")

# Get random structure
rand.form=paste(flag_map_df$random[flag_map_df$flag %in% mod.terms[ind_on]],sep="")


# Fixed Effects part
fixed.mod.tab=readRDS("R/model/output/fixed.map.form_2023_0809.rds")
fixed_i=14
fixed_ind_on=as.matrix(fixed.mod.tab[fixed_i,1:length(fixed.mod.terms)])
fixed_off=as.matrix(!fixed.mod.tab[fixed_i,1:length(fixed.mod.terms)])

# Define map based on flags that are OFF
fixed.map.form=paste(fixed.mod.terms[fixed_off],"=factor(NA)",sep="",collapse=",")
# model.forms[i,1] <- mod.form

# Set map to include certain parameters
# if else statement to set map=NULL when all params should be included
if(all(fixed_off == FALSE)){ # yes this ind_off refers to the random effects indicators off
  fixed.map=eval(parse(text = paste("list(",map.form,")",sep="")))
} else {
  fixed.map = eval(parse(text = paste("list(",fixed.map.form,",",map.form,")",sep="")))
}


# training the model
model_full_fleet_train <- MakeADFun(data=data,parameters_fleet,map=fixed.map,DLL="mnl_fixed_random",
                              control=list(eval.max=10000,iter.max=1000,rel.tol=1e-15),silent=T,
                              random=c(rand.form))
fit_train <- nlminb(model_full_fleet_train$par,model_full_fleet_train$fn,model_full_fleet_train$gr)


# set up testing 
# define matrices for covariates
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

# redefine 
test_data = eval(parse(text = paste(fixed_dat,dat.form,")",sep="")))

#=== MAP should ALL BE NA, we are not estimating ANYTHING
map_fix = list(# Fixed
  CpueV_par=factor(NA),
  CpueF_par=factor(NA),Ice_par=factor(NA),Trad_par=factor(NA),
  Cpuelag_par=factor(NA), Sort_par=factor(NA),Port_par=factor(NA),Cong_par=factor(NA),
  Surv_par=factor(NA),
  CvF_par=factor(NA),
  CvV_par=factor(NA),
  Wind_par=factor(NA),
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
model_full_fleet_test <- MakeADFun(data=test_data,parameters_fleet,map=map_fix,DLL="mnl_fixed_random",
                                    control=list(eval.max=10000,iter.max=1000,rel.tol=1e-15),silent=T,
                                    random=c(rand.form))


model_full_fleet_train$fn(fit_train$par)


#==== Data form 
# only influenced by random effects (need to turn on the relevant random effects)
i= 5
ind_on=as.matrix(mod.tab[i,1:length(mod.terms)])
ind_off=as.matrix(!mod.tab[i,1:length(mod.terms)])

# Get the data form first - will not change with fixed effects
if (length(mod.terms[ind_on])>0){
  flags_1=paste(c(mod.terms[ind_on]),"=1",sep="",collapse=",")
}else{flags_1=NULL}

if (length(mod.terms[ind_off])>0){
  flags_0=paste(c(mod.terms[ind_off]),"=0",sep="",collapse=",")
}else{flags_0=NULL}

dat.form=paste(flags_1,flags_0,sep=",")
# removes leading or trailing commas 
dat.form=gsub('^\\,|\\,$', '', dat.form)
data = eval(parse(text = paste(fixed_dat,dat.form,")",sep="")))

# Get RANDOM EFFECT map form next
map.form=paste(flag_map_df$map[flag_map_df$flag %in% mod.terms[ind_off]],sep="",collapse=",")

# Get random structure
rand.form=paste(flag_map_df$random[flag_map_df$flag %in% mod.terms[ind_on]],sep="")

#==== Parameter form
# fill in each parameter with values from selected fixed effect model (which already has best RE structure)
pars = model_full_fleet$env$last.par.best
# see which random structures are necessary, just manually do this
mod.terms[ind_on] 
Int_Area_Vessel_rand=pars[which(names(pars) %in% c("Int_Area_Vessel_rand"))]
logSigma_Area_Vessel_rand=pars[which(names(pars) %in% c("logSigma_Area_Vessel_rand"))]

Port_Vess_rand_par=pars[which(names(pars) %in% c("Port_Vess_rand_par"))]
logSigma_Port_Vessel_rand=pars[which(names(pars) %in% c("logSigma_Port_Vessel_rand"))]

Cong_Vess_rand_par=pars[which(names(pars) %in% c("Cong_Vess_rand_par"))]
logSigma_Cong_Vessel_rand=pars[which(names(pars) %in% c("logSigma_Cong_Vessel_rand"))]

Trad_Vess_rand_par=pars[which(names(pars) %in% c("Trad_Vess_rand_par"))]
logSigma_Trad_Vessel_rand=pars[which(names(pars) %in% c("logSigma_Trad_Vessel_rand"))]

# see which fixed effects structures are necessary, just manually do this
fixed.mod.terms[fixed_ind_on]

AreaPar=pars[which(names(pars) %in% c("AreaPar"))]
CpueV_par=pars[which(names(pars) %in% c("CpueV_par"))]
CpueF_par=pars[which(names(pars) %in% c("CpueF_par"))]
Trad_par=pars[which(names(pars) %in% c("Trad_par"))]
Ice_par=pars[which(names(pars) %in% c("Ice_par"))]
Cong_par=pars[which(names(pars) %in% c("Cong_par"))]
Cpuelag_par=pars[which(names(pars) %in% c("Cpuelag_par"))]
Sort_par=pars[which(names(pars) %in% c("Sort_par"))]
Port_par=pars[which(names(pars) %in% c("Port_par"))]
Surv_par=pars[which(names(pars) %in% c("Surv_par"))]
CvF_par=pars[which(names(pars) %in% c("CvF_par"))]
CvV_par=pars[which(names(pars) %in% c("CvV_par"))]
Wind_par=pars[which(names(pars) %in% c("Wind_par"))]
SurvOff_par=pars[which(names(pars) %in% c("SurvOff_par"))]

parameters_fleet <- list(# Fixed - Fleetwide
  AreaPar=AreaPar,CpueV_par=CpueV_par,CpueF_par=CpueF_par,Trad_par=Trad_par,
  Ice_par=Ice_par,Cong_par=Cong_par,Cpuelag_par=Cpuelag_par,Sort_par=Sort_par,Port_par=Port_par,Surv_par=0.0,CvF_par=CvF_par,CvV_par=CvV_par,
  Wind_par=Wind_par,SurvOff_par=0.0,
  # Random - Fleet 
  Int_Area_Season_rand=matrix(0,nrow=Narea-1,ncol=Nseason),logSigma_Area_Season_rand=-0.5,
  CpueV_Season_rand_par=rep(0.0,Nseason),logSigma_CpueV_Season_rand=0.0,
  CpueF_Season_rand_par=rep(0.0,Nseason),logSigma_CpueF_Season_rand=0.0,
  # Random - Unique Vessel 
  Int_Area_Vessel_rand=as.matrix(report$Int_Area_Vessel_rand),logSigma_Area_Vessel_rand=logSigma_Area_Vessel_rand,
  Ice_Vess_rand_par=rep(0.0,Nvessel),logSigma_Ice_Vessel_rand=0.0,
  Port_Vess_rand_par=Port_Vess_rand_par,logSigma_Port_Vessel_rand=logSigma_Port_Vessel_rand,
  Cong_Vess_rand_par=rep(0.0,Nvessel),logSigma_Cong_Vessel_rand=0.0,
  Trad_Vess_rand_par=Trad_Vess_rand_par,logSigma_Trad_Vessel_rand=logSigma_Trad_Vessel_rand,
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


#==== MAP should ALL BE NA, we are not estimating ANYTHING
map_full_fleet=list(# Fixed
  CpueV_par=factor(NA),
  CpueF_par=factor(NA),Ice_par=factor(NA),Trad_par=factor(NA),
  Cpuelag_par=factor(NA), Sort_par=factor(NA),Port_par=factor(NA),Cong_par=factor(NA),
  Surv_par=factor(NA),
  CvF_par=factor(NA),
  CvV_par=factor(NA),
  Wind_par=factor(NA),
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

model_full_fleet_kfold <- MakeADFun(data=data,parameters_fleet,map=map_full_fleet,DLL="mnl_fixed_random",
                              control=list(eval.max=10000,iter.max=1000,rel.tol=1e-15),silent=T,
                              random=c(rand.form))

fit <- nlminb(model_full_fleet_kfold$par,model_full_fleet$fn,model_full_fleet$gr)
