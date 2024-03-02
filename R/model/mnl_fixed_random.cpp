#include <TMB.hpp>


template<class Type>
Type objective_function<Type>::operator() ()
{
  // Data passed in
  DATA_INTEGER(Narea);
  DATA_IVECTOR(ActArea);

  DATA_INTEGER(Nvessel); 
  DATA_IVECTOR(ActVess); 

  DATA_INTEGER(Nseason); 
  DATA_IVECTOR(ActSeason); 

  DATA_MATRIX(CpueV_C);
  DATA_MATRIX(CpueF_C);
  DATA_MATRIX(Ice_C);
  DATA_MATRIX(Trad_C);
  DATA_MATRIX(Cpuelag_C);
  DATA_MATRIX(Sort_C);
  DATA_MATRIX(Cong_C);
  DATA_MATRIX(Port_C);
  DATA_MATRIX(Surv_C);
  DATA_MATRIX(CvF_C);
  DATA_MATRIX(CvV_C);
  DATA_MATRIX(Wind_C);
  DATA_MATRIX(SurvOff_C);

  // RE Lookup Tables
  // DATA_IMATRIX(Int_AreaVess_tbl);
  
  // Fixed Effects
  PARAMETER_VECTOR(AreaPar); //necessary for null model
  //PARAMETER_VECTOR(VesselPar); //necessary? no
  PARAMETER(CpueV_par);
  PARAMETER(CpueF_par);
  PARAMETER(Ice_par);
  PARAMETER(Trad_par);
  PARAMETER(Cpuelag_par);
  PARAMETER(Sort_par);
  PARAMETER(Cong_par);
  PARAMETER(Port_par);
  PARAMETER(Surv_par);
  PARAMETER(CvF_par);
  PARAMETER(CvV_par);
  PARAMETER(Wind_par);
  PARAMETER(SurvOff_par);
  
  // Random Effects
    // Fleet-wide 
      // Area
        DATA_INTEGER(Int_AreaSeason_rand_flag);
        PARAMETER_MATRIX(Int_Area_Season_rand);
        PARAMETER(logSigma_Area_Season_rand); 
        Type Sigma_Area_Season_rand = exp(logSigma_Area_Season_rand);

      // CPUE V & F
        DATA_INTEGER(CpueV_Season_rand_flag);
        PARAMETER_VECTOR(CpueV_Season_rand_par);
        PARAMETER(logSigma_CpueV_Season_rand); 
        Type Sigma_CpueV_Season_rand = exp(logSigma_CpueV_Season_rand);

        DATA_INTEGER(CpueF_Season_rand_flag);
        PARAMETER_VECTOR(CpueF_Season_rand_par);
        PARAMETER(logSigma_CpueF_Season_rand); 
        Type Sigma_CpueF_Season_rand = exp(logSigma_CpueF_Season_rand);

    // Vessel-level
      // Area
        DATA_INTEGER(Int_AreaVess_rand_flag);
        PARAMETER_MATRIX(Int_Area_Vessel_rand); 
        //PARAMETER_VECTOR(Int_Area_Vessel_rand); // old lookup table code
        PARAMETER(logSigma_Area_Vessel_rand); 
        Type Sigma_Area_Vessel_rand = exp(logSigma_Area_Vessel_rand);

      // Ice
        DATA_INTEGER(Ice_Vess_rand_flag);
        PARAMETER_VECTOR(Ice_Vess_rand_par);
        PARAMETER(logSigma_Ice_Vessel_rand); 
        Type Sigma_Ice_Vessel_rand = exp(logSigma_Ice_Vessel_rand);

        DATA_INTEGER(Ice_Vess_Season_rand_flag);
        PARAMETER_MATRIX(Ice_Vess_Season_rand_par);
        PARAMETER(logSigma_Ice_Vessel_Season_rand); 
        Type Sigma_Ice_Vessel_Season_rand = exp(logSigma_Ice_Vessel_Season_rand);

      // Port
        DATA_INTEGER(Port_Vess_rand_flag);
        PARAMETER_VECTOR(Port_Vess_rand_par);
        PARAMETER(logSigma_Port_Vessel_rand); 
        Type Sigma_Port_Vessel_rand = exp(logSigma_Port_Vessel_rand);

      // Cong
        DATA_INTEGER(Cong_Vess_rand_flag);
        PARAMETER_VECTOR(Cong_Vess_rand_par);
        PARAMETER(logSigma_Cong_Vessel_rand); 
        Type Sigma_Cong_Vessel_rand = exp(logSigma_Cong_Vessel_rand);

      // Trad
        DATA_INTEGER(Trad_Vess_rand_flag);
        PARAMETER_VECTOR(Trad_Vess_rand_par);
        PARAMETER(logSigma_Trad_Vessel_rand); 
        Type Sigma_Trad_Vessel_rand = exp(logSigma_Trad_Vessel_rand);

      // CpueF
        DATA_INTEGER(CpueF_Vess_rand_flag);
        PARAMETER_VECTOR(CpueF_Vess_rand_par);
        PARAMETER(logSigma_CpueF_Vessel_rand); 
        Type Sigma_CpueF_Vessel_rand = exp(logSigma_CpueF_Vessel_rand);
  
      // CpueV
        DATA_INTEGER(CpueV_Vess_rand_flag);
        PARAMETER_VECTOR(CpueV_Vess_rand_par);
        PARAMETER(logSigma_CpueV_Vessel_rand); 
        Type Sigma_CpueV_Vessel_rand = exp(logSigma_CpueV_Vessel_rand);

      // CvF
        DATA_INTEGER(CvF_Vess_rand_flag);
        PARAMETER_VECTOR(CvF_Vess_rand_par);
        PARAMETER(logSigma_CvF_Vessel_rand); 
        Type Sigma_CvF_Vessel_rand = exp(logSigma_CvF_Vessel_rand);

      // CvV
        DATA_INTEGER(CvV_Vess_rand_flag);
        PARAMETER_VECTOR(CvV_Vess_rand_par);
        PARAMETER(logSigma_CvV_Vessel_rand); 
        Type Sigma_CvV_Vessel_rand = exp(logSigma_CvV_Vessel_rand);
      
      // Wind
        DATA_INTEGER(Wind_Vess_rand_flag);
        PARAMETER_VECTOR(Wind_Vess_rand_par);
        PARAMETER(logSigma_Wind_Vessel_rand); 
        Type Sigma_Wind_Vessel_rand = exp(logSigma_Wind_Vessel_rand);

      // Survey 
        DATA_INTEGER(Surv_Vess_rand_flag);
        PARAMETER_VECTOR(Surv_Vess_rand_par);
        PARAMETER(logSigma_Surv_Vessel_rand); 
        Type Sigma_Surv_Vessel_rand = exp(logSigma_Surv_Vessel_rand);
  
      // Survey Offset
        DATA_INTEGER(SurvOff_Vess_rand_flag);
        PARAMETER_VECTOR(SurvOff_Vess_rand_par);
        PARAMETER(logSigma_SurvOff_Vessel_rand); 
        Type Sigma_SurvOff_Vessel_rand = exp(logSigma_SurvOff_Vessel_rand);

      // CPUE lag
        DATA_INTEGER(Cpuelag_Vess_rand_flag);
        PARAMETER_VECTOR(Cpuelag_Vess_rand_par);
        PARAMETER(logSigma_Cpuelag_Vessel_rand); 
        Type Sigma_Cpuelag_Vessel_rand = exp(logSigma_Cpuelag_Vessel_rand);

  
  // Each row is the predictions for a data points; rows area areas
  int n = ActArea.size();
  matrix<Type>preds(n,Narea);

  Type Total;
  Type nll;

  // Now do the main calculation
  for (int Idata=0;Idata<n;Idata++)
   {
    // Relative area effect (note in logistic space)
    preds(Idata,0) = 0;
    
        for (int Iarea=1;Iarea<Narea;Iarea++){
            preds(Idata,Iarea) = AreaPar(Iarea-1);
            if (Int_AreaVess_rand_flag==1){
                //int Int_AreaVess_tbl_index = Int_AreaVess_tbl(Iarea-1,ActVess(Idata));// from old lookup table
                preds(Idata,Iarea) += Int_Area_Vessel_rand(Iarea-1,ActVess(Idata)); 
            }else {}

            if (Int_AreaSeason_rand_flag==1){
                preds(Idata,Iarea) += Int_Area_Season_rand(Iarea-1,ActSeason(Idata)); 
            }else {}
        }
    //for (int Iarea=1;Iarea<Narea;Iarea++){
    //for (int Ivessel=0;Ivessel<Nvessel;Ivessel++){
    //preds(Idata,Iarea) = AreaPar(Iarea-1);
    //preds(Idata,Iarea) += VesselPar(Ivessel);
    //}}
        // Covariates
        for (int Iarea=0;Iarea<Narea;Iarea++){
          // CPUE V 
            preds(Idata,Iarea) += CpueV_par*CpueV_C(Idata,Iarea);
            if (CpueV_Season_rand_flag==1){
                preds(Idata,Iarea) += CpueV_Season_rand_par(ActSeason(Idata))*CpueV_C(Idata,Iarea); 
            }
            if (CpueV_Vess_rand_flag==1){
                preds(Idata,Iarea) += CpueV_Vess_rand_par(ActVess(Idata))*CpueV_C(Idata,Iarea); 
            }
          
          // CPUE F
            preds(Idata,Iarea) += CpueF_par*CpueF_C(Idata,Iarea);
            if (CpueF_Season_rand_flag==1){
                preds(Idata,Iarea) += CpueF_Season_rand_par(ActSeason(Idata))*CpueF_C(Idata,Iarea); 
            }
            if (CpueF_Vess_rand_flag==1){
                preds(Idata,Iarea) += CpueF_Vess_rand_par(ActVess(Idata))*CpueF_C(Idata,Iarea); 
            }

          // Sort lag
            preds(Idata,Iarea) += Sort_par*Sort_C(Idata,Iarea);

          // Ice
            preds(Idata,Iarea) += Ice_par*Ice_C(Idata,Iarea);
            if (Ice_Vess_rand_flag==1){
                preds(Idata,Iarea) += Ice_Vess_rand_par(ActVess(Idata))*Ice_C(Idata,Iarea); 
            }
            if (Ice_Vess_Season_rand_flag==1){
                preds(Idata,Iarea) += Ice_Vess_Season_rand_par(ActVess(Idata),ActSeason(Idata))*Ice_C(Idata,Iarea); 
            }
          
          // Trad
            preds(Idata,Iarea) += Trad_par*Trad_C(Idata,Iarea);
            if (Trad_Vess_rand_flag==1){
                preds(Idata,Iarea) += Trad_Vess_rand_par(ActVess(Idata))*Trad_C(Idata,Iarea); 
            }

          // Cong
            preds(Idata,Iarea) += Cong_par*Cong_C(Idata,Iarea);
            if (Cong_Vess_rand_flag==1){
                preds(Idata,Iarea) += Cong_Vess_rand_par(ActVess(Idata))*Cong_C(Idata,Iarea); 
            }

           // Port
            preds(Idata,Iarea) += Port_par*Port_C(Idata,Iarea);  
            if (Port_Vess_rand_flag==1){
                preds(Idata,Iarea) += Port_Vess_rand_par(ActVess(Idata))*Port_C(Idata,Iarea); 
            }

            // CvF
            preds(Idata,Iarea) += CvF_par*CvF_C(Idata,Iarea);  
            if (CvF_Vess_rand_flag==1){
                preds(Idata,Iarea) += CvF_Vess_rand_par(ActVess(Idata))*CvF_C(Idata,Iarea); 
            }

            // CvV
            preds(Idata,Iarea) += CvV_par*CvV_C(Idata,Iarea);  
            if (CvV_Vess_rand_flag==1){
                preds(Idata,Iarea) += CvV_Vess_rand_par(ActVess(Idata))*CvV_C(Idata,Iarea); 
            }

            // Wind
            preds(Idata,Iarea) += Wind_par*Wind_C(Idata,Iarea);  
            if (Wind_Vess_rand_flag==1){
                preds(Idata,Iarea) += Wind_Vess_rand_par(ActVess(Idata))*Wind_C(Idata,Iarea); 
            }

            // SurvOff
            preds(Idata,Iarea) += SurvOff_par*SurvOff_C(Idata,Iarea);  
            if (SurvOff_Vess_rand_flag==1){
                preds(Idata,Iarea) += SurvOff_Vess_rand_par(ActVess(Idata))*SurvOff_C(Idata,Iarea); 
            }

            // Survey
            preds(Idata,Iarea) += Surv_par*Surv_C(Idata,Iarea);  
            if (Surv_Vess_rand_flag==1){
                preds(Idata,Iarea) += Surv_Vess_rand_par(ActVess(Idata))*Surv_C(Idata,Iarea); 
            }   

            // Cpuelag
            preds(Idata,Iarea) += Cpuelag_par*Cpuelag_C(Idata,Iarea);  
            if (Cpuelag_Vess_rand_flag==1){
                preds(Idata,Iarea) += Cpuelag_Vess_rand_par(ActVess(Idata))*Cpuelag_C(Idata,Iarea); 
            }   

        } 

      Total = 0;
      for (int Iarea=0;Iarea<Narea;Iarea++) { 
        preds(Idata,Iarea) = exp(preds(Idata,Iarea)); 
        Total+=preds(Idata,Iarea); }

      for (int Iarea=0;Iarea<Narea;Iarea++) 
        preds(Idata,Iarea) /= Total;
      // nll depends only on the area where the catch occurs
    nll -= log(preds(Idata,ActArea(Idata))); 
   }

// Area Random Intercept Vessel Effects
    if (Int_AreaVess_rand_flag==1){
        for (int Iarea=1;Iarea<Narea;Iarea++)
        for (int Ivessel=0;Ivessel<Nvessel;Ivessel++)
      nll -= dnorm(Int_Area_Vessel_rand(Iarea-1,Ivessel),Type(0.0),Sigma_Area_Vessel_rand,true);
   } else {}

// // Random Intercept Vessel Effects -- NEW
//     if (Int_AreaVess_rand_flag==1){
//         int x = Int_Area_Vessel_rand.size();
//         for (int Iarea_vess=0;Iarea_vess<x;Iarea_vess++)
//       nll -= dnorm(Int_Area_Vessel_rand(Iarea_vess),Type(0.0),Sigma_Area_Vessel_rand,true);
//    } else {}

// Area Random Intercept Season Effects // CLEAN THESE UP IN SAME LOOP!!
    if (Int_AreaSeason_rand_flag==1){
        for (int Iarea=1;Iarea<Narea;Iarea++)
        for (int Iseason=0;Iseason<Nseason;Iseason++)
      nll -= dnorm(Int_Area_Season_rand(Iarea-1,Iseason),Type(0.0),Sigma_Area_Season_rand,true);
   } else {}

    if (CpueV_Season_rand_flag==1){   
      for (int Iseason=0;Iseason<Nseason;Iseason++)
        nll -= dnorm(CpueV_Season_rand_par(Iseason),Type(0.0),Sigma_CpueV_Season_rand,true);
   } else {}

  if (CpueF_Season_rand_flag==1){
    for (int Iseason=0;Iseason<Nseason;Iseason++)
        nll -= dnorm(CpueF_Season_rand_par(Iseason),Type(0.0),Sigma_CpueF_Season_rand,true);
   } else {}

// Covariate Random Effects by Vessel - in same for loop // CLEAN THESE UP IN SAME LOOP!!
  if (Ice_Vess_rand_flag==1){
    for (int Ivessel=0;Ivessel<Nvessel;Ivessel++)
        nll -= dnorm(Ice_Vess_rand_par(Ivessel),Type(0.0),Sigma_Ice_Vessel_rand,true);
   } else {}

  if (Cong_Vess_rand_flag==1){
    for (int Ivessel=0;Ivessel<Nvessel;Ivessel++)
        nll -= dnorm(Cong_Vess_rand_par(Ivessel),Type(0.0),Sigma_Cong_Vessel_rand,true);
   } else {}

   if (Trad_Vess_rand_flag==1){ 
    for (int Ivessel=0;Ivessel<Nvessel;Ivessel++)
        nll -= dnorm(Trad_Vess_rand_par(Ivessel),Type(0.0),Sigma_Trad_Vessel_rand,true);
   } else {}

   if (CpueF_Vess_rand_flag==1){ 
    for (int Ivessel=0;Ivessel<Nvessel;Ivessel++)
        nll -= dnorm(CpueF_Vess_rand_par(Ivessel),Type(0.0),Sigma_CpueF_Vessel_rand,true);
   } else {}

  if (CpueV_Vess_rand_flag==1){
    for (int Ivessel=0;Ivessel<Nvessel;Ivessel++)
        nll -= dnorm(CpueV_Vess_rand_par(Ivessel),Type(0.0),Sigma_CpueV_Vessel_rand,true);
   } else {}

  if (Port_Vess_rand_flag==1){  
    for (int Ivessel=0;Ivessel<Nvessel;Ivessel++)
        nll -= dnorm(Port_Vess_rand_par(Ivessel),Type(0.0),Sigma_Port_Vessel_rand,true);
   } else {}

  if (CvF_Vess_rand_flag==1){  
    for (int Ivessel=0;Ivessel<Nvessel;Ivessel++)
        nll -= dnorm(CvF_Vess_rand_par(Ivessel),Type(0.0),Sigma_CvF_Vessel_rand,true);
   } else {}

  if (CvV_Vess_rand_flag==1){  
    for (int Ivessel=0;Ivessel<Nvessel;Ivessel++)
        nll -= dnorm(CvV_Vess_rand_par(Ivessel),Type(0.0),Sigma_CvV_Vessel_rand,true);
   } else {}

  if (Wind_Vess_rand_flag==1){  
    for (int Ivessel=0;Ivessel<Nvessel;Ivessel++)
        nll -= dnorm(Wind_Vess_rand_par(Ivessel),Type(0.0),Sigma_Wind_Vessel_rand,true);
   } else {}

  if (SurvOff_Vess_rand_flag==1){  
    for (int Ivessel=0;Ivessel<Nvessel;Ivessel++)
        nll -= dnorm(SurvOff_Vess_rand_par(Ivessel),Type(0.0),Sigma_SurvOff_Vessel_rand,true);
   } else {}

  if (Surv_Vess_rand_flag==1){  
    for (int Ivessel=0;Ivessel<Nvessel;Ivessel++)
        nll -= dnorm(Surv_Vess_rand_par(Ivessel),Type(0.0),Sigma_Surv_Vessel_rand,true);
   } else {}

  if (Cpuelag_Vess_rand_flag==1){
      for (int Ivessel=0;Ivessel<Nvessel;Ivessel++)
        nll -= dnorm(Cpuelag_Vess_rand_par(Ivessel),Type(0.0),Sigma_Cpuelag_Vessel_rand,true);
   } else {}


// Covariate Random Effects by Vessel-Season - in same for loop
if (Ice_Vess_Season_rand_flag==1){
    for (int Ivessel=0;Ivessel<Nvessel;Ivessel++)
    for (int Iseason=0;Iseason<Nseason;Iseason++)
        nll -= dnorm(Ice_Vess_Season_rand_par(Ivessel,Iseason),Type(0.0),Sigma_Ice_Vessel_Season_rand,true);
   } else {}

   
  // for manual checking
  REPORT(preds);            // plot
  REPORT(Int_Area_Vessel_rand);
  REPORT(Int_Area_Vessel_rand);
  REPORT(AreaPar); 
  REPORT(Ice_Vess_rand_par);

  return nll;
}

