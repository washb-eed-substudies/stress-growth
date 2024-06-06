



rm(list=ls())

source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/stress_growth_data_clean.RDS"))

#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "HHwealth_scaled",
         "cesd_sum_t2", "diar7d_t2", "tr", "life_viol_any_t3")

Wvars[!(Wvars %in% colnames(d))]



# time varying covariates:
Wvars2_anthro<-c("ageday_at2", "month_at2")
Wvars3_anthro<-c("ageday_at3", "month_at3", "diar7d_t3", "cesd_sum_ee_t3", "pss_sum_mom_t3", "life_viol_any_t3")  

Wvars2_F2<-c("ageday_ut2", "month_ut2") 
Wvars3_vital<-c("ageday_t3_vital", "month_vt3", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t3", "life_viol_any_t3") 
Wvars3_salimetrics<-c("ageday_t3_salimetrics", "month_lt3", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t3", "life_viol_any_t3") 
Wvars3_oragene<-c("ageday_t3_oragene", "month_ot3", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t3", "life_viol_any_t3") 


# add time-varying covariates
# add hcz and time of day measurement later in pick covariates function 
W2_F2.W2_anthro <- c(Wvars, Wvars2_F2 ,Wvars2_anthro) %>% unique(.)
W2_F2.W3_anthro <- c(Wvars, Wvars2_F2 ,Wvars3_anthro) %>% unique(.)
W2_F2.W23_anthro <- c(Wvars, Wvars2_F2, Wvars2_anthro, Wvars3_anthro) %>% unique(.)
W3_vital.W3_anthro <- c(Wvars, Wvars3_vital, Wvars3_anthro) %>% unique(.)
W3_salimetrics.W3_anthro <- c(Wvars, Wvars3_salimetrics, Wvars3_anthro) %>% unique(.)
W3_oragene.W3_anthro <- c(Wvars, Wvars3_oragene, Wvars3_anthro) %>% unique(.)


pick_covariates <- function(i, j){
  # i is exposure as string
  # j is outcome as string
  # choose correct/build correct adjustment set based on exposure and outcome
  if(grepl("t2_", i)){
    if(grepl("_t2_t3", j)){Wset = W2_F2.W23_anthro}
    else if(grepl("_t2", j)){Wset = W2_F2.W2_anthro}
    else if(grepl("_t3", j)){Wset = W2_F2.W3_anthro}}
  else if(grepl("saa|cort", i)){
    if(grepl("residual", i)){Wset = W3_salimetrics.W3_anthro}
    else{Wset = c(W3_salimetrics.W3_anthro, "t3_col_time_z01_cont")}}
  else if(i %in% c("t3_map", "t3_hr_mean")){Wset = W3_vital.W3_anthro}
  else{Wset = W3_oragene.W3_anthro}
  
  if(j=="hcz_t3"){
    if(grepl("t2_",i)){Wset=c(Wset)}
    else{Wset=c(Wset)}}
  if(j=="hcz_t2"){Wset=c(Wset)}
  return(Wset)
}



#Loop over exposure-outcome pairs

##Hypothesis 1a
#Urinary creatine-adjusted F2-isoprostanes isomer score  at Year 1 is negatively associated with 
#concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 1.

# Exposure: Quartile of F2-isoprostanes isomer score
# Primary Outcome  : Child LAZ at Year 1
# Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 1
# Tertiary Outcomes: Child WLZ at Year 1

##Hypothesis 1b
#Urinary creatine-adjusted F2-isoprostanes isomer score at Year 1 is negatively 
#associated with child growth velocity (kg/month or cm/month) between the Year 1 and Year 2 visits.	

#Exposure: Quartile of F2-isoprostanes isomer score
#Primary Outcome: Child length velocity (in cm/month) from Year 1 to Year 2
#Secondary Outcome: Child weight velocity (in kg/month) and head circumference velocity (in cm/month) from Year 1 to Year 2

## Hypothesis 1c
#Urinary creatine-adjusted F2-isoprostanes isomer score at Year 1 is negatively 
#associated with subsequent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2. 

#Exposure: Quartile of F2-isoprostanes isomer score
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcome: Child WLZ at Year 2

##Hypothesis 1d
#Urinary creatine-adjusted F2-isoprostanes isomer score at Year 1 is negatively 
#associated with the change in child LAZ, WAZ, WLZ, and head circumference-for-age Z score from Year 1 to Year 2. 

#Exposure: Quartiles of F2-isoprostanes isomer score
#Primary Outcome: Change in child LAZ from Year 1 to Year 2
#Secondary Outcome: Change in child WAZ and head circumference-for-age Z score from Year 1 to Year 2
#Tertiary Outcomes: Change in child WLZ from Year 1 to Year 2

Xvars <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_iso_pca")            
Yvars <- c("laz_t2", "waz_t2", "whz_t2" ,"hcz_t2", 
           "len_velocity_t2_t3", "wei_velocity_t2_t3", "hc_velocity_t2_t3",
           "laz_t3", "waz_t3", "whz_t3", "hcz_t3",
           "delta_laz_t2_t3", "delta_waz_t2_t3", "delta_whz_t2_t3", "delta_hcz_t2_t3")

#Fit models
H1_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-pick_covariates(i, j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wset, forcedW = NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}



#Get primary contrasts
H1_adj_res <- NULL
for(i in 1:nrow(H1_adj_models)){
  res <- data.frame(X=H1_adj_models$X[i], Y=H1_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H1_adj_models$fit[i][[1]], d=H1_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1_adj_res <-  bind_rows(H1_adj_res , preds$res)
}

# #Make list of plots
# H1_adj_plot_list <- NULL
# H1_adj_plot_data <- NULL
# for(i in 1:nrow(H1_adj_models)){
#   res <- data.frame(X=H1_adj_models$X[i], Y=H1_adj_models$Y[i])
#   simul_plot <- gam_simul_CI(H1_adj_models$fit[i][[1]], H1_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
#   H1_adj_plot_list[[i]] <-  simul_plot$p
#   H1_adj_plot_data <-  rbind(H1_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
# }


## Hypothesis 2a
#Change in slope between pre- and post-stressor cortisol measured at Year 2 is positively associated 
#with concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of pre- and post-stressor cortisol at Year 2
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcome: Child WLZ at Year 2

##Hypothesis 2b
#Residualized gain score for cortisol measured at Year 2 is positively associated 
#with concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of pre- and post-stressor cortisol at Year 2
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcomes: Child WLZ at Year 2

##Hypothesis 2c
#Change in slope between pre- and post-stressor alpha-amylase measured at Year 2 is negatively associated 
#with concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of pre- and post-stressor alpha-amylase at Year 2
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcome: Child WLZ at Year 2

##Hypothesis 2d
#Residualized gain score for alpha-amylase measured at Year 2 is negatively associated 
#with concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of pre- and post-stressor alpha-amylase at Year 2
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcome: Child WLZ at Year 2

Xvars <- c("t3_cort_slope", "t3_residual_cort", "t3_cort_z01", "t3_cort_z03",
           "t3_saa_slope", "t3_residual_saa", "t3_saa_z01", "t3_saa_z02")          
Yvars <- c("laz_t3", "waz_t3", "whz_t3", "hcz_t3")

#Fit models
H2_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-pick_covariates(i, j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wset)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2_adj_models <- bind_rows(H2_adj_models, res)
  }
}

#Get primary contrasts
H2_adj_res <- NULL
for(i in 1:nrow(H2_adj_models)){
  res <- data.frame(X=H2_adj_models$X[i], Y=H2_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H2_adj_models$fit[i][[1]], d=H2_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H2_adj_res <-  bind_rows(H2_adj_res , preds$res)
}

# #Make list of plots
# H2_adj_plot_list <- NULL
# H2_adj_plot_data <- NULL
# for(i in 1:nrow(H2_adj_models)){
#   res <- data.frame(X=H2_adj_models$X[i], Y=H2_adj_models$Y[i])
#   simul_plot <- gam_simul_CI(H2_adj_models$fit[i][[1]], H2_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
#   H2_adj_plot_list[[i]] <-  simul_plot$p
#   H2_adj_plot_data <-  rbind(H2_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred%>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
# }



##Hypothesis 3a
#Mean arterial pressure measured at Year 2 is negatively associated with concurrent 
#child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of mean arterial pressure at Year 2
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcomes: Child WLZ at Year 2

##Hypothesis 3b
#Resting heart rate measured at Year 2 is negatively associated with concurrent 
#child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of resting heart rate at Year 2
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcomes: Child WLZ at Year 2

Xvars <- c("t3_map", "t3_hr_mean")            
Yvars <- c("laz_t3", "waz_t3", "whz_t3", "hcz_t3")


#Fit models
H3_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-pick_covariates(i, j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wset)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H3_models <- bind_rows(H3_models, res)
  }
}

#Get primary contrasts
H3_res <- NULL
for(i in 1:nrow(H3_models)){
  res <- data.frame(X=H3_models$X[i], Y=H3_models$Y[i])
  preds <- predict_gam_diff(fit=H3_models$fit[i][[1]], d=H3_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H3_res <-  bind_rows(H3_res , preds$res)
}

# #Make list of plots
# H3_plot_list <- NULL
# H3_plot_data <- NULL
# for(i in 1:nrow(H3_models)){
#   res <- data.frame(X=H3_models$X[i], Y=H3_models$Y[i])
#   simul_plot <- gam_simul_CI(H3_models$fit[i][[1]], H3_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
#   H3_plot_list[[i]] <-  simul_plot$p
#   H3_plot_data <-  rbind(H3_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred%>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
# }





##Hypothesis 4a
#Glucocorticoid receptor (NR3C1) exon 1F promoter methylation in saliva samples at Year 2 
#is negatively associated with concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of overall percentage of methylation across the entire promoter region at Year 2 post-intervention
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcomes: Child WLZ at Year 2

##Hypothesis 4b
#Glucocorticoid receptor NGFI-A transcription factor binding site methylation in saliva samples at Year 2 
#is negatively associated with concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of percentage methylation at NGFI-A transcription factor binding 	site (CpG site #12)
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcomes: Child WLZ at Year 2

Xvars <- c("t3_gcr_mean", "t3_gcr_cpg12")            
Yvars <- c("laz_t3", "waz_t3", "whz_t3", "hcz_t3")

#Fit models
H4_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-pick_covariates(i, j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wset)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H4_models <- bind_rows(H4_models, res)
  }
}

#Get primary contrasts
H4_res <- NULL
for(i in 1:nrow(H4_models)){
  res <- data.frame(X=H4_models$X[i], Y=H4_models$Y[i])
  preds <- predict_gam_diff(fit=H4_models$fit[i][[1]], d=H4_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H4_res <-  bind_rows(H4_res , preds$res)
}

# #Make list of plots
# H4_plot_list <- NULL
# H4_plot_data <- NULL
# for(i in 1:nrow(H4_models)){
#   res <- data.frame(X=H4_models$X[i], Y=H4_models$Y[i])
#   simul_plot <- gam_simul_CI(H4_models$fit[i][[1]], H4_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
#   H4_plot_list[[i]] <-  simul_plot$p
#   H4_plot_data <-  rbind(H4_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred%>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
# }



#Save results
res <- bind_rows(H1_adj_res,H2_adj_res,H3_res,H4_res)
saveRDS(res, here("results/adjusted/adj_res_sens.RDS"))


