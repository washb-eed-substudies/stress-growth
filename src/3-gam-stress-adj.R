rm(list=ls())

source(here::here("0-config.R"))
source(here::here("src/0-gam-functions.R"))

d <- read.csv(paste0(dropboxDir,"Data/Cleaned/Audrie/bangladesh-dm-ee-stress-growth-covariates-stresslab-anthro.csv"))

#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "elec", "asset_wardrobe",
         "asset_table", "asset_chair", "asset_clock","asset_khat", "asset_chouki", 
         "asset_radio", "asset_tv", "asset_refrig", "asset_bike", "asset_moto", "asset_sewmach", 
         "asset_mobile", "n_cattle", "n_goat", "n_chicken")

Wvars[!(Wvars %in% colnames(d))]

#WARNING: adjustment set is missing time-varying covariates and child sex

#Fit GAM model with random effects for childid
res_adj <- fit_RE_gam(d=d, X="t3_cort_z01", Y="laz_t3",  W=Wvars)

#Get predictions of differences from the 25th percentile of exposure
preds_adj <- predict_gam_diff(fit=res_adj$fit, d=res_adj$dat, quantile_diff=c(0.25,0.75), Xvar="t3_cort_z01", Yvar="laz_t3")

#Primary parameter we are estimating: difference between 25th and 75th percentile of the exposure
preds_adj$res

#Plot the difference from the 25th percentile for the full range of the exposure:
#NOTE: not making these plots anymore, just using for diagnostics
# p <- plot_gam_diff(preds_adj$plotdf)
# print(p)

#Fit spline with simultaneous confidence intervals
simul_plot <- gam_simul_CI(res_adj$fit, res_adj$dat, xlab="t3_cort_z01", ylab="laz_t3", title="example title")
simul_plot$p


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

Xvars <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i")            
Yvars <- c("laz_t2", "waz_t2", "whz_t2" ,"hcz_t2", 
           "len_velocity_t2_t3", "wei_velocity_t2_t3", "hc_velocity_t2_t3",
           "laz_t3", "waz_t3", "whz_t3", "hcz_t3",
           "delta_laz_t2_t3", "delta_waz_t2_t3", "delta_whz_t2_t3", "delta_hcz_t2_t3")

#Fit models
H1_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wvars)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_models <- bind_rows(H1_models, res)
  }
}

#Get primary contrasts
H1_res <- NULL
for(i in 1:nrow(H1_models)){
  res <- data.frame(X=H1_models$X[i], Y=H1_models$Y[i])
  preds <- predict_gam_diff(fit=H1_models$fit[i][[1]], d=H1_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1_res <-  bind_rows(H1_res , preds$res)
}
H1_res$adjusted <- 0

#Make list of plots
H1_plot_list <- NULL
H1_plot_data <- NULL
for(i in 1:nrow(H1_models)){
  res <- data.frame(X=H1_models$X[i], Y=H1_models$Y[i])
  simul_plot <- gam_simul_CI(H1_models$fit[i][[1]], H1_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1_plot_list[[i]] <-  simul_plot$p
  H1_plot_data <-  rbind(H1_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
saveRDS(H1_models, here("models/H1_models.RDS"))

#Save results
saveRDS(H1_res, here("results/adjusted/H1_res.RDS"))


#Save plots
saveRDS(H1_plot_list, here("figure-objects/H1_adj_splines.RDS"))

#Save plot data
saveRDS(H1_plot_data, here("figure-data/H1_adj_spline_data.RDS"))




