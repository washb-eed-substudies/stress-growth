



rm(list=ls())

source(here::here("0-config.R"))

d <- readRDS("C:/Users/andre/Documents//EE/eed-substudy-data/bangladesh-cleaned-master-data.RDS")
#d<- readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/stress_growth_data.RDS"))

#Double check the outcome transformation:
summary(d$t3_saa_z01_raw)
summary(d$laz_t1_cat)


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
W2_F2.W2_anthro <- c(Wvars, Wvars2_F2 ,Wvars2_anthro, "laz_t1_cat", "waz_t1_cat") %>% unique(.)
W2_F2.W3_anthro <- c(Wvars, Wvars2_F2 ,Wvars3_anthro, "laz_t2", "waz_t2") %>% unique(.)
#W2_F2.W23_anthro <- c(Wvars, Wvars2_F2, Wvars2_anthro, Wvars3_anthro, "laz_t2_cat", "waz_t2_cat") %>% unique(.)
W2_F2.W23_anthro <- c(Wvars, Wvars2_F2, Wvars2_anthro, Wvars3_anthro) %>% unique(.)
W3_vital.W3_anthro <- c(Wvars, Wvars3_vital, Wvars3_anthro, "laz_t2_cat", "waz_t2_cat") %>% unique(.)
W3_salimetrics.W3_anthro <- c(Wvars, Wvars3_salimetrics, Wvars3_anthro, "laz_t2_cat", "waz_t2_cat") %>% unique(.)
W3_oragene.W3_anthro <- c(Wvars, Wvars3_oragene, Wvars3_anthro, "laz_t2_cat", "waz_t2_cat") %>% unique(.)


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
    if(grepl("t2_",i)){Wset=c(Wset, "hcz_t2")}
    else{Wset=c(Wset, "hcz_t2_cat")}}
  if(j=="hcz_t2"){Wset=c(Wset, "hcz_t1_cat")}
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

d <- d %>% mutate(
  stunt_t2 = ifelse(laz_t2 < -2, 1, 0),
  stunt_t3 = ifelse(laz_t3 < -2, 1, 0),
  wasted_t2 = ifelse(whz_t2 < -2, 1, 0),
  wasted_t3 = ifelse(whz_t3 < -2, 1, 0),
  underweight_t2 = ifelse(waz_t2 < -2, 1, 0),
  underweight_t3 = ifelse(waz_t3 < -2, 1, 0)
)

Yvars <- c("stunt_t2", "wasted_t2", "underweight_t2", "stunt_t3", "wasted_t3", "underweight_t3")

#Fit models
H1_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-pick_covariates(i, j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wset, family = "binomial")
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}



#Get primary contrasts
H1_adj_res <- NULL
for(i in 1:nrow(H1_adj_models)){
  res <- data.frame(X=H1_adj_models$X[i], Y=H1_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H1_adj_models$fit[i][[1]], d=H1_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y, binaryX = T)
  H1_adj_res <-  bind_rows(H1_adj_res , preds$res)
}
H1_adj_res

#saveRDS(H1_adj_res, here("results/adjusted/H1_adj_res.RDS"))
