rm(list=ls())

source(here::here("0-config.R"))
source(here::here("src/0-gam-functions.R"))

d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/stress_growth_data.RDS"))


#Example:

#Fit GAM model with random effects for childid
res_unadj <- fit_RE_gam(d=d, X="t3_cort_z01_raw", Y="laz_t3",  W=NULL)

#Get predictions of differences from the 25th percentile of exposure
preds_unadj <- predict_gam_diff(fit=res_unadj$fit, d=res_unadj$dat, quantile_diff=c(0.25,0.75), Xvar="delta_TS", Yvar="laz_t3")


#Primary parameter we are estimating: difference between 25th and 75th percentile of the exposure
preds_unadj$res

#Plot the difference from the 25th percentile for the full range of the exposure:
#NOTE: not making these plots anymore, just using for diagnostics
# p <- plot_gam_diff(preds_unadj$plotdf)
# print(p)        

#Fit spline with simultaneous confidence intervals
simul_plot <- gam_simul_CI(res_unadj$fit, res_unadj$dat, xlab="delta_TS", ylab="laz_t3", title="example title")
simul_plot$p


#Loop over exposure-outcome pairs

#Hypothesis 1a
#Urinary creatine-adjusted F2-isoprostanes isomer score  at Year 1 is negatively associated with 
#concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 1.


# Exposure: Quartile of F2-isoprostanes isomer score
# Primary Outcome  : Child LAZ at Year 1
# Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 1
# Tertiary Outcomes: Child WLZ at Year 1

Xvars <- c("t2_f2_8ip_raw", "t2_f2_23d_raw", "t2_f2_VI_raw", "t2_f2_12i_raw")            
Yvars <- c("laz_t2", "waz_t2", "whz_t2" ,"hcz_t2" )

H1_list <- list()
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    H1_list <- bind_rows(H1_list, res_unadj)
  }
}




