rm(list=ls())

source(here::here("0-config.R"))
source(here::here("src/0-gam-functions.R"))
load(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.RData"))


#Fit GAM model with random effects for childid
res_unadj <- fit_RE_gam(d=d, Y="laz_t3", X="delta_TS", W=c("sex","n_chicken"))

#Get predictions of differences from the 25th percentile of exposure
preds_unadj <- predict_gam_diff(fit=res_unadj$fit, d=res$dat, quantile_diff=c(0.25,0.75), Xvar="delta_TS", Yvar="laz_t3")


#Primary parameter we are estimating: difference between 25th and 75th percentile of the exposure
preds_unadj$res

#Plot the difference from the 25th percentile for the full range of the exposure:
p <- plot_gam_diff(preds_unadj$plotdf)
print(p)        


#Fit adjusted GAM model
#W should be full list of covariates considered
#Note: this vector may not be correct based on the analysis plan.
#DOUBLE CHECK!
Wvars<-c("sex","birthord", "momage","momheight","momedu", "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock","asset_khat", "asset_chouki", "asset_radio", "asset_tv", "asset_refrig", "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", "n_cattle", "n_goat", "n_chicken")

res_adj <- fit_RE_gam(d=d, Y="laz_t3", X="delta_TS", W=Wvars)


, Xvar="delta_TS", Yvar="laz_t3"
