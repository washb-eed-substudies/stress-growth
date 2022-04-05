



rm(list=ls())

source(here::here("0-config.R"))

d <- box_read("880476682582")


#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "HHwealth_scaled",
         "cesd_sum_t2", "diar7d_t2", "tr", "life_viol_any_t3")

Wvars[!(Wvars %in% colnames(d))]



Xvars <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_iso_pca")            
Yvars <- c("laz_t2", "waz_t2", "whz_t2" ,"hcz_t2", 
           "len_velocity_t2_t3", "wei_velocity_t2_t3", "hc_velocity_t2_t3",
           "laz_t3", "waz_t3", "whz_t3", "hcz_t3",
           "delta_laz_t2_t3", "delta_waz_t2_t3", "delta_whz_t2_t3", "delta_hcz_t2_t3")


Xvars <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_iso_pca")            


library(corrplot)


df <- d %>% select(Xvars,"t3_cort_slope", "t3_residual_cort", "t3_cort_z01", "t3_cort_z03",
                                     "t3_saa_slope", "t3_residual_saa", "t3_saa_z01", "t3_saa_z02","t3_map", "t3_hr_mean", "t3_gcr_mean", "t3_gcr_cpg12",Yvars)

M = cor(as.data.frame(df), use = "complete.obs")
corrp <- corrplot(M, method = 'color') # colorful number
corrp