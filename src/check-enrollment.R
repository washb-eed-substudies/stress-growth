rm(list=ls())

source(here::here("0-config.R"))


d<-readRDS(paste0(dropboxDir, "Data/Cleaned/Andrew/stress_growth_data.RDS"))

t2 <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_iso_pca")
t3 <- c("t3_cort_slope", "t3_residual_cort", "t3_cort_z01", "t3_cort_z03",
        "t3_saa_slope", "t3_residual_saa", "t3_saa_z01", "t3_saa_z02",
        "t3_map", "t3_hr_mean",
        "t3_gcr_mean", "t3_gcr_cpg12")

filtering <- function(row){
  all(is.na(row))
}

has_expt2<-d[apply(select(d, all_of(t2)), 1, filtering),]
has_expt3<-d[apply(select(d, all_of(t3)), 1, filtering),]
has_anyexp<-d[apply(select(d, all_of(c(t2, t3))), 1, filtering),]
nrow(has_expt2)
nrow(has_expt3)