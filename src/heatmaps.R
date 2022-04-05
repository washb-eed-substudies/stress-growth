


rm(list=ls())

source(here::here("0-config.R"))


H1_res <- readRDS(paste0(here::here(),"/results/adjusted/H1_adj_res.RDS"))
H2_res <- readRDS(paste0(here::here(),"/results/adjusted/H2_adj_res.RDS"))
H3_res <- readRDS(paste0(here::here(),"/results/adjusted/H3_adj_res.RDS"))
H4_res <- readRDS(paste0(here::here(),"/results/adjusted/H4_adj_res.RDS"))

H1_res_unadj <- readRDS(paste0(here::here(),"/results/unadjusted/H1_res.RDS"))
H1_res_sens <- readRDS(paste0(here::here(),"/results/sensitivity/H1_adj_res_noWgrowth.RDS"))
H1_res_glm <- readRDS(paste0(here::here(),"/results/sensitivity/H1_glm_res.RDS"))

clean_H1 <- function(res){
  res$Y <- gsub("_t2_t3","",res$Y)
  res$Y <- gsub("delta","Change in",res$Y)
  res$Y <- gsub("_"," ",res$Y)
  
  res$Y <- gsub("laz","LAZ",res$Y)
  res$Y <- gsub("waz","WAZ",res$Y)
  res$Y <- gsub("whz","WLZ",res$Y)
  res$Y <- gsub("hcz","HCZ",res$Y)
  
  res$Y <- gsub("t2","-Year 1",res$Y)
  res$Y <- gsub("t3","-Year 2",res$Y)
  
  res$Y <- gsub("len","Length",res$Y)
  res$Y <- gsub("wei","Weight",res$Y)
  res$Y <- gsub("hc","Head Circumference",res$Y)
  
  unique(res$Y)
  res$Y <- factor(res$Y, levels = rev(c("LAZ -Year 1","WAZ -Year 1","WLZ -Year 1","HCZ -Year 1",               
                                        "LAZ -Year 2",   "WAZ -Year 2","WLZ -Year 2","HCZ -Year 2",                
                                        "Change in LAZ","Change in WAZ","Change in WLZ","Change in HCZ",
                                        "Length velocity","Weight velocity","Head Circumference velocity")))
  return(res)
}


H1_res <- clean_H1(H1_res)
H1_res_unadj <- clean_H1(H1_res_unadj)
H1_res_sens <- clean_H1(H1_res_sens)
H1_res_glm <- clean_H1(H1_res_glm)


hm1 <- washbgam::plot_sig_heatmap(H1_res)
hm1_unadj <- washbgam::plot_sig_heatmap(H1_res_unadj)
hm1_sens <- washbgam::plot_sig_heatmap(H1_res_sens)
hm1_glm <- washbgam::plot_sig_heatmap(H1_res_glm)

#Other hypotheses
hm2 <- washbgam::plot_sig_heatmap(H2_res)
hm3 <- washbgam::plot_sig_heatmap(H3_res)
hm4 <- washbgam::plot_sig_heatmap(H4_res)

save(list=ls(pattern="hm"), file=paste0(here::here(),"/figures/supplementary/heatmaps.Rdata"))

