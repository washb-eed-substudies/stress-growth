rm(list=ls())
source(here::here("0-config.R"))
library(cowplot)
library(patchwork)
theme_set(theme_ki())

  color_levels = c("LAZ Year 1", "WAZ Year 1", "WLZ Year 1", "HCZ Year 1",
                   "LAZ Year 2", "WAZ Year 2", "WLZ Year 2", "HCZ Year 2",
                   "Change in LAZ\nYear 1 to Year 2", "Change in WAZ\nYear 1 to Year 2", "Change in WLZ\nYear 1 to Year 2", "Change in HCZ\nYear 1 to Year 2",
                   "Length velocity\nYear 1 and Year 2", "Weight velocity\nYear 1 to Year 2", "Head circumference velocity\nYear 1 to Year 2")

#load spline data
H1_spline <- readRDS(paste0(here(),"/figure-data/H1_adj_spline_data.RDS"))
H2_spline <- readRDS(paste0(here(),"/figure-data/H2_adj_spline_data.RDS"))
H3_spline <- readRDS(paste0(here(),"/figure-data/H3_adj_spline_data.RDS"))
H4_spline <- readRDS(paste0(here(),"/figure-data/H4_adj_spline_data.RDS"))

d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/stress_growth_data_clean.RDS"))
ggplot(d, aes(x=t2_f2_8ip, y=laz_t3)) + geom_point() + geom_smooth(method="lm")


#load results for quartiles
H1_quartiles <- readRDS(here("results/adjusted/H1_adj_res.RDS")) %>% rename(Xvar = X, Yvar = Y)
H2_quartiles <- readRDS(here("results/adjusted/H2_adj_res.RDS")) %>% rename(Xvar = X, Yvar = Y)
H3_quartiles <- readRDS(here("results/adjusted/H3_adj_res.RDS")) %>% rename(Xvar = X, Yvar = Y)
H4_quartiles <- readRDS(here("results/adjusted/H4_adj_res.RDS")) %>% rename(Xvar = X, Yvar = Y)

H1_d <- left_join(H1_spline, H1_quartiles, by=c("Xvar", "Yvar"))
head(H1_d)
H1_d <- clean_res(H1_d)

H1_d$Xvar <- factor(H1_d$Xvar,
                   levels = c("t3_gcr_mean", "t3_gcr_cpg12",
                              "t3_cort_slope", "t3_residual_cort", "t3_cort_z01", "t3_cort_z03",
                              "t3_saa_slope", "t3_residual_saa", "t3_saa_z01", "t3_saa_z02",
                              "t3_map", "t3_hr_mean",
                              "t2_f2_8ip", "t2_f2_23d","t2_f2_VI", "t2_f2_12i", "t2_iso_pca"),
                   labels = c("Mean Overall Percentage\nGlucocorticoid Receptor Methylation", 
                              "Percentage methylation at NGFI-A\ntranscription factor binding site",
                              "Cortisol Reactivity (ug/dl/min)", 
                              "Cortisol Residualized Gain Score", 
                              "Pre-stressor Cortisol (ug/dl)", "Post-stressor Cortisol (ug/dl)", 
                              "sAA Reactivity (U/ml/min)", 
                              "sAA Residualized Gain Score", 
                              "Pre-stressor sAA (U/ml)", "Post-stressor sAA (U/ml)",
                              "Mean arterial pressure (mmHg)", "Mean Resting Heart Rate (bpm)", 
                              "IPF(2a)-III (ng/mg creatinine)", "2,3-dinor-iPF(2a)-III (ng/mg creatinine)", 
                              "iPF(2a)-VI (ng/mg creatinine)", "8,12-iso-iPF(2a)-VI (ng/mg creatinine)",
                              "Combined oxidative stress biomarker score"))



#cut plots at the 10th-90th percentile distributions
H1_d <- H1_d %>% group_by(Xvar, Yvar) %>% mutate(quantile=ntile(X,10)) %>% filter(!(quantile %in% c(1,10)))

#TO DO! 
#Check unadjusted splines vs LMs


  p1 <- ggplot(H1_d %>% filter(grepl("laz",Yvar)), aes(x = X, color=Yvar, fill=Yvar)) +
    geom_smooth(aes(y = fit ), se = F) +
    #geom_smooth(aes(y = Y ), se = F, method = "lm") +
  #geom_point(aes(x=X, y= Y), alpha=0.1) +
  #geom_rug(aes(x=X), sides="b", length = unit(0.15, "npc"), size=1, alpha=0.05, color="grey30") +
      geom_ribbon(aes(ymin=lwrS, ymax=uprS), alpha=0.5) +
      facet_wrap(Yvar~Xvar, scales="free", ncol=5) +
    geom_vline(aes(xintercept = q1), linetype="dashed", color = "grey50") +
    geom_vline(aes(xintercept = q3), linetype="dashed", color = "grey50") +
      scale_colour_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) +
      scale_fill_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) +
      xlab("Isoprostane") + ylab("LAZ (or change in LAZ)")
  p1
  
  
  p2 <- ggplot(H1_d %>% filter(Yvar=="laz_t2"), aes(x = X, color=Yvar, fill=Yvar)) +
    geom_line(aes(y = fit ), se = F) +
    #geom_smooth(aes(y = Y ), se = F, method = "lm") +
    #geom_point(aes(x=X, y= Y), alpha=0.1) +
    #geom_rug(aes(x=X), sides="b", length = unit(0.15, "npc"), size=1, alpha=0.05, color="grey30") +
    geom_ribbon(aes(ymin=lwrS, ymax=uprS), alpha=0.5) +
    facet_wrap(~Xvar, scales="free", ncol=2) +
    geom_vline(aes(xintercept = q1), linetype="dashed", color = "grey50") +
    geom_vline(aes(xintercept = q3), linetype="dashed", color = "grey50") +
    scale_x_continuous(expand = c(0,0)) +
    scale_colour_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) +
    scale_fill_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) +
    xlab("Log Isoprostane concentration") + ylab("LAZ")
  p2

  saveRDS(p2, file=paste0(here::here(),"/figures/supplementary/spline_plot.RDS"))
  