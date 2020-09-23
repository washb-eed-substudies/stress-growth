rm(list=ls())
source(here::here("0-config.R"))
library(cowplot)
library(patchwork)
theme_set(theme_ki())

#load spline data
H1_spline <- readRDS(paste0(dropboxDir,"results/stress-growth-models/figure-data/H1_adj_spline_data.RDS"))
H2_spline <- readRDS(paste0(dropboxDir,"results/stress-growth-models/figure-data/H2_adj_spline_data.RDS"))
H3_spline <- readRDS(paste0(dropboxDir,"results/stress-growth-models/figure-data/H3_adj_spline_data.RDS"))
H4_spline <- readRDS(paste0(dropboxDir,"results/stress-growth-models/figure-data/H4_adj_spline_data.RDS"))

#load results for quartiles
H1_quartiles <- readRDS(here("results/adjusted/H1_adj_res.RDS"))
H2_quartiles <- readRDS(here("results/adjusted/H2_adj_res.RDS"))
H3_quartiles <- readRDS(here("results/adjusted/H3_adj_res.RDS"))
H4_quartiles <- readRDS(here("results/adjusted/H4_adj_res.RDS"))


d1 <- rbind(
  data.frame(x="IPF(2a)-III", y="LAZ Year 1", H1_spline%>%filter(Xvar=="t2_f2_8ip", Yvar=="laz_t2"), H1_quartiles%>%filter(X=="t2_f2_8ip", Y=="laz_t2")%>%select(q1, q3)),
  data.frame(x="IPF(2a)-III", y="LAZ Year 2", H1_spline%>%filter(Xvar=="t2_f2_8ip", Yvar=="laz_t3"), H1_quartiles%>%filter(X=="t2_f2_8ip", Y=="laz_t3")%>%select(q1, q3)),
  data.frame(x="IPF(2a)-III", y="Change in LAZ\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="t2_f2_8ip", Yvar=="delta_laz_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_8ip", Y=="delta_laz_t2_t3")%>%select(q1, q3)),
  data.frame(x="IPF(2a)-III", y="Length velocity\nYear 1 and Year 2", H1_spline%>%filter(Xvar=="t2_f2_8ip", Yvar=="len_velocity_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_8ip", Y=="len_velocity_t2_t3")%>%select(q1, q3)))
d2 <- rbind(
  data.frame(x="2,3-dinor-iPF(2a)-III", y="LAZ Year 1", H1_spline%>%filter(Xvar=="t2_f2_23d", Yvar=="laz_t2"), H1_quartiles%>%filter(X=="t2_f2_23d", Y=="laz_t2")%>%select(q1, q3)),
  data.frame(x="2,3-dinor-iPF(2a)-III", y="LAZ Year 2",  H1_spline%>%filter(Xvar=="t2_f2_23d", Yvar=="laz_t3"), H1_quartiles%>%filter(X=="t2_f2_23d", Y=="laz_t3")%>%select(q1, q3)),
  data.frame(x="2,3-dinor-iPF(2a)-III", y="Change in LAZ\nYear 1 to Year 2",  H1_spline%>%filter(Xvar=="t2_f2_23d", Yvar=="delta_laz_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_23d", Y=="delta_laz_t2_t3")%>%select(q1, q3)),
  data.frame(x="2,3-dinor-iPF(2a)-III", y="Length velocity\nYear 1 and Year 2",  H1_spline%>%filter(Xvar=="t2_f2_23d", Yvar=="len_velocity_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_23d", Y=="len_velocity_t2_t3")%>%select(q1, q3)))
d3 <- rbind(
  data.frame(x="iPF(2a)-VI", y="LAZ Year 1", H1_spline%>%filter(Xvar=="t2_f2_VI", Yvar=="laz_t2"), H1_quartiles%>%filter(X=="t2_f2_VI", Y=="laz_t2")%>%select(q1, q3)),
  data.frame(x="iPF(2a)-VI", y="LAZ Year 2", H1_spline%>%filter(Xvar=="t2_f2_VI", Yvar=="laz_t3"), H1_quartiles%>%filter(X=="t2_f2_VI", Y=="laz_t3")%>%select(q1, q3)),
  data.frame(x="iPF(2a)-VI", y="Change in LAZ\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="t2_f2_VI", Yvar=="delta_laz_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_VI", Y=="delta_laz_t2_t3")%>%select(q1, q3)),
  data.frame(x="iPF(2a)-VI", y="Length velocity\nYear 1 and Year 2", H1_spline%>%filter(Xvar=="t2_f2_VI", Yvar=="len_velocity_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_VI", Y=="len_velocity_t2_t3")%>%select(q1, q3)))
d4 <- rbind(
  data.frame(x="8,12-iso-iPF(2a)-VI", y="LAZ Year 1", H1_spline%>%filter(Xvar=="t2_f2_12i", Yvar=="laz_t2"), H1_quartiles%>%filter(X=="t2_f2_12i", Y=="laz_t2")%>%select(q1, q3)),
  data.frame(x="8,12-iso-iPF(2a)-VI", y="LAZ Year 2", H1_spline%>%filter(Xvar=="t2_f2_12i", Yvar=="laz_t3"), H1_quartiles%>%filter(X=="t2_f2_12i", Y=="laz_t3")%>%select(q1, q3)),
  data.frame(x="8,12-iso-iPF(2a)-VI", y="Change in LAZ\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="t2_f2_12i", Yvar=="delta_laz_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_12i", Y=="delta_laz_t2_t3")%>%select(q1, q3)),
  data.frame(x="8,12-iso-iPF(2a)-VI", y="Length velocity\nYear 1 and Year 2", H1_spline%>%filter(Xvar=="t2_f2_12i", Yvar=="len_velocity_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_12i", Y=="len_velocity_t2_t3")%>%select(q1, q3)))
d5 <- rbind(
  data.frame(x="Combined urinary oxidative\nstress biomarkers", y="LAZ Year 1", H1_spline%>%filter(Xvar=="iso.pca", Yvar=="laz_t2"), H1_quartiles%>%filter(X=="iso.pca", Y=="laz_t2")%>%select(q1, q3)),
  data.frame(x="Combined urinary oxidative\nstress biomarkers", y="LAZ Year 2", H1_spline%>%filter(Xvar=="iso.pca", Yvar=="laz_t3"), H1_quartiles%>%filter(X=="iso.pca", Y=="laz_t3")%>%select(q1, q3)),
  data.frame(x="Combined urinary oxidative\nstress biomarkers", y="Change in LAZ\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="iso.pca", Yvar=="delta_laz_t2_t3"), H1_quartiles%>%filter(X=="iso.pca", Y=="delta_laz_t2_t3")%>%select(q1, q3)),
  data.frame(x="Combined urinary oxidative\nstress biomarkers", y="Length velocity\nYear 1 and Year 2", H1_spline%>%filter(Xvar=="iso.pca", Yvar=="len_velocity_t2_t3"), H1_quartiles%>%filter(X=="iso.pca", Y=="len_velocity_t2_t3")%>%select(q1, q3)))


d6 <- rbind(
  data.frame(x="IPF(2a)-III", y="WAZ Year 1", H1_spline%>%filter(Xvar=="t2_f2_8ip", Yvar=="waz_t2"), H1_quartiles%>%filter(X=="t2_f2_8ip", Y=="waz_t2")%>%select(q1, q3)),
  data.frame(x="IPF(2a)-III", y="WLZ Year 1", H1_spline%>%filter(Xvar=="t2_f2_8ip", Yvar=="whz_t2"), H1_quartiles%>%filter(X=="t2_f2_8ip", Y=="whz_t2")%>%select(q1, q3)),
  data.frame(x="IPF(2a)-III", y="HCZ Year 1", H1_spline%>%filter(Xvar=="t2_f2_8ip", Yvar=="hcz_t2"), H1_quartiles%>%filter(X=="t2_f2_8ip", Y=="hcz_t2")%>%select(q1, q3)))
d7 <- rbind(
  data.frame(x="2,3-dinor-iPF(2a)-III", y="WAZ Year 1", H1_spline%>%filter(Xvar=="t2_f2_23d", Yvar=="waz_t2"), H1_quartiles%>%filter(X=="t2_f2_23d", Y=="waz_t2")%>%select(q1, q3)),
  data.frame(x="2,3-dinor-iPF(2a)-III", y="WLZ Year 1",  H1_spline%>%filter(Xvar=="t2_f2_23d", Yvar=="whz_t2"), H1_quartiles%>%filter(X=="t2_f2_23d", Y=="whz_t2")%>%select(q1, q3)),
  data.frame(x="2,3-dinor-iPF(2a)-III", y="HCZ Year 1",  H1_spline%>%filter(Xvar=="t2_f2_23d", Yvar=="hcz_t2"), H1_quartiles%>%filter(X=="t2_f2_23d", Y=="hcz_t2")%>%select(q1, q3)))
d8 <- rbind(
  data.frame(x="iPF(2a)-VI", y="WAZ Year 1", H1_spline%>%filter(Xvar=="t2_f2_VI", Yvar=="waz_t2"), H1_quartiles%>%filter(X=="t2_f2_VI", Y=="waz_t2")%>%select(q1, q3)),
  data.frame(x="iPF(2a)-VI", y="WLZ Year 1", H1_spline%>%filter(Xvar=="t2_f2_VI", Yvar=="whz_t2"), H1_quartiles%>%filter(X=="t2_f2_VI", Y=="whz_t2")%>%select(q1, q3)),
  data.frame(x="iPF(2a)-VI", y="HCZ Year 1", H1_spline%>%filter(Xvar=="t2_f2_VI", Yvar=="hcz_t2"), H1_quartiles%>%filter(X=="t2_f2_VI", Y=="hcz_t2")%>%select(q1, q3)))
d9 <- rbind(
  data.frame(x="8,12-iso-iPF(2a)-VI", y="WAZ Year 1", H1_spline%>%filter(Xvar=="t2_f2_12i", Yvar=="waz_t2"), H1_quartiles%>%filter(X=="t2_f2_12i", Y=="waz_t2")%>%select(q1, q3)),
  data.frame(x="8,12-iso-iPF(2a)-VI", y="WLZ Year 1", H1_spline%>%filter(Xvar=="t2_f2_12i", Yvar=="whz_t2"), H1_quartiles%>%filter(X=="t2_f2_12i", Y=="whz_t2")%>%select(q1, q3)),
  data.frame(x="8,12-iso-iPF(2a)-VI", y="HCZ Year 1", H1_spline%>%filter(Xvar=="t2_f2_12i", Yvar=="hcz_t2"), H1_quartiles%>%filter(X=="t2_f2_12i", Y=="hcz_t2")%>%select(q1, q3)))
d10 <- rbind(
  data.frame(x="Combined urinary oxidative\nstress biomarkers", y="WAZ Year 1", H1_spline%>%filter(Xvar=="iso.pca", Yvar=="waz_t2"), H1_quartiles%>%filter(X=="iso.pca", Y=="waz_t2")%>%select(q1, q3)),
  data.frame(x="Combined urinary oxidative\nstress biomarkers", y="WLZ Year 1", H1_spline%>%filter(Xvar=="iso.pca", Yvar=="whz_t2"), H1_quartiles%>%filter(X=="iso.pca", Y=="whz_t2")%>%select(q1, q3)),
  data.frame(x="Combined urinary oxidative\nstress biomarkers", y="HCZ Year 1", H1_spline%>%filter(Xvar=="iso.pca", Yvar=="hcz_t2"), H1_quartiles%>%filter(X=="iso.pca", Y=="hcz_t2")%>%select(q1, q3)))


d11 <- rbind(
  data.frame(x="IPF(2a)-III", y="WAZ Year 2", H1_spline%>%filter(Xvar=="t2_f2_8ip", Yvar=="waz_t3"), H1_quartiles%>%filter(X=="t2_f2_8ip", Y=="waz_t3")%>%select(q1, q3)),
  data.frame(x="IPF(2a)-III", y="WLZ Year 2", H1_spline%>%filter(Xvar=="t2_f2_8ip", Yvar=="whz_t3"), H1_quartiles%>%filter(X=="t2_f2_8ip", Y=="whz_t3")%>%select(q1, q3)),
  data.frame(x="IPF(2a)-III", y="HCZ Year 2", H1_spline%>%filter(Xvar=="t2_f2_8ip", Yvar=="hcz_t3"), H1_quartiles%>%filter(X=="t2_f2_8ip", Y=="hcz_t3")%>%select(q1, q3)))
d12 <- rbind(
  data.frame(x="2,3-dinor-iPF(2a)-III", y="WAZ Year 2", H1_spline%>%filter(Xvar=="t2_f2_23d", Yvar=="waz_t3"), H1_quartiles%>%filter(X=="t2_f2_23d", Y=="waz_t3")%>%select(q1, q3)),
  data.frame(x="2,3-dinor-iPF(2a)-III", y="WLZ Year 2",  H1_spline%>%filter(Xvar=="t2_f2_23d", Yvar=="whz_t3"), H1_quartiles%>%filter(X=="t2_f2_23d", Y=="whz_t3")%>%select(q1, q3)),
  data.frame(x="2,3-dinor-iPF(2a)-III", y="HCZ Year 2",  H1_spline%>%filter(Xvar=="t2_f2_23d", Yvar=="hcz_t3"), H1_quartiles%>%filter(X=="t2_f2_23d", Y=="hcz_t3")%>%select(q1, q3)))
d13 <- rbind(
  data.frame(x="iPF(2a)-VI", y="WAZ Year 2", H1_spline%>%filter(Xvar=="t2_f2_VI", Yvar=="waz_t3"), H1_quartiles%>%filter(X=="t2_f2_VI", Y=="waz_t3")%>%select(q1, q3)),
  data.frame(x="iPF(2a)-VI", y="WLZ Year 2", H1_spline%>%filter(Xvar=="t2_f2_VI", Yvar=="whz_t3"), H1_quartiles%>%filter(X=="t2_f2_VI", Y=="whz_t3")%>%select(q1, q3)),
  data.frame(x="iPF(2a)-VI", y="HCZ Year 2", H1_spline%>%filter(Xvar=="t2_f2_VI", Yvar=="hcz_t3"), H1_quartiles%>%filter(X=="t2_f2_VI", Y=="hcz_t3")%>%select(q1, q3)))
d14 <- rbind(
  data.frame(x="8,12-iso-iPF(2a)-VI", y="WAZ Year 2", H1_spline%>%filter(Xvar=="t2_f2_12i", Yvar=="waz_t3"), H1_quartiles%>%filter(X=="t2_f2_12i", Y=="waz_t3")%>%select(q1, q3)),
  data.frame(x="8,12-iso-iPF(2a)-VI", y="WLZ Year 2", H1_spline%>%filter(Xvar=="t2_f2_12i", Yvar=="whz_t3"), H1_quartiles%>%filter(X=="t2_f2_12i", Y=="whz_t3")%>%select(q1, q3)),
  data.frame(x="8,12-iso-iPF(2a)-VI", y="HCZ Year 2", H1_spline%>%filter(Xvar=="t2_f2_12i", Yvar=="hcz_t3"), H1_quartiles%>%filter(X=="t2_f2_12i", Y=="hcz_t3")%>%select(q1, q3)))
d15 <- rbind(
  data.frame(x="Combined urinary oxidative\nstress biomarkers", y="WAZ Year 2", H1_spline%>%filter(Xvar=="iso.pca", Yvar=="waz_t3"), H1_quartiles%>%filter(X=="iso.pca", Y=="waz_t3")%>%select(q1, q3)),
  data.frame(x="Combined urinary oxidative\nstress biomarkers", y="WLZ Year 2", H1_spline%>%filter(Xvar=="iso.pca", Yvar=="whz_t3"), H1_quartiles%>%filter(X=="iso.pca", Y=="whz_t3")%>%select(q1, q3)),
  data.frame(x="Combined urinary oxidative\nstress biomarkers", y="HCZ Year 2", H1_spline%>%filter(Xvar=="iso.pca", Yvar=="hcz_t3"), H1_quartiles%>%filter(X=="iso.pca", Y=="hcz_t3")%>%select(q1, q3)))


d16 <- rbind(
  data.frame(x="IPF(2a)-III", y="Change in WAZ\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="t2_f2_8ip", Yvar=="delta_waz_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_8ip", Y=="delta_waz_t2_t3")%>%select(q1, q3)),
  data.frame(x="IPF(2a)-III", y="Change in WLZ\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="t2_f2_8ip", Yvar=="delta_whz_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_8ip", Y=="delta_whz_t2_t3")%>%select(q1, q3)),
  data.frame(x="IPF(2a)-III", y="Change in HCZ\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="t2_f2_8ip", Yvar=="delta_hcz_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_8ip", Y=="delta_hcz_t2_t3")%>%select(q1, q3)))
d17 <- rbind(
  data.frame(x="2,3-dinor-iPF(2a)-III", y="Change in WAZ\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="t2_f2_23d", Yvar=="delta_waz_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_23d", Y=="delta_waz_t2_t3")%>%select(q1, q3)),
  data.frame(x="2,3-dinor-iPF(2a)-III", y="Change in WLZ\nYear 1 to Year 2",  H1_spline%>%filter(Xvar=="t2_f2_23d", Yvar=="delta_whz_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_23d", Y=="delta_whz_t2_t3")%>%select(q1, q3)),
  data.frame(x="2,3-dinor-iPF(2a)-III", y="Change in HCZ\nYear 1 to Year 2",  H1_spline%>%filter(Xvar=="t2_f2_23d", Yvar=="delta_hcz_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_23d", Y=="delta_hcz_t2_t3")%>%select(q1, q3)))
d18 <- rbind(
  data.frame(x="iPF(2a)-VI", y="Change in WAZ\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="t2_f2_VI", Yvar=="delta_waz_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_VI", Y=="delta_waz_t2_t3")%>%select(q1, q3)),
  data.frame(x="iPF(2a)-VI", y="Change in WLZ\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="t2_f2_VI", Yvar=="delta_whz_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_VI", Y=="delta_whz_t2_t3")%>%select(q1, q3)),
  data.frame(x="iPF(2a)-VI", y="Change in HCZ\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="t2_f2_VI", Yvar=="delta_hcz_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_VI", Y=="delta_hcz_t2_t3")%>%select(q1, q3)))
d19 <- rbind(
  data.frame(x="8,12-iso-iPF(2a)-VI", y="Change in WAZ\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="t2_f2_12i", Yvar=="delta_waz_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_12i", Y=="delta_waz_t2_t3")%>%select(q1, q3)),
  data.frame(x="8,12-iso-iPF(2a)-VI", y="Change in WLZ\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="t2_f2_12i", Yvar=="delta_whz_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_12i", Y=="delta_whz_t2_t3")%>%select(q1, q3)),
  data.frame(x="8,12-iso-iPF(2a)-VI", y="Change in HCZ\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="t2_f2_12i", Yvar=="delta_hcz_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_12i", Y=="delta_hcz_t2_t3")%>%select(q1, q3)))
d20 <- rbind(
  data.frame(x="Combined urinary oxidative\nstress biomarkers", y="Change in WAZ\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="iso.pca", Yvar=="delta_waz_t2_t3"), H1_quartiles%>%filter(X=="iso.pca", Y=="delta_waz_t2_t3")%>%select(q1, q3)),
  data.frame(x="Combined urinary oxidative\nstress biomarkers", y="Change in WLZ\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="iso.pca", Yvar=="delta_whz_t2_t3"), H1_quartiles%>%filter(X=="iso.pca", Y=="delta_whz_t2_t3")%>%select(q1, q3)),
  data.frame(x="Combined urinary oxidative\nstress biomarkers", y="Change in HCZ\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="iso.pca", Yvar=="delta_hcz_t2_t3"), H1_quartiles%>%filter(X=="iso.pca", Y=="delta_hcz_t2_t3")%>%select(q1, q3)))


d21 <- rbind(
  data.frame(x="IPF(2a)-III", y="Weight velocity\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="t2_f2_8ip", Yvar=="wei_velocity_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_8ip", Y=="wei_velocity_t2_t3")%>%select(q1, q3)),
  data.frame(x="IPF(2a)-III", y="Head circumference velocity\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="t2_f2_8ip", Yvar=="hc_velocity_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_8ip", Y=="hc_velocity_t2_t3")%>%select(q1, q3)))
d22 <- rbind(
  data.frame(x="2,3-dinor-iPF(2a)-III", y="Weight velocity\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="t2_f2_23d", Yvar=="wei_velocity_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_23d", Y=="wei_velocity_t2_t3")%>%select(q1, q3)),
  data.frame(x="2,3-dinor-iPF(2a)-III", y="Head circumference velocity\nYear 1 to Year 2",  H1_spline%>%filter(Xvar=="t2_f2_23d", Yvar=="hc_velocity_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_23d", Y=="hc_velocity_t2_t3")%>%select(q1, q3)))
d23 <- rbind(
  data.frame(x="iPF(2a)-VI", y="Weight velocity\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="t2_f2_VI", Yvar=="wei_velocity_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_VI", Y=="wei_velocity_t2_t3")%>%select(q1, q3)),
  data.frame(x="iPF(2a)-VI", y="Head circumference velocity\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="t2_f2_VI", Yvar=="hc_velocity_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_VI", Y=="hc_velocity_t2_t3")%>%select(q1, q3)))
d24 <- rbind(
  data.frame(x="8,12-iso-iPF(2a)-VI", y="Weight velocity\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="t2_f2_12i", Yvar=="wei_velocity_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_12i", Y=="wei_velocity_t2_t3")%>%select(q1, q3)),
  data.frame(x="8,12-iso-iPF(2a)-VI", y="Head circumference velocity\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="t2_f2_12i", Yvar=="hc_velocity_t2_t3"), H1_quartiles%>%filter(X=="t2_f2_12i", Y=="hc_velocity_t2_t3")%>%select(q1, q3)))
d25 <- rbind(
  data.frame(x="Combined urinary oxidative\nstress biomarkers", y="Weight velocity\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="iso.pca", Yvar=="wei_velocity_t2_t3"), H1_quartiles%>%filter(X=="iso.pca", Y=="wei_velocity_t2_t3")%>%select(q1, q3)),
  data.frame(x="Combined urinary oxidative\nstress biomarkers", y="Head circumference velocity\nYear 1 to Year 2", H1_spline%>%filter(Xvar=="iso.pca", Yvar=="hc_velocity_t2_t3"), H1_quartiles%>%filter(X=="iso.pca", Y=="hc_velocity_t2_t3")%>%select(q1, q3)))


d26 <- rbind(
  data.frame(x="Change in slope between\npre- and post-stress change in cortisol", y="LAZ Year 2", H2_spline%>%filter(Xvar=="t3_cort_slope", Yvar=="laz_t3"), H2_quartiles%>%filter(X=="t3_cort_slope", Y=="laz_t3")%>%select(q1, q3)))
d27 <- rbind(
  data.frame(x="Cortisol residualized gain score", y="LAZ Year 2", H2_spline%>%filter(Xvar=="t3_residual_cort", Yvar=="laz_t3"), H2_quartiles%>%filter(X=="t3_residual_cort", Y=="laz_t3")%>%select(q1, q3)))
d28 <- rbind(
  data.frame(x="Change in slope between\npre- and post-stress change in sAA", y="LAZ Year 2", H2_spline%>%filter(Xvar=="t3_saa_slope", Yvar=="laz_t3"), H2_quartiles%>%filter(X=="t3_saa_slope", Y=="laz_t3")%>%select(q1, q3)))
d29 <- rbind(
  data.frame(x="sAA residualized gain score", y="LAZ Year 2", H2_spline%>%filter(Xvar=="t3_residual_saa", Yvar=="laz_t3"), H2_quartiles%>%filter(X=="t3_residual_saa", Y=="laz_t3")%>%select(q1, q3)))


d30 <- rbind(
  data.frame(x="Change in slope between pre-\nand post-stress change in cortisol", y="WAZ Year 2", H2_spline%>%filter(Xvar=="t3_cort_slope", Yvar=="waz_t3"), H2_quartiles%>%filter(X=="t3_cort_slope", Y=="waz_t3")%>%select(q1, q3)),
  data.frame(x="Change in slope between pre-\nand post-stress change in cortisol", y="WLZ Year 2", H2_spline%>%filter(Xvar=="t3_cort_slope", Yvar=="whz_t3"), H2_quartiles%>%filter(X=="t3_cort_slope", Y=="whz_t3")%>%select(q1, q3)),
  data.frame(x="Change in slope between pre-\nand post-stress change in cortisol", y="HCZ Year 2", H2_spline%>%filter(Xvar=="t3_cort_slope", Yvar=="hcz_t3"), H2_quartiles%>%filter(X=="t3_cort_slope", Y=="hcz_t3")%>%select(q1, q3)))
d31 <- rbind(
  data.frame(x="Cortisol residualized gain score", y="WAZ Year 2", H2_spline%>%filter(Xvar=="t3_residual_cort", Yvar=="waz_t3"), H2_quartiles%>%filter(X=="t3_residual_cort", Y=="waz_t3")%>%select(q1, q3)),
  data.frame(x="Cortisol residualized gain score", y="WLZ Year 2", H2_spline%>%filter(Xvar=="t3_residual_cort", Yvar=="whz_t3"), H2_quartiles%>%filter(X=="t3_residual_cort", Y=="whz_t3")%>%select(q1, q3)),
  data.frame(x="Cortisol residualized gain score", y="HCZ Year 2", H2_spline%>%filter(Xvar=="t3_residual_cort", Yvar=="hcz_t3"), H2_quartiles%>%filter(X=="t3_residual_cort", Y=="hcz_t3")%>%select(q1, q3)))
d32 <- rbind(
  data.frame(x="Change in slope between pre-\nand post-stress change in sAA", y="WAZ Year 2", H2_spline%>%filter(Xvar=="t3_saa_slope", Yvar=="waz_t3"), H2_quartiles%>%filter(X=="t3_saa_slope", Y=="waz_t3")%>%select(q1, q3)),
  data.frame(x="Change in slope between pre-\nand post-stress change in sAA", y="WLZ Year 2", H2_spline%>%filter(Xvar=="t3_saa_slope", Yvar=="whz_t3"), H2_quartiles%>%filter(X=="t3_saa_slope", Y=="whz_t3")%>%select(q1, q3)),
  data.frame(x="Change in slope between pre-\nand post-stress change in sAA", y="HCZ Year 2", H2_spline%>%filter(Xvar=="t3_saa_slope", Yvar=="hcz_t3"), H2_quartiles%>%filter(X=="t3_saa_slope", Y=="hcz_t3")%>%select(q1, q3)))
d33 <- rbind(
  data.frame(x="sAA residualized gain score", y="WAZ Year 2", H2_spline%>%filter(Xvar=="t3_residual_saa", Yvar=="waz_t3"), H2_quartiles%>%filter(X=="t3_residual_saa", Y=="waz_t3")%>%select(q1, q3)),
  data.frame(x="sAA residualized gain score", y="WLZ Year 2", H2_spline%>%filter(Xvar=="t3_residual_saa", Yvar=="whz_t3"), H2_quartiles%>%filter(X=="t3_residual_saa", Y=="whz_t3")%>%select(q1, q3)),
  data.frame(x="sAA residualized gain score", y="HCZ Year 2", H2_spline%>%filter(Xvar=="t3_residual_saa", Yvar=="hcz_t3"), H2_quartiles%>%filter(X=="t3_residual_saa", Y=="hcz_t3")%>%select(q1, q3)))


d34 <- rbind(
  data.frame(x="Mean arterial pressure", y="LAZ Year 2", H3_spline%>%filter(Xvar=="t3_map", Yvar=="laz_t3"), H3_quartiles%>%filter(X=="t3_map", Y=="laz_t3")%>%select(q1, q3)))
d35 <- rbind(
  data.frame(x="Mean resting heart rate", y="LAZ Year 2", H3_spline%>%filter(Xvar=="t3_hr_mean", Yvar=="laz_t3"), H3_quartiles%>%filter(X=="t3_hr_mean", Y=="laz_t3")%>%select(q1, q3)))


d36 <- rbind(
  data.frame(x="Mean arterial pressure", y="WAZ Year 2", H3_spline%>%filter(Xvar=="t3_map", Yvar=="waz_t3"), H3_quartiles%>%filter(X=="t3_map", Y=="waz_t3")%>%select(q1, q3)),
  data.frame(x="Mean arterial pressure", y="WLZ Year 2", H3_spline%>%filter(Xvar=="t3_map", Yvar=="whz_t3"), H3_quartiles%>%filter(X=="t3_map", Y=="whz_t3")%>%select(q1, q3)),
  data.frame(x="Mean arterial pressure", y="HCZ Year 2", H3_spline%>%filter(Xvar=="t3_map", Yvar=="hcz_t3"), H3_quartiles%>%filter(X=="t3_map", Y=="hcz_t3")%>%select(q1, q3)))
d37 <- rbind(
  data.frame(x="Mean resting heart rate", y="WAZ Year 2", H3_spline%>%filter(Xvar=="t3_hr_mean", Yvar=="waz_t3"), H3_quartiles%>%filter(X=="t3_hr_mean", Y=="waz_t3")%>%select(q1, q3)),
  data.frame(x="Mean resting heart rate", y="WLZ Year 2", H3_spline%>%filter(Xvar=="t3_hr_mean", Yvar=="whz_t3"), H3_quartiles%>%filter(X=="t3_hr_mean", Y=="whz_t3")%>%select(q1, q3)),
  data.frame(x="Mean resting heart rate", y="HCZ Year 2", H3_spline%>%filter(Xvar=="t3_hr_mean", Yvar=="hcz_t3"), H3_quartiles%>%filter(X=="t3_hr_mean", Y=="hcz_t3")%>%select(q1, q3)))


d38 <- rbind(
  data.frame(x="Entire promoter region\n(39 assayed CpG sites)", y="LAZ Year 2", H4_spline%>%filter(Xvar=="t3_gcr_mean", Yvar=="laz_t3"), H4_quartiles%>%filter(X=="t3_gcr_mean", Y=="laz_t3")%>%select(q1, q3)))
d39 <- rbind(
  data.frame(x="NGFI-A transcription factor\nbinding site (CpG site #12)", y="LAZ Year 2", H4_spline%>%filter(Xvar=="t3_gcr_cpg12", Yvar=="laz_t3"), H4_quartiles%>%filter(X=="t3_gcr_cpg12", Y=="laz_t3")%>%select(q1, q3)))


d40 <- rbind(
  data.frame(x="Entire promoter region\n(39 assayed CpG sites)", y="WAZ Year 2", H4_spline%>%filter(Xvar=="t3_gcr_mean", Yvar=="waz_t3"), H4_quartiles%>%filter(X=="t3_gcr_mean", Y=="waz_t3")%>%select(q1, q3)),
  data.frame(x="Entire promoter region\n(39 assayed CpG sites)", y="WLZ Year 2", H4_spline%>%filter(Xvar=="t3_gcr_mean", Yvar=="whz_t3"), H4_quartiles%>%filter(X=="t3_gcr_mean", Y=="whz_t3")%>%select(q1, q3)),
  data.frame(x="Entire promoter region\n(39 assayed CpG sites)", y="HCZ Year 2", H4_spline%>%filter(Xvar=="t3_gcr_mean", Yvar=="hcz_t3"), H4_quartiles%>%filter(X=="t3_gcr_mean", Y=="hcz_t3")%>%select(q1, q3)))
d41 <- rbind(
  data.frame(x="NGFI-A transcription factor\nbinding site (CpG site #12)", y="WAZ Year 2", H4_spline%>%filter(Xvar=="t3_gcr_cpg12", Yvar=="waz_t3"), H4_quartiles%>%filter(X=="t3_gcr_cpg12", Y=="waz_t3")%>%select(q1, q3)),
  data.frame(x="NGFI-A transcription factor\nbinding site (CpG site #12)", y="WLZ Year 2", H4_spline%>%filter(Xvar=="t3_gcr_cpg12", Yvar=="whz_t3"), H4_quartiles%>%filter(X=="t3_gcr_cpg12", Y=="whz_t3")%>%select(q1, q3)),
  data.frame(x="NGFI-A transcription factor\nbinding site (CpG site #12)", y="HCZ Year 2", H4_spline%>%filter(Xvar=="t3_gcr_cpg12", Yvar=="hcz_t3"), H4_quartiles%>%filter(X=="t3_gcr_cpg12", Y=="hcz_t3")%>%select(q1, q3)))
  

d1$y <- factor(d1$y)
d2$y <- factor(d2$y)
d3$y <- factor(d3$y)
d4$y <- factor(d4$y)
d5$y <- factor(d5$y)
d6$y <- factor(d6$y)
d7$y <- factor(d7$y)
d8$y <- factor(d8$y)
d9$y <- factor(d9$y)
d10$y <- factor(d10$y)
d11$y <- factor(d11$y)
d12$y <- factor(d12$y)
d13$y <- factor(d13$y)
d14$y <- factor(d14$y)
d15$y <- factor(d15$y)
d16$y <- factor(d16$y)
d17$y <- factor(d17$y)
d18$y <- factor(d18$y)
d19$y <- factor(d19$y)
d20$y <- factor(d20$y)
d21$y <- factor(d21$y)
d22$y <- factor(d22$y)
d23$y <- factor(d23$y)
d24$y <- factor(d24$y)
d25$y <- factor(d25$y)
d26$y <- factor(d26$y)
d27$y <- factor(d27$y)
d28$y <- factor(d28$y)
d29$y <- factor(d29$y)
d30$y <- factor(d30$y)
d31$y <- factor(d31$y)
d32$y <- factor(d32$y)
d33$y <- factor(d33$y)
d34$y <- factor(d34$y)
d35$y <- factor(d35$y)
d36$y <- factor(d36$y)
d37$y <- factor(d37$y)
d38$y <- factor(d38$y)
d39$y <- factor(d39$y)
d40$y <- factor(d40$y)
d41$y <- factor(d41$y)


#spline plot function
spline_plot_functions <- function(d){
  
  color_levels = c("LAZ Year 1", "WAZ Year 1", "WLZ Year 1", "HCZ Year 1",
                   "LAZ Year 2", "WAZ Year 2", "WLZ Year 2", "HCZ Year 2",
                   "Change in LAZ\nYear 1 to Year 2", "Change in WAZ\nYear 1 to Year 2", "Change in WLZ\nYear 1 to Year 2", "Change in HCZ\nYear 1 to Year 2",
                   "Length velocity\nYear 1 and Year 2", "Weight velocity\nYear 1 to Year 2", "Head circumference velocity\nYear 1 to Year 2")

  nlevels <- length(levels(d$y))

  quantiles <- d %>% group_by(y) %>%
    summarize(
      x.lb=as.numeric(quantile(X, probs = seq(0, 1, 0.05))[2]),
      x.ub=as.numeric(quantile(X, probs = seq(0, 1, 0.05))[20]),
      y.lb=as.numeric(quantile(Y, probs = seq(0, 1, 0.05))[2]),
      y.ub=as.numeric(quantile(Y, probs = seq(0, 1, 0.05))[20])
    )
  
  d <- left_join(d, quantiles, by="y")
  p1 <- d[d$y==levels(d$y)[1],] %>% {ggplot(.,aes(x = X)) +
      geom_smooth(aes(y = fit, color=y), se = F) +
      geom_rug(aes(x=q1), sides="b", length = unit(0.15, "npc"), size=1, color="grey30") +
      geom_rug(aes(x=q3), sides="b", length = unit(0.15, "npc"), size=1, color="grey30") +
      geom_point(aes(y=Y), alpha=0.5) +
      geom_ribbon(aes(ymin=lwrS, ymax=uprS, fill=y, color=y), alpha=0.5) +
      coord_cartesian(xlim = c(.$x.lb[1], .$x.ub[1]), ylim = c(.$y.lb[1], .$y.ub[1])) +
      scale_colour_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) + 
      scale_fill_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) + 
      xlab(.$x[1]) + ylab(.$y[1])
  }
  if(nlevels>1){
  p2 <- d[d$y==levels(d$y)[2],] %>% {ggplot(.,aes(x = X)) +
      geom_smooth(aes(y = fit, color=y), se = F) +
      geom_ribbon(aes(ymin=lwrS, ymax=uprS, fill=y, color=y), alpha=0.5) +
      geom_rug(aes(x=q1), sides="b", length = unit(0.15, "npc"), size=1, color="grey30") +
      geom_rug(aes(x=q3), sides="b", length = unit(0.15, "npc"), size=1, color="grey30") +
      geom_point(aes(y=Y), alpha=0.5) +
      coord_cartesian(xlim = c(.$x.lb[1], .$x.ub[1]), ylim = c(.$y.lb[1], .$y.ub[1])) +
      scale_colour_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) +
      scale_fill_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) +
      xlab(.$x[1]) + ylab(.$y[1])
  }
  }
  if(nlevels>2){
    p3 <- d[d$y==levels(d$y)[3],] %>% {ggplot(.,aes(x = X)) +
        geom_smooth(aes(y = fit, color=y), se = F) +
        geom_rug(aes(x=q1), sides="b", length = unit(0.15, "npc"), size=1, color="grey30") +
        geom_rug(aes(x=q3), sides="b", length = unit(0.15, "npc"), size=1, color="grey30") +
        geom_ribbon(aes(ymin=lwrS, ymax=uprS, fill=y, color=y), alpha=0.5) +
        geom_point(aes(y=Y), alpha=0.5) +
        coord_cartesian(xlim = c(.$x.lb[1], .$x.ub[1]), ylim = c(.$y.lb[1], .$y.ub[1])) +
        scale_colour_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) +
        scale_fill_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) +
        xlab(.$x[1]) + ylab(.$y[1])
    }
    if(nlevels==4){
      p4 <- d[d$y==levels(d$y)[4],] %>% {ggplot(.,aes(x = X)) +
          geom_smooth(aes(y = fit, color=y), se = F) +
          geom_point(aes(y=Y), alpha=0.5) +
          geom_rug(aes(x=q1), sides="b", length = unit(0.15, "npc"), size=1, color="grey30") +
          geom_rug(aes(x=q3), sides="b", length = unit(0.15, "npc"), size=1, color="grey30") +
          geom_ribbon(aes(ymin=lwrS, ymax=uprS, fill=y, color=y), alpha=0.5) +
          coord_cartesian(xlim = c(.$x.lb[1], .$x.ub[1]), ylim = c(.$y.lb[1], .$y.ub[1])) +
          scale_colour_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) +
          scale_fill_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) +
          xlab(.$x[1]) + ylab(.$y[1])
      }
    }
  }

  if(nlevels==2){
    return(list(p1, p2))
  } else if (nlevels==3){
    return(list(p1, p2, p3))
  } else if (nlevels==4) {
    return (list(p1, p2, p3, p4))
  }
  return (list(p1))
}

plist1 <- spline_plot_functions(d1)
plist2 <- spline_plot_functions(d2)
plist3 <- spline_plot_functions(d3)
plist4 <- spline_plot_functions(d4)
plist5 <- spline_plot_functions(d5)
plist6 <- spline_plot_functions(d6)
plist7 <- spline_plot_functions(d7)
plist8 <- spline_plot_functions(d8)
plist9 <- spline_plot_functions(d9)
plist10 <- spline_plot_functions(d10)
plist11 <- spline_plot_functions(d11)
plist12 <- spline_plot_functions(d12)
plist13 <- spline_plot_functions(d13)
plist14 <- spline_plot_functions(d14)
plist15 <- spline_plot_functions(d15)
plist16 <- spline_plot_functions(d16)
plist17 <- spline_plot_functions(d17)
plist18 <- spline_plot_functions(d18)
plist19 <- spline_plot_functions(d19)
plist20 <- spline_plot_functions(d20)
plist21 <- spline_plot_functions(d21)
plist22 <- spline_plot_functions(d22)
plist23 <- spline_plot_functions(d23)
plist24 <- spline_plot_functions(d24)
plist25 <- spline_plot_functions(d25)
plist26 <- spline_plot_functions(d26)
plist27 <- spline_plot_functions(d27)
plist28 <- spline_plot_functions(d28)
plist29 <- spline_plot_functions(d29)
plist30 <- spline_plot_functions(d30)
plist31 <- spline_plot_functions(d31)
plist32 <- spline_plot_functions(d32)
plist33 <- spline_plot_functions(d33)
plist34 <- spline_plot_functions(d34)
plist35 <- spline_plot_functions(d35)
plist36 <- spline_plot_functions(d36)
plist37 <- spline_plot_functions(d37)
plist38 <- spline_plot_functions(d38)
plist39 <- spline_plot_functions(d39)
plist40 <- spline_plot_functions(d40)
plist41 <- spline_plot_functions(d41)

p1 <- plot_grid(plist1[[1]], plist1[[2]], plist1[[3]], plist1[[4]], ncol=1, labels=c("",""))
p2 <- plot_grid(plist2[[1]], plist2[[2]], plist2[[3]], plist2[[4]], ncol=1, labels=c("",""))
p3 <- plot_grid(plist3[[1]], plist3[[2]], plist3[[3]], plist3[[4]], ncol=1, labels=c("",""))
p4 <- plot_grid(plist4[[1]], plist4[[2]], plist4[[3]], plist4[[4]], ncol=1, labels=c("",""))
p5 <- plot_grid(plist5[[1]], plist5[[2]], plist5[[3]], plist5[[4]], ncol=1, labels=c("",""))

p6 <- plot_grid(plist6[[1]], plist6[[2]], plist6[[3]], ncol=1, labels=c("",""))
p7 <- plot_grid(plist7[[1]], plist7[[2]], plist7[[3]], ncol=1, labels=c("",""))
p8 <- plot_grid(plist8[[1]], plist8[[2]], plist8[[3]], ncol=1, labels=c("",""))
p9 <- plot_grid(plist9[[1]], plist9[[2]], plist9[[3]], ncol=1, labels=c("",""))
p10 <- plot_grid(plist10[[1]], plist10[[2]], plist10[[3]], ncol=1, labels=c("",""))

p11 <- plot_grid(plist11[[1]], plist11[[2]], plist11[[3]], ncol=1, labels=c("",""))
p12 <- plot_grid(plist12[[1]], plist12[[2]], plist12[[3]], ncol=1, labels=c("",""))
p13 <- plot_grid(plist13[[1]], plist13[[2]], plist13[[3]], ncol=1, labels=c("",""))
p14 <- plot_grid(plist14[[1]], plist14[[2]], plist14[[3]], ncol=1, labels=c("",""))
p15 <- plot_grid(plist15[[1]], plist15[[2]], plist15[[3]], ncol=1, labels=c("",""))

p16 <- plot_grid(plist16[[1]], plist16[[2]], plist16[[3]], ncol=1, labels=c("",""))
p17 <- plot_grid(plist17[[1]], plist17[[2]], plist17[[3]], ncol=1, labels=c("",""))
p18 <- plot_grid(plist18[[1]], plist18[[2]], plist18[[3]], ncol=1, labels=c("",""))
p19 <- plot_grid(plist19[[1]], plist19[[2]], plist19[[3]], ncol=1, labels=c("",""))
p20 <- plot_grid(plist20[[1]], plist20[[2]], plist20[[3]], ncol=1, labels=c("",""))

p21 <- plot_grid(plist21[[1]], plist21[[2]], ncol=1, labels=c("",""))
p22 <- plot_grid(plist22[[1]], plist22[[2]], ncol=1, labels=c("",""))
p23 <- plot_grid(plist23[[1]], plist23[[2]], ncol=1, labels=c("",""))
p24 <- plot_grid(plist24[[1]], plist24[[2]], ncol=1, labels=c("",""))
p25 <- plot_grid(plist25[[1]], plist25[[2]], ncol=1, labels=c("",""))

p26 <- plot_grid(plist26[[1]], ncol=1, labels=c("",""))
p27 <- plot_grid(plist27[[1]], ncol=1, labels=c("",""))
p28 <- plot_grid(plist28[[1]], ncol=1, labels=c("",""))
p29 <- plot_grid(plist29[[1]], ncol=1, labels=c("",""))

p30 <- plot_grid(plist30[[1]], plist30[[2]], plist30[[3]], ncol=1, labels=c("",""))
p31 <- plot_grid(plist31[[1]], plist31[[2]], plist31[[3]], ncol=1, labels=c("",""))
p32 <- plot_grid(plist32[[1]], plist32[[2]], plist32[[3]], ncol=1, labels=c("",""))
p33 <- plot_grid(plist33[[1]], plist33[[2]], plist33[[3]], ncol=1, labels=c("",""))

p34 <- plot_grid(plist34[[1]], ncol=1, labels=c("",""))
p35 <- plot_grid(plist35[[1]], ncol=1, labels=c("",""))

p36 <- plot_grid(plist36[[1]], plist36[[2]], plist36[[3]], ncol=1, labels=c("",""))
p37 <- plot_grid(plist37[[1]], plist37[[2]], plist37[[3]], ncol=1, labels=c("",""))

p38 <- plot_grid(plist38[[1]], ncol=1, labels=c("",""))
p39 <- plot_grid(plist39[[1]], ncol=1, labels=c("",""))

p40 <- plot_grid(plist40[[1]], plist40[[2]], plist40[[3]], ncol=1, labels=c("",""))
p41 <- plot_grid(plist41[[1]], plist41[[2]], plist41[[3]], ncol=1, labels=c("",""))

#splines for urinary stress biomarkers v. measures of growth
pcomb1 <- plot_grid(p1, p2, p3, p4, p5, ncol=5, labels = c("","",""), hjust=0.5, vjust=0.5)
pcomb2 <- plot_grid(p6, p7, p8, p9, p10, ncol=5, labels = c("","","",""), hjust=0.5,vjust=0.5)
pcomb3 <- plot_grid(p11, p12, p13, p14, p15, ncol=5, labels = c("","","",""), hjust=0.5,vjust=0.5)
pcomb4 <- plot_grid(p16, p17, p18, p19, p20, ncol=5, labels = c("","","",""), hjust=0.5,vjust=0.5)
pcomb5 <- plot_grid(p21, p22, p23, p24, p25, ncol=5, labels = c("","","",""), hjust=0.5,vjust=0.5)

#splines for cortisol and saa v. LAZ, WAZ, WLZ, WHZ at Year 2
pcomb6 <- plot_grid(p26, p27, p28, p29, ncol=4, labels=c("","","",""), hjust=0.5,vjust=0.5)
pcomb7 <- plot_grid(p30, p31, p32, p33, ncol=4, labels=c("","","",""), hjust=0.5,vjust=0.5)

#splines for mean arterial pressure and heart rate v. LAZ, WAZ, WLZ, WHZ at Year 2
pcomb8 <- plot_grid(p34, p35, ncol=2, labels=c("","","",""), hjust=0.5,vjust=0.5)
pcomb9 <- plot_grid(p36, p37, ncol=2, labels=c("","","",""), hjust=0.5,vjust=0.5)

#splines for methylation v. LAZ, WAZ, WLZ, WHZ at Year 2
pcomb10 <- plot_grid(p38, p39, ncol=2, labels=c("","","",""), hjust=0.5,vjust=0.5)
pcomb11 <- plot_grid(p40, p41, ncol=2, labels=c("","","",""), hjust=0.5,vjust=0.5)

ggsave(pcomb1, file = here("figures/main/stress-growth-figure1.tiff"), height=12, width=14)
ggsave(pcomb2, file = here("figures/supplementary/stress-growth-suppfigure1a.tiff"), height=9, width=14)
ggsave(pcomb3, file = here("figures/supplementary/stress-growth-suppfigure1b.tiff"), height=9, width=14)
ggsave(pcomb4, file = here("figures/supplementary/stress-growth-suppfigure1c.tiff"), height=9, width=14)
ggsave(pcomb5, file = here("figures/supplementary/stress-growth-suppfigure1d.tiff"), height=7, width=14)
ggsave(pcomb6, file = here("figures/main/stress-growth-figure2.tiff"), height=6, width=14)
ggsave(pcomb7, file = here("figures/supplementary/stress-growth-suppfigure2.tiff"), height=10, width=14)
ggsave(pcomb8, file = here("figures/main/stress-growth-figure3.tiff"), height=9, width=14)
ggsave(pcomb9, file = here("figures/supplementary/stress-growth-suppfigure3.tiff"), height=14, width=12)
ggsave(pcomb10, file = here("figures/main/stress-growth-figure4.tiff"), height=9, width=14)
ggsave(pcomb11, file = here("figures/supplementary/stress-growth-suppfigure4.tiff"), height=14, width=12)
