
rm(list=ls())
source(here::here("0-config.R"))
library(cowplot)

#colors
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

#Adjusted
H1adj <- readRDS(here('results/adjusted/H1_adj_res_clean.RDS'))
H2adj <- readRDS(here('results/adjusted/H2_adj_res_clean.RDS'))
H3adj <- readRDS(here('results/adjusted/H3_adj_res_clean.RDS'))
H4adj <- readRDS(here('results/adjusted/H4_adj_res_clean.RDS'))

exposure <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_iso_pca","t3_cort_z01", "t3_cort_z03", "t3_cort_slope", "t3_residual_cort", 
              "t3_saa_z01", "t3_saa_z02", "t3_saa_slope", "t3_residual_saa","t3_map", "t3_hr_mean","t3_gcr_mean", "t3_gcr_cpg12")
outcome <- c("laz_t2","laz_t3","len_velocity_t2_t3","delta_laz_t2_t3")
expo_var <- c("IPF(2a)-III", "2,3-dinor-iPF(2a)-III", "iPF(2a)-VI", "8,12-iso-iPF(2a)-VI", "Combined urinary oxidative stress biomarkers",
              "Pre-stressor cortisol", "Post-stressor cortisol", "Pre to post-stress change in cortisol", "Cortisol residualized gain score", 
              "Pre-stressor sAA", "Post-stressor sAA", "Pre to post-stress change in sAA", "sAA residualized gain score",
              "Mean arterial pressure", "Mean resting heart rate","Entire promoter region (39 assayed CpG sites)", "NGFI-A transcription factor binding site (CpG site #12)")
out_var <- c("LAZ Year 1", "LAZ Year 2","Length velocity (cm/month) Year 1 to Year 2", "Change in LAZ from Year 1 to Year 2")

plotdf <- bind_rows(H1adj,H2adj,H3adj,H4adj) %>% filter(Y %in% outcome) %>% clean_res()

levels(plotdf$group)
plotdf$group <- factor( plotdf$group,
                        levels = c("Oxidative stress (Year 1)",
                                   "Hypothalamic-pituitary-adrenal axis (Year 2)", 
                                   "Sympathetic adrenomedullary axis (Year 2)"),
                        labels = c("Oxidative stress (Year 1)",
                                     "Hypothalamic-pituitary-adrenal\naxis (Year 2)", 
                                     "Sympathetic adrenomedullary\naxis (Year 2)"))



#
unique(plotdf$X)
plotdf$X <- factor(plotdf$X,
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




###Adapted from WASH-Stress
library(ggtext)

unique(plotdf$group)

ylimits = c(-0.51, 0.22)
p1 <- ggplot(plotdf %>% filter(Y!="LAZ -Year 2"), 
             (aes(x=X, y=point.diff, color=Y))) + 
  geom_point(size=3, position = position_dodge(.5)) +
  geom_errorbar(aes(ymin=lb.diff, ymax=ub.diff),
                width = 0.75, size = 1, position = position_dodge(.5)) +
  ylim(ylimits) +
  geom_hline(yintercept = 0, linetype="dashed") +
  scale_color_manual(values = tableau10[1:3]) +
  coord_flip() +
  labs(y = "Adjusted mean difference", x = "Child stress measure") +
  ggtitle("") + theme_ki() + 
  theme(legend.position = "right", 
        axis.text.y=element_text(hjust=0.95,vjust=0.2)) +
  guides(color = guide_legend("Growth Outcome", nrow = 3)) 
p1


ylimits2 = c(-0.2, 0.25)
p2 <- ggplot(plotdf %>% filter(Y=="LAZ -Year 2"), 
             (aes(x=X, y=point.diff, color=group))) + 
  geom_point(size=3) +
  geom_errorbar(aes(ymin=lb.diff, ymax=ub.diff),
                width = 0.75, size = 1) +
  ylim(ylimits2) +
  geom_hline(yintercept = 0, linetype="dashed") +
  scale_color_manual(values = tableau10[c(5,9,10)]) +
  #facet_wrap(~Y, ncol=1, scales="free") +
  coord_flip() +
  labs(y = "LAZ adjusted mean difference", x = "Child stress measure") +
  ggtitle("") + theme_ki() + theme(legend.position = "right",
                                   axis.text.y=element_text(hjust=0.95,vjust=0.2)) +
  guides(color = guide_legend("Stress axis", nrow = 3)) 
  
p2

p <- plot_grid(p1,p2, labels = c("A", "B"), ncol = 1, rel_heights = c(1, 2))
ggsave(p, file=paste0(here::here(),"/figures/forest_plot.jpeg"), height=8, width=8)
saveRDS(p, file=paste0(here::here(),"/figures/main/forest_plot.RDS"))


#color by group
#Make one panel with isoprostanes with a geom_dodge for outcomes, another with the rest
