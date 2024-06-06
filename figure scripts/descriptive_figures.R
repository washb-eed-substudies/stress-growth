


rm(list=ls())
library(tidyverse)

d <- readRDS("C:/Users/andre/Documents//EE/eed-substudy-data/bangladesh-cleaned-master-data.RDS")


Xvars <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_iso_pca")            
Yvars <- c("laz_t2", "waz_t2", "whz_t2" ,"hcz_t2", 
           "len_velocity_t2_t3", "wei_velocity_t2_t3", "hc_velocity_t2_t3",
           "laz_t3", "waz_t3", "whz_t3", "hcz_t3",
           "delta_laz_t2_t3", "delta_waz_t2_t3", "delta_whz_t2_t3", "delta_hcz_t2_t3")

#boxplots by wasting/stunting status
d <- d %>% mutate(stunt2=ifelse(laz_t2< (-2), "Stunted","Not stunted"),
                  wast2=ifelse(laz_t2< (-2), "Wasted","Not wasted"))
d$laz_t2_cat <- factor(ntile(d$laz_t2,3))

ggplot(d, aes(y=t2_f2_23d, x=stunt2)) + geom_violin(draw_quantiles=c(0.25,.5,.75))
ggplot(d, aes(y=t2_f2_8ip, x=stunt2)) + geom_boxplot()

ggplot(d, aes(y=t2_f2_VI_raw, x=laz_t2)) + geom_point(alpha=0.1) + geom_smooth()
ggplot(d, aes(y=t2_f2_VI_raw, x=laz_t2)) + geom_point(alpha=0.1) + geom_smooth() + coord_cartesian(xlim=c(-3,0), ylim=c(0,50))


ggplot(d, aes(y=t2_f2_VI, x=laz_t2_cat)) + geom_violin(draw_quantiles=c(0.25,.5,.75))
ggplot(d, aes(y=t2_f2_VI, x=laz_t2_cat)) + geom_boxplot()

# d <- d %>% mutate(laz_t2_cat = case_when(
#   laz_t2 < (-3) ~ "Sev. stunted",
#   laz_t2 < (-2) ~ "Stunted",
#   laz_t2 < (0) ~ "Below median",
#   laz_t2 >= (0) ~ "Above median"),
#   waz_t2_cat = case_when(
#     waz_t2 < (-3) ~ "Sev. underweight",
#     waz_t2 < (-2) ~ "Underweight",
#     waz_t2 < (0) ~ "Below median",
#     waz_t2 >= (0) ~ "Above median"),
#   whz_t2_cat = case_when(
#     whz_t2 < (-3) ~ "Sev. wasted",
#     whz_t2 < (-2) ~ "Wasted",
#     whz_t2 < (0) ~ "Below median",
#     whz_t2 >= (0) ~ "Above median"),
#   hcz_t2_cat = case_when(
#     hcz_t2 < (-3) ~ "Sev. low HC",
#     hcz_t2 < (-2) ~ "Low HC",
#     hcz_t2 < (0) ~ "Below median",
#     hcz_t2 >= (0) ~ "Above median"),
#   laz_t2_cat = factor(laz_t2_cat, levels=c("Sev. stunted","Stunted","Below median","Above median")),
#   waz_t2_cat = factor(waz_t2_cat, levels=c("Sev. underweight","Underweight","Below median","Above median")),
#   whz_t2_cat = factor(whz_t2_cat, levels=c("Sev. wasted","Wasted","Below median","Above median")),
#   hcz_t2_cat = factor(hcz_t2_cat, levels=c("Sev. low HC","Low HC","Below median","Above median")))

plotdf <- d %>% mutate(laz_t2_cat = case_when(
  laz_t2 < (-3) ~ "< -3",
  laz_t2 < (-2) ~ "< -2",
  laz_t2 < (0) ~ "[-2, 0)",
  laz_t2 >= (0) ~ ">0"),
  waz_t2_cat = case_when(
    waz_t2 < (-3) ~ "< -3",
    waz_t2 < (-2) ~ "< -2",
    waz_t2 < (0) ~ "[-2, 0)",
    waz_t2 >= (0) ~ ">0"),
  whz_t2_cat = case_when(
    whz_t2 < (-3) ~ "< -3",
    whz_t2 < (-2) ~ "< -2",
    whz_t2 < (0) ~ "[-2, 0)",
    whz_t2 >= (0) ~ ">0"),
  hcz_t2_cat = case_when(
    hcz_t2 < (-3) ~ "< -3",
    hcz_t2 < (-2) ~ "< -2",
    hcz_t2 < (0) ~ "[-2, 0)",
    hcz_t2 >= (0) ~ ">0"),
  laz_t2_cat = factor(laz_t2_cat, levels=c("< -3","< -2","[-2, 0)",">0")),
  waz_t2_cat = factor(waz_t2_cat, levels=c("< -3","< -2","[-2, 0)",">0")),
  whz_t2_cat = factor(whz_t2_cat, levels=c("< -3","< -2","[-2, 0)",">0")),
  hcz_t2_cat = factor(hcz_t2_cat, levels=c("< -3","< -2","[-2, 0)",">0"))) %>%
  select(laz_t2_cat, waz_t2_cat, whz_t2_cat, hcz_t2_cat, t2_f2_8ip, t2_f2_23d, t2_f2_VI, t2_f2_12i)

head(plotdf)  

plotdf <- plotdf %>% 
  pivot_longer(-c(t2_f2_8ip, t2_f2_23d, t2_f2_VI, t2_f2_12i), names_to='measure', values_to = "growth_cat") %>%
  pivot_longer(-c(measure, growth_cat), names_to='isoprostane', values_to = "log_value") %>%
  mutate(measure  = gsub('_t2_cat','',measure), isoprostane = gsub('t2_','', isoprostane)) %>%
  filter(!is.na(growth_cat))

#TEMP- maybe free scales
plotdf$log_value <- scale(plotdf$log_value)



#ggplot(d, aes(y=t2_f2_VI_raw, x=laz_t2_cat)) + geom_violin(draw_quantiles=c(0.25,.5,.75))
p <- ggplot(plotdf, aes(y=log_value, x=growth_cat)) + 
  geom_boxplot() + facet_grid(measure~isoprostane) +
  labs(y="log - isoprostane concentration", x="Z-score category") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

ggsave(p, file = here("figures/main/stress-boxplots.tiff"), height=12, width=14)
saveRDS(p, file = "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/results/stress-growth-models/figure-objects/boxplot_objects.RDS")




