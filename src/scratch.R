

H1_plot_data <- readRDS(paste0(dropboxDir,"results/stress-growth-models/figure-data/H1_adj_spline_data.RDS"))
H2_plot_data <- readRDS(paste0(dropboxDir,"results/stress-growth-models/figure-data/H2_adj_spline_data.RDS"))
H3_plot_data <- readRDS(paste0(dropboxDir,"results/stress-growth-models/figure-data/H3_adj_spline_data.RDS"))
H4_plot_data <- readRDS(paste0(dropboxDir,"results/stress-growth-models/figure-data/H4_adj_spline_data.RDS"))


H1_plot_data_sub <- H1_plot_data %>% filter(Xvar=="t2_f2_8ip", Yvar=="laz_t2")
gam_diff <- H1_res %>% filter(X=="t2_f2_8ip", Y=="laz_t2")
p <- ggplot(H1_plot_data_sub) + 
  geom_ribbon(aes(x = X, ymin = lwrS, ymax = uprS), 
      alpha = 0.5) + 
  geom_vline(aes(xintercept=q1), data=gam_diff) +
  geom_vline(aes(xintercept=q3), data=gam_diff) +
  geom_path(aes(x = X, y = fit), color = "black") + 
  xlab("f2_8ip") + ylab("LAZ") + 
  #ggtitle("Concurrent association")+ 
  ggtitle(paste0(round(gam_diff$point.diff, 2), " (", 
                 round(gam_diff$lb.diff, 2), ", ", round(gam_diff$ub.diff, 
                                                         2), ")"))
p


p <- ggplot(H1_plot_data_sub) + 
  geom_point(aes(x = X, y = Y), alpha=0.2) +
  geom_ribbon(aes(x = X, ymin = lwrS, ymax = uprS), alpha = 0.5) + 
  geom_path(aes(x = X, y = fit), color = "black", size=1.5) + 
  geom_vline(aes(xintercept=q1), data=gam_diff) +
  geom_vline(aes(xintercept=q3), data=gam_diff) +
  xlab("") + ylab("") + 
  ggtitle("")
p

laz_t2  t2_f2_8ip 
laz_t2  t2_f2_23d
laz_t2   t2_f2_VI
laz_t2 t2_iso_pca 

H1_plot_list <- readRDS(paste0(dropboxDir,"results/stress-growth-models/figure-objects/H1_adj_splines.RDS"))
# H2_plot_list <- readRDS(paste0(dropboxDir,"results/stress-growth-models/figure-objects/H2_adj_splines.RDS"))
# H3_plot_list <- readRDS(paste0(dropboxDir,"results/stress-growth-models/figure-objects/H3_adj_splines.RDS"))
# H4_plot_list <- readRDS(paste0(dropboxDir,"results/stress-growth-models/figure-objects/H4_adj_splines.RDS"))

H1_adj_models <- readRDS(paste0(dropboxDir,"results/stress-growth-models/models/adj_H4_models.RDS"))
i<-1
simul_plot <- gam_simul_CI(H1_adj_models$fit[i][[1]], H1_adj_models$dat[i][[1]], xlab=H1_adj_models$X[i], ylab=H1_adj_models$Y[i], title="", gam_diff=H1_res[i,])
simul_plot$p

H1_res <- readRDS(paste0(here::here(),"/results/adjusted/H1_adj_res.RDS"))
H2_res <- readRDS(paste0(here::here(),"/results/adjusted/H2_adj_res.RDS"))
H3_res <- readRDS(paste0(here::here(),"/results/adjusted/H3_adj_res.RDS"))
H4_res <- readRDS(paste0(here::here(),"/results/adjusted/H4_adj_res.RDS"))


H1_res[H1_res$BH.Pval < 0.05,]
H2_res[H2_res$BH.Pval < 0.05,]
H3_res[H3_res$BH.Pval < 0.05,]
H4_res[H4_res$BH.Pval < 0.05,]

H1_res$Xtime <- "t2"
H1_res <- H1_res %>% mutate(
  Ytime = case_when(
    grepl("_t2_t3",H1_res$Y) ~ "delta",
    grepl("_t2",H1_res$Y) ~ "t2",
    grepl("_t3",H1_res$Y) ~ "t3"),
Ytype = case_when(
  grepl("laz",H1_res$Y) ~ "LAZ",
  grepl("waz",H1_res$Y) ~ "WAZ",
  grepl("whz",H1_res$Y) ~ "WHZ",
  grepl("hcz",H1_res$Y) ~ "HCZ"))
H1_res$sig.cat <- ifelse(H1_res$BH.Pval < 0.05, "*", "")

plotdf <- H1_res %>% filter(Ytype=="LAZ") %>% droplevels() %>%
  mutate(Y=factor(Y, levels=c("laz_t2","laz_t3","delta_laz_t2_t3")))
p <- ggplot(data = plotdf, (aes(x=X, y=point.diff#, 
                                #group=sample_cat, color=sample_cat
                                ))) + 
  geom_point(size=3, position = position_dodge(0.5), alpha=0.75) +
  geom_errorbar(aes(ymin=lb.diff, ymax=ub.diff), position = position_dodge(0.5),
                width = 0.3, size = 1) +
  facet_wrap(~Y,  scales="free_x") +
  geom_text(aes(label=sig.cat), color="black", position = position_dodge(0.5), vjust = -0.1) +
  coord_flip() +
  # scale_color_manual(breaks = legend_labels,
  #                    values = colours, drop = FALSE) +
  geom_hline(yintercept = 0, linetype="dashed") +
  #labs(color="Sample type") +
  xlab("Stress biomarker") + ylab("Mean difference: 25th vs. 75th percentile") + 
  theme_ki() + 
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        strip.placement = "outside",
        strip.text.x = element_text(size=10, face = "bold"),
        strip.text.y = element_text(size=10, angle = 270, face = "bold"),          plot.title = element_text(hjust = 0.5, face = "plain", size=9),
        panel.spacing = unit(0, "lines")) 
p



tbl2 <- read.csv(here('tables/main/stress-growth-table2.csv'))

tbl2 <- tbl2[,-c(1,4:8)]
colnames(tbl2)[c(3,4)] <- c("Coefficient (95% CI)", "P-value")
tbl2 <- tbl2 %>% filter(Outcome!="",Outcome!=" ", grepl("\\*",`P-value`))



H1_plot_list
