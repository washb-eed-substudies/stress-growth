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
  data.frame(x="IPF(2a)-III", y="LAZ Year 2", H1_spline%>%filter(Xvar=="t2_f2_8ip", Yvar=="laz_t3"), H1_quartiles%>%filter(X=="t2_f2_8ip", Y=="laz_t3")%>%select(q1, q3)))
d2 <- rbind(
  data.frame(x="2,3-dinor-iPF(2a)-III", y="LAZ Year 1", H1_spline%>%filter(Xvar=="t2_f2_23d", Yvar=="laz_t2"), H1_quartiles%>%filter(X=="t2_f2_23d", Y=="laz_t2")%>%select(q1, q3)),
  data.frame(x="2,3-dinor-iPF(2a)-III", y="LAZ Year 2",  H1_spline%>%filter(Xvar=="t2_f2_23d", Yvar=="laz_t3"), H1_quartiles%>%filter(X=="t2_f2_23d", Y=="laz_t3")%>%select(q1, q3)))
d3 <- rbind(
  data.frame(x="iPF(2a)-VI", y="LAZ Year 1", H1_spline%>%filter(Xvar=="t2_f2_VI", Yvar=="laz_t2"), H1_quartiles%>%filter(X=="t2_f2_VI", Y=="laz_t2")%>%select(q1, q3)),
  data.frame(x="iPF(2a)-VI", y="LAZ Year 2", H1_spline%>%filter(Xvar=="t2_f2_VI", Yvar=="laz_t3"), H1_quartiles%>%filter(X=="t2_f2_VI", Y=="laz_t3")%>%select(q1, q3)))
d4 <- rbind(
  data.frame(x="8,12-iso-iPF(2a)-VI", y="LAZ Year 1", H1_spline%>%filter(Xvar=="t2_f2_12i", Yvar=="laz_t2"), H1_quartiles%>%filter(X=="t2_f2_12i", Y=="laz_t2")%>%select(q1, q3)),
  data.frame(x="8,12-iso-iPF(2a)-VI", y="LAZ Year 2", H1_spline%>%filter(Xvar=="t2_f2_12i", Yvar=="laz_t3"), H1_quartiles%>%filter(X=="t2_f2_12i", Y=="laz_t3")%>%select(q1, q3)))

d5 <- rbind(
  data.frame(x="Change in slope between\npre- and post-stress change in cortisol", y="LAZ Year 2", H2_spline%>%filter(Xvar=="t3_cort_slope", Yvar=="laz_t3"), H2_quartiles%>%filter(X=="t3_cort_slope", Y=="laz_t3")%>%select(q1, q3)))
d6 <- rbind(
  data.frame(x="Cortisol residualized gain score", y="LAZ Year 2", H2_spline%>%filter(Xvar=="t3_residual_cort", Yvar=="laz_t3"), H2_quartiles%>%filter(X=="t3_residual_cort", Y=="laz_t3")%>%select(q1, q3)))
d7 <- rbind(
  data.frame(x="Change in slope between\npre- and post-stress change in sAA", y="LAZ Year 2", H2_spline%>%filter(Xvar=="t3_saa_slope", Yvar=="laz_t3"), H2_quartiles%>%filter(X=="t3_saa_slope", Y=="laz_t3")%>%select(q1, q3)))
d8 <- rbind(
  data.frame(x="sAA residualized gain score", y="LAZ Year 2", H2_spline%>%filter(Xvar=="t3_residual_saa", Yvar=="laz_t3"), H2_quartiles%>%filter(X=="t3_residual_saa", Y=="laz_t3")%>%select(q1, q3)))

d9 <- rbind(
  data.frame(x="Mean arterial pressure", y="LAZ Year 2", H3_spline%>%filter(Xvar=="t3_map", Yvar=="laz_t3"), H3_quartiles%>%filter(X=="t3_map", Y=="laz_t3")%>%select(q1, q3)))
d10 <- rbind(
  data.frame(x="Mean resting heart rate", y="LAZ Year 2", H3_spline%>%filter(Xvar=="t3_hr_mean", Yvar=="laz_t3"), H3_quartiles%>%filter(X=="t3_hr_mean", Y=="laz_t3")%>%select(q1, q3)))

d11 <- rbind(
  data.frame(x="Entire promoter region\n(39 assayed CpG sites)", y="LAZ Year 2", H4_spline%>%filter(Xvar=="t3_gcr_mean", Yvar=="laz_t3"), H4_quartiles%>%filter(X=="t3_gcr_mean", Y=="laz_t3")%>%select(q1, q3)))
d12 <- rbind(
  data.frame(x="NGFI-A transcription factor\nbinding site (CpG site #12)", y="LAZ Year 2", H4_spline%>%filter(Xvar=="t3_gcr_cpg12", Yvar=="laz_t3"), H4_quartiles%>%filter(X=="t3_gcr_cpg12", Y=="laz_t3")%>%select(q1, q3)))

  
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



#spline plot function
spline_plot_functions <- function(d){
  
  # color_levels = c("Change in LAZ\nbetween years 1 and 2", "Change in WLZ\nbetween years 1 and 2", "Change in WAZ\nbetween years 1 and 2", "Change in HCZ\nbetween years 1 and 2",  
  #                  "LAZ - year 1", "WLZ - year 1","WAZ - year 1", "HCZ - year 1" ,
  #                  "LAZ - year 2", "WLZ - year 2","WAZ - year 2", "HCZ - year 2" ,
  #                  "Length velocity (cm/mo)\nbetween years 1 and 2", "Weight velocity (kg/mo)\nbetween years 1 and 2", "Head circumference velocity (cm/mo)\nbetween years 1 and 2")
  color_levels <- c("LAZ Year 1", "LAZ Year 2")
  
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
      geom_ribbon(aes(ymin=lwrS, ymax=uprS, fill=y, color=y), alpha=0.5) +
      geom_point(aes(y=Y), alpha=0.5) +
      coord_cartesian(xlim = c(.$x.lb[1], .$x.ub[1]), ylim = c(.$y.lb[1], .$y.ub[1])) +
      scale_colour_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) + 
      scale_fill_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) + 
      xlab(.$x[1]) + ylab(.$y[1])
  }
  p2 <- d[d$y==levels(d$y)[2],] %>% {ggplot(.,aes(x = X)) +
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
  # p3 <- d[d$y==levels(d$y)[3],] %>% {ggplot(.,aes(x = X)) +
  #     geom_smooth(aes(y = fit, color=y), se = F) +
  #     geom_ribbon(aes(ymin=lwrS, ymax=uprS, fill=y, color=y), alpha=0.5) +
  #     geom_rug(aes(x=Q1), sides="b", length = unit(0.15, "npc"), size=1, color="grey30") +
  #     geom_rug(aes(x=Q2), sides="b", length = unit(0.15, "npc"), size=1, color="grey30") +
  #     geom_rug(aes(x=Q3), sides="b", length = unit(0.15, "npc"), size=1, color="grey30") +
  #     geom_point(aes(y=Y), alpha=0.5) +
  #     coord_cartesian(xlim = c(.$x.lb, .$x.ub), ylim = c(.$y.lb, .$y.ub)) +
  #     scale_colour_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) + 
  #     scale_fill_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) + 
  #     xlab(.$x[1]) + ylab(.$y[1])
  # }
  # if(nlevels==4){
  #   p4 <- d[d$y==levels(d$y)[4],] %>% {ggplot(.,aes(x = X)) +
  #       geom_smooth(aes(y = fit, color=y), se = F) +
  #       geom_ribbon(aes(ymin=lwrS, ymax=uprS, fill=y, color=y), alpha=0.5) +
  #       geom_point(aes(y=Y), alpha=0.5) +
  #       geom_rug(aes(x=Q1), sides="b", length = unit(0.15, "npc"), size=1, color="grey30") +
  #       geom_rug(aes(x=Q2), sides="b", length = unit(0.15, "npc"), size=1, color="grey30") +
  #       geom_rug(aes(x=Q3), sides="b", length = unit(0.15, "npc"), size=1, color="grey30") +
  #       coord_cartesian(xlim = c(.$x.lb, .$x.ub), ylim = c(.$y.lb, .$y.ub)) +
  #       scale_colour_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) + 
  #       scale_fill_manual(values=tableau10[c(1:4,1:4,1:4,5:7)], drop=TRUE, limits=color_levels) + 
  #       xlab(.$x[1]) + ylab(.$y[1])
  #   }    
  # }
  
  if(nlevels==2){
    #p <- p1 + p2 + p3 + p4 + plot_layout(nrow = 1)
    return(list(p1, p2))
  }else{
    #p <- p1 + p2 + p3 + plot_layout(nrow = 1) 
    return(list(p1))
  }
  
  return(p)
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

p1 <- plot_grid(plist1[[1]], plist1[[2]], ncol=1, labels=c("",""))
p2 <- plot_grid(plist2[[1]], plist2[[2]], ncol=1, labels=c("",""))
p3 <- plot_grid(plist3[[1]], plist3[[2]], ncol=1, labels=c("",""))
p4 <- plot_grid(plist4[[1]], plist4[[2]], ncol=1, labels=c("",""))

# p1 <- plot_grid(plist1[[1]], plist1[[2]], plist1[[3]], plist1[[4]], nrow=1, labels = c("","","",""))
# p2 <- plot_grid(plist2[[1]], plist2[[2]], plist2[[3]], nrow=1, labels = c("","",""))
# p3 <- plot_grid(plist3[[1]], plist3[[2]], plist3[[3]], plist3[[4]], nrow=1, labels = c("","","",""))
# p4 <- plot_grid(plist4[[1]], plist4[[2]], plist4[[3]], plist4[[4]], nrow=1, labels = c("","","",""))
# p5 <- plot_grid(plist5[[1]], plist5[[2]], plist5[[3]], plist5[[4]], nrow=1, 
#                 labels = c("Adjusted differences between quartiles of telomere length at Year 2 for each growth outcome","","",""),
#                 hjust=1,vjust=1)
# p6 <- plot_grid(plist6[[1]], plist6[[2]], plist6[[3]], plist6[[4]], nrow=1, labels = c("","","",""))
# p7 <- plot_grid(plist7[[1]], plist7[[2]], plist7[[3]], nrow=1, labels = c("","",""))
# p8 <- plot_grid(plist8[[1]], plist8[[2]], plist8[[3]], plist8[[4]], nrow=1, labels = c("","","",""))



#Adjusted differences between quartiles of urinary stress biomarkers for LAZ Year 1 and 2
pcomb1 <- plot_grid(p1,
                    p2,
                    p3,
                    p4,
                    nrow=2,
                    ncol=4,
                    labels = c("","",""),
                    hjust=0.5, vjust=0.5,
                    rel_heights = c(1, 1, 1))
#Adjusted differences between quartiles of sAA and cortisol for LAZ
pcomb2 <- plot_grid(plist5[[1]], plist6[[1]], plist7[[1]], plist8[[1]],
                    ncol=2, nrow=2,
                    labels = c("","","",""),
                    hjust=0.5,vjust=0.5,
                    rel_heights = c(1, 1, 1, 1))
#Adjusted differences between quartiles of MAP and heartrate for LAZ
pcomb3 <- plot_grid(plist9[[1]], plist10[[1]],
                    ncol=2,
                    labels = c("",""),
                    hjust=0.5,vjust=0.5,
                    rel_heights = c(1, 1, 1, 1))
#Adjusted differences between quartiles of methylation sites for LAZ
pcomb4 <- plot_grid(plist11[[1]], plist12[[1]],
                    ncol=2,
                    labels = c("",""),
                    hjust=0.5,vjust=0.5,
                    rel_heights = c(1, 1, 1, 1))


ggsave(pcomb1, file = here("figures/stress-growth-figure1.tiff"), height=10, width=14)
ggsave(pcomb2, file = here("figures/stress-growth-figure2.tiff"), height=12, width=12)
ggsave(pcomb3, file = here("figures/stress-growth-figure3.tiff"), height=9, width=14)
ggsave(pcomb4, file = here("figures/stress-growth-figure4.tiff"), height=9, width=14)

# ggplot(d,aes(x = X)) +
#   geom_smooth(aes(y = fit, color=y), se = F) +
#   geom_ribbon(aes(ymin=lwrS, ymax=uprS, fill=y, color=y), alpha=0.5) +
#   geom_rug(aes(y=Y)) +
#   facet_wrap(~y)


