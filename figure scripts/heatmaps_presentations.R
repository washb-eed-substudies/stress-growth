


rm(list=ls())

source(here::here("0-config.R"))



main_res <- bind_rows(readRDS(paste0(here::here(),"/results/adjusted/H1_adj_res_clean.RDS")),
          readRDS(paste0(here::here(),"/results/adjusted/H2_adj_res_clean.RDS")),
          readRDS(paste0(here::here(),"/results/adjusted/H3_adj_res_clean.RDS")),
          readRDS(paste0(here::here(),"/results/adjusted/H4_adj_res_clean.RDS"))) %>%
          clean_res()

table(main_res$group)
table(main_res$X, is.na(main_res$group))

unadj_res <- bind_rows(readRDS(paste0(here::here(),"/results/unadjusted/H1_res_clean.RDS")),
                      readRDS(paste0(here::here(),"/results/unadjusted/H2_res_clean.RDS")),
                      readRDS(paste0(here::here(),"/results/unadjusted/H3_res_clean.RDS")),
                      readRDS(paste0(here::here(),"/results/unadjusted/H4_res_clean.RDS"))) %>%
                      clean_res()


sens_res <- bind_rows(readRDS(paste0(here::here(),"/results/sensitivity/H1_adj_res_noWgrowth.RDS"))%>% mutate(BH.Pval=p.adjust(Pval, method="BH")),
                       readRDS(paste0(here::here(),"/results/sensitivity/H2_adj_res_noWgrowth.RDS"))%>% mutate(BH.Pval=p.adjust(Pval, method="BH")),
                       readRDS(paste0(here::here(),"/results/sensitivity/H3_adj_res_noWgrowth.RDS"))%>% mutate(BH.Pval=p.adjust(Pval, method="BH")),
                       readRDS(paste0(here::here(),"/results/sensitivity/H4_adj_res_noWgrowth.RDS"))%>% mutate(BH.Pval=p.adjust(Pval, method="BH"))) %>%
                      clean_res()

glm_res <- bind_rows(readRDS(paste0(here::here(),"/results/sensitivity/H1_glm_res.RDS"))%>% mutate(BH.Pval=p.adjust(Pval, method="BH")),
                      readRDS(paste0(here::here(),"/results/sensitivity/H2_glm_res.RDS"))%>% mutate(BH.Pval=p.adjust(Pval, method="BH")),
                      readRDS(paste0(here::here(),"/results/sensitivity/H3_glm_res.RDS"))%>% mutate(BH.Pval=p.adjust(Pval, method="BH")),
                      readRDS(paste0(here::here(),"/results/sensitivity/H4_glm_res.RDS"))%>% mutate(BH.Pval=p.adjust(Pval, method="BH"))) %>%
                      clean_res()

growth_res <- bind_rows(readRDS(paste0(here::here(),"/results/sensitivity/H1_growth_exp_res.RDS"))%>% mutate(BH.Pval=p.adjust(Pval, method="BH")),
                     readRDS(paste0(here::here(),"/results/sensitivity/H2_4_growth_exp_res.RDS"))%>% mutate(BH.Pval=p.adjust(Pval, method="BH"))) 


#saveRDS(H2_growth_exp_res, here("results/sensitivity/H2_4_growth_exp_res.RDS"))


#consider either combining all h2-h4, or combine all time 2 and include t1 and velocity in plot 1

# pval_var = "Pval"
# title = ""
# Outcome = "Growth Outcome"
# Exposure = "Stress Exposure"
# print.est = T
# print.ci = F
# null = 0


plot_sig_heatmap <- function (d, pval_var = "Pval", title = "", Outcome = "Growth Outcome", 
          Exposure = "Stress Exposure", print.est = T, print.ci = F, null = 0){
  require(RColorBrewer)
  colnames(d)[colnames(d) == pval_var] <- "pval"
  dfull <- expand_grid(unique(d$Y), unique(d$X))
  colnames(dfull) <- c("Y", "X")
  d <- left_join(dfull, d, by = c("Y", "X"))
  d <- distinct(d)
  if (null == 0) {
    d$sign <- sign(d$point.diff)
  }else{
    d$sign <- ifelse(d$point.diff > 1, 1, -1)
  }
  d$pval_cat <- cut(d$pval, breaks = c(-1, 0.01, 0.05, 0.2, 2), labels = c("<0.01", "<0.05", "0.05-0.2", "0.2-1"))
  d$pval_cat <- ifelse(d$sign == 1, paste0(d$pval_cat, " increase"), 
                       paste0(d$pval_cat, " decrease"))
  d$pval_cat[d$pval_cat %in% c("0.2-1 decrease", "0.2-1 increase")] <- "0.2-1"
  table(d$pval_cat)
  d$pval_cat <- factor(d$pval_cat, levels = c("<0.01 decrease", 
                                              "<0.05 decrease", "0.05-0.2 decrease", 
                                              "0.2-1", "0.05-0.2 increase",  "<0.05 increase", "<0.01 increase"))
  d$pval_cat <- addNA(d$pval_cat)
  levels(d$pval_cat) = c(levels(d$pval_cat), "Not estimated")
  d$pval_cat[is.na(d$pval_cat)] <- "Not estimated"
  table(d$pval_cat)
  table(is.na(d$pval_cat))
  d$est = ifelse(d$BH.Pval < 0.05, "*", "")
  d$est = ifelse(d$BH.Pval < 0.01, "**", d$est)
  d$est = ifelse(d$BH.Pval < 0.001, "***", d$est)
  if (print.est) {
    d$est = paste0(round(d$point.diff, 2),d$est)
    if (print.ci) {
      d$est = paste0(round(d$est, 2), " (", round(d$lb.diff, 
                                                  2), ", ", round(d$ub.diff, 2), ")")
    }
  }
  d$est = gsub("NA \\(NA, NA\\)", "", d$est)
  textcol = "grey20"
  cols = rev(brewer.pal(n = 9, name = "Spectral"))
  colours <- c("<0.01 decrease" = cols[1], 
               "<0.05 decrease" = cols[2], 
               "0.05-0.2 decrease" = cols[3], 
               "0.2-1" = cols[5],  
               "0.05-0.2 increase" = cols[7], 
               "<0.05 increase" = cols[8], 
               "<0.01 increase" = cols[9], 
               "Not estimated" = "gray80")

  #dummy category so all categories show up in legend
  dummy_data <- data.frame(
    Y=d$Y[1:8],
    X_f=d$X_f[1:8],
    pval_cat = c("<0.01 decrease",
                 "<0.05 decrease",
                 "0.05-0.2 decrease",
                 "0.2-1",
                 "0.05-0.2 increase",
                 "<0.05 increase",
                 "<0.01 increase",
                 "Not estimated"))

  hm <- ggplot(d, aes(x = Y, y = X_f, fill = pval_cat)) + 
    geom_tile(data=dummy_data) + 
    geom_tile(colour = "grey80", size = 0.25) + 
    #scale_x_discrete(expand = c(0, 0), limits = rev(levels(d$X))) + 
    scale_x_discrete(expand = c(0, 0)) + 
    scale_y_discrete(expand = c(0, 0)) + theme_minimal(base_size = 10) + 
    scale_fill_manual(name="P-value category",
                      limits =   c("<0.01 decrease",
                                   "<0.05 decrease",
                                   "0.05-0.2 decrease",
                                   "0.2-1",
                                   "0.05-0.2 increase",
                                   "<0.05 increase",
                                   "<0.01 increase",
                                   "Not estimated"),
                      values = colours) +
    geom_text(aes(label = est)) + 
    theme(#aspect.ratio = 1, 
          legend.title = element_text(color = textcol, size = 8),
          legend.position="bottom", 
          legend.margin = margin(grid::unit(0.1, "cm")), 
          legend.text = element_text(colour = textcol, "cm"), 
          legend.key.width = grid::unit(1, "cm"),
          axis.text.x = element_text(size = 8, colour = textcol, angle=35, hjust=1, vjust=1), 
          axis.text.y = element_text(size = 8, vjust = 0.2, 
                                     colour = textcol), axis.ticks = element_line(size = 0.4), 
          plot.title = element_text(colour = textcol, hjust = 0, 
                                    size = 12, face = "bold"), strip.text.x = element_text(size = 10), 
          strip.text.y = element_text(angle = 0, size = 10), 
          plot.background = element_blank(), panel.border = element_blank(), 
          strip.background = element_blank(), panel.background = element_rect(fill = "grey80", 
                                                                              colour = "grey80"), panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) + 
      guides(fill = guide_legend("P-value strength", nrow = 2, title.position = "top")) +
      labs(x = Outcome, y = Exposure, title = title)
  hm
  return(hm)
}


p_main <- plot_sig_heatmap(main_res)
p_main_t2 <- plot_sig_heatmap(main_res %>% filter(group=="Oxidative stress (Year 1)"))
p_main_t2 <- p_main_t2 + guides(fill = guide_legend("P-value strength", nrow = 1, title.position = "top"))

p_main_t3 <- plot_sig_heatmap(main_res %>% filter(group!="Oxidative stress (Year 1)"))
p_main_t3 <- p_main_t3 + theme(legend.position="none", 
                               axis.text.x = element_text(size = 8, colour = "grey20", angle=0, hjust=0.5, vjust=1), 
                               axis.text.y = element_text(vjust=0.5))
p_main_t3

#save png with high dpi
ggsave(p_main_t2, file=paste0(here::here(),"/figures/heatmap_main_presentation.png"), height=4, width=10, dpi=600)

ggsave(p_main_t2, file=paste0(here::here(),"/figures/heatmap_main_t2_presentation.jpeg"), height=4, width=10)
ggsave(p_main_t3, file=paste0(here::here(),"/figures/heatmap_main_t3_presentation.jpeg"), height=3, width=6)
