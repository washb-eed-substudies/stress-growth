
rm(list=ls())
library(tidyverse)

H1_res <- readRDS(here("results/adjusted/H1_adj_res.RDS"))
H2_res <- readRDS(here("results/adjusted/H2_adj_res.RDS"))
H3_res <- readRDS(here("results/adjusted/H3_adj_res.RDS"))
H4_res <- readRDS(here("results/adjusted/H4_adj_res.RDS"))

res <- bind_rows(H1_res,H2_res,H3_res,H4_res) %>% mutate(dataset="growth-adj")
res_sense <- readRDS(here("results/adjusted/adj_res_sens.RDS"))  %>% mutate(dataset="no growth")
res <- bind_rows(res, res_sense)

table(res$Y)
res <- res %>% filter(grepl("delta_",Y)|grepl("velocity_",Y))

table(res$dataset)
head(res)

ggplot(res, aes(x=Y, y=point.diff, group=dataset, color=dataset)) + 
  geom_point(position = position_dodge(0.5)) +
  geom_linerange(aes(ymin=lb.diff,ymax=ub.diff), position = position_dodge(0.5)) + coord_flip() +
  facet_wrap(~X) + geom_hline(yintercept = 0) + theme(legend.position = "bottom")