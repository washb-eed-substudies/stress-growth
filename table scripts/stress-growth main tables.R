rm(list=ls())

source(here::here("0-config.R"))

# load enrollment characteristics and results
d <- read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-stress-growth-covariates-stresslab-anthro.csv"))
H1 <- readRDS(here('results/unadjusted/H1_res.RDS'))
H2 <- readRDS(here('results/unadjusted/H2_res.RDS'))
H3 <- readRDS(here('results/unadjusted/H3_res.RDS'))
H4 <- readRDS(here('results/unadjusted/H4_res.RDS'))
H1adj <- readRDS(here('results/adjusted/H1_adj_res.RDS'))
H2adj <- readRDS(here('results/adjusted/H2_adj_res.RDS'))
H3adj <- readRDS(here('results/adjusted/H3_adj_res.RDS'))
H4adj <- readRDS(here('results/adjusted/H4_adj_res.RDS'))


#### Table 1 ####
# Characteristics of participants
nperc <- function(vector){
  n <- sum(vector==1, na.rm=T)
  perc <- round(n/sum(!is.na(vector))*100)
  paste(n, " (", perc, "%)", sep="")
}

mediqr <- function(vector){
  quantiles <- round(quantile(vector, na.rm=T), 2)
  paste(quantiles[3], " (", quantiles[2], ", ", quantiles[4], ")", sep="")
}

n_med_col <- c(nperc(d$sex), mediqr(d$t2_f2_8ip), mediqr(d$t2_f2_23d), mediqr(d$t2_f2_VI), mediqr(d$t2_f2_12i),
               mediqr(d$t3_cort_slope), mediqr(d$t3_residual_cort), mediqr(d$t3_saa_slope), mediqr(d$t3_residual_saa),
               mediqr(d$t3_map), mediqr(d$t3_hr_mean), mediqr(d$t3_gcr_mean), mediqr(d$t3_gcr_cpg12),
               mediqr(d$laz_t2), mediqr(d$waz_t2), mediqr(d$whz_t2), mediqr(d$hcz_t2),
               mediqr(d$laz_t3), mediqr(d$waz_t3), mediqr(d$whz_t3), mediqr(d$hcz_t3),
               nperc(d$diar7d_t2), nperc(d$diar7d_t3), mediqr(d$momage), mediqr(d$momheight), 
               mediqr(d$momeduy), mediqr(d$cesd_sum_t2), mediqr(d$cesd_sum_ee_t3), mediqr(d$pss_sum_mom_t3), 
               nperc(d$life_viol_any_t3))

tbl1 <- data.table(" " = c("Child","","","","","","","","","","","","","","","","","","","","","","","Mother","","","","","",""),
                   " " = c("", "Urinary F2-isoprostanes (Year 1)","","","", "Salivary cortisol reactivity (Year 2)","", "sAA reactivity (Year 2)","",
                           "SAM biomarkers (Year 2)","", "Glucocorticoid receptor","", "Anthropometry (14 months, Year 1)","","","",
                           "Anthropometry (28 months, Year 2)","","","", "Diarrhea (14 months, Year 1)", "Diarrhea (28 months, Year 2)","",
                           "Anthropometry at enrollment", "Education", "Depression at Year 1", "Depression at Year 2", "Perceived stress at Year 2", 
                           "Intimate partner violence"),
                   " " = c("Female", "iPF(2a)-III", "2,3-dinor-iPF(2a)-III", "iPF(2a-VI", "8,12-iso-iPF(2a)-VI", 
                           "Change in slope between pre- and post-stressor cortisol", "Cortisol residualized gain score", 
                           "Change in slope between pre- and post-stressor sAA change", "sAA residualized gain score",
                           "Mean arterial pressure", "Resting heart rate", "NR3C1 exon 1F promoter methylation", "NGFI-A transcription factor binding site methylation",
                           "Length-for-age Z score", "Weight-for-age Z score", "Weight-for-length Z score", "Head circumference-for-age Z score",
                           "Length-for-age Z score", "Weight-for-age Z score", "Weight-for-length Z score", "Head circumference-for-age Z score",
                           "Caregiver-reported 7-day recall", "Caregiver-reported 7-day recall", "Age (years)", "Height (cm)", "Schooling completed (years)",
                           "CES-D score", "CES-D score", "Perceived Stress Scale score", "Any lifetime exposure"),
                   "n (%) or median (IQR)" = n_med_col)

write.csv(tbl1, file=here("tables/main/stress-growth-table1.csv"))


#### Functions for tables 2-4 ####
growth_tbl <- function(exposure, outcome, results, results_adj){
  tbl <- data.table(" " = character(), " " = character(), " " = character(), " " = character(),
                    " Outcome, Q3 v. Q1" = character(), " " = character(), " " = character(), " " = character())
  tbl <- rbind(tbl, list(" ", " ", " ", " ", "Unadjusted", " ", "Fully adjusted", " "))
  tbl <- rbind(tbl, list(" ", "Outcome", "Q1 Mean", "Q3 Mean", "Coefficient (95% CI)", "P-value", "Coefficient (95% CI)", "P-value"))
  for (exp in exposure) {
    for (out in outcome) {
      filtered_res <- results[results$Y==out & results$X==exp,]
      filtered_adj <- results_adj[results_adj$Y==out & results_adj$X==exp,]
      unadj <- paste(round(filtered_res$`point.diff`, 2), " (", round(filtered_res$`lb.diff`, 2), ", ", round(filtered_res$`ub.diff`, 2), ")", sep="")
      adj <- paste(round(filtered_adj$`point.diff`, 2), " (", round(filtered_adj$`lb.diff`, 2), ", ", round(filtered_adj$`ub.diff`, 2), ")", sep="")
      tbl <- rbind(tbl, list(exp, out, round(filtered_res$q1, 2), round(filtered_res$q3, 2), unadj, round(filtered_res$Pval, 2), adj, round(filtered_adj$Pval, 2)))
    }
  }
  tbl
}


#### Table 2 ####

exposure <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i")
outcome <- c("laz_t2", "laz_t3", "len_velocity_t2_t3", "delta_laz_t2_t3")
tbl2 <- growth_tbl(exposure, outcome, H1, H1adj)

tbl2[, 1] <- c(" ", "Urinary oxidative stress biomarker", "IPF(2a)-III", "", "", "", 
               "2,3-dinor-iPF(2a)-III", "", "", "",
               "iPF(2a)-VI", "", "", "", 
               "8,12-iso-iPF(2a)-VI", "", "", "")

tbl2[, 2] <- c("", "Outcome", "LAZ, Year 1", "LAZ, Year 2", "Length velocity Year 1 to Year 2", "Change in LAZ Year 1 to Year 2",
               "LAZ, Year 1", "LAZ, Year 2", "Length velocity Year 1 to Year 2", "Change in LAZ Year 1 to Year 2",
               "LAZ, Year 1", "LAZ, Year 2", "Length velocity Year 1 to Year 2", "Change in LAZ Year 1 to Year 2",
               "LAZ, Year 1", "LAZ, Year 2", "Length velocity Year 1 to Year 2", "Change in LAZ Year 1 to Year 2")

write.csv(tbl2, here('tables/main/stress-growth-table2.csv'))

#### Table 3 ####



#### Table 4 ####



#### Table 5 ####