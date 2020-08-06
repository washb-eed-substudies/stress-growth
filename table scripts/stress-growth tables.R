rm(list=ls())

library('flextable')
library('officer')
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


#### Functions for growth tables ####
growth_tbl <- function(name, expo_var, out_var, exposure, outcome, results, results_adj){
  ### name: string name of group of exposures
  ### expo_var: vector of string exposures to include in table
  ### out_var: vector of string outcomes to include in table
  ### exposure: vector of string exposure variable names
  ### outcome: vector of string outcome variable names
  ### results: data frame with unadjusted results
  ### results_adj: data fram with adjusted results
  
  ### this function produces a table that can be saved as a csv
  
  tbl <- data.table(" " = character(), " " = character(), " " = character(), " " = character(),
                    " Outcome, Q3 v. Q1" = character(), " " = character(), " " = character(), " " = character())
  tbl <- rbind(tbl, list(" ", " ", " ", " ", "Unadjusted", " ", "Fully adjusted", " "))
  tbl <- rbind(tbl, list(" ", "Outcome", "Q1 Mean", "Q3 Mean", "Coefficient (95% CI)", "P-value", "Coefficient (95% CI)", "P-value"))
  for (i in 1:length(exposure)) {
    for (j in 1:length(outcome)) {
      exp <- exposure[i]
      out <- outcome[j]
      filtered_res <- results[results$Y==out & results$X==exp,]
      filtered_adj <- results_adj[results_adj$Y==out & results_adj$X==exp,]
      unadj <- paste(round(filtered_res$`point.diff`, 2), " (", round(filtered_res$`lb.diff`, 2), ", ", round(filtered_res$`ub.diff`, 2), ")", sep="")
      adj <- paste(round(filtered_adj$`point.diff`, 2), " (", round(filtered_adj$`lb.diff`, 2), ", ", round(filtered_adj$`ub.diff`, 2), ")", sep="")
      if (j==1){
        tbl <- rbind(tbl, list(expo_var[i], out_var[j], round(filtered_res$q1, 2), round(filtered_res$q3, 2), unadj, round(filtered_res$Pval, 2), adj, round(filtered_adj$Pval, 2)))
      }else {
        tbl <- rbind(tbl, list("", out_var[j], round(filtered_res$q1, 2), round(filtered_res$q3, 2), unadj, round(filtered_res$Pval, 2), adj, round(filtered_adj$Pval, 2)))
      }
    }
    if (i != length(exposure)) {
      tbl <- rbind(tbl, list("","","","","","","",""))
    }
  }
  tbl
}

growth_tbl_flex <- function(name, expo_var, out_var, exposure, outcome, results, results_adj){
  ### name: string name of group of exposures
  ### expo_var: vector of string exposures to include in table
  ### out_var: vector of string outcomes to include in table
  ### exposure: vector of string exposure variable names
  ### outcome: vector of string outcome variable names
  ### results: data frame with unadjusted results
  ### results_adj: data fram with adjusted results
  
  ### this function produces a table that can be saved as an image or 
  ### directly to a word document!
  
  # build table
  tbl <- data.table(matrix(nrow=0, ncol=8))
  for (i in 1:length(exposure)) {
    for (j in 1:length(outcome)) {
      exp <- exposure[i]
      out <- outcome[j]
      filtered_res <- results[results$Y==out & results$X==exp,]
      filtered_adj <- results_adj[results_adj$Y==out & results_adj$X==exp,]
      unadj <- paste(round(filtered_res$`point.diff`, 2), " (", round(filtered_res$`lb.diff`, 2), ", ", round(filtered_res$`ub.diff`, 2), ")", sep="")
      adj <- paste(round(filtered_adj$`point.diff`, 2), " (", round(filtered_adj$`lb.diff`, 2), ", ", round(filtered_adj$`ub.diff`, 2), ")", sep="")
      if (j==1){
        tbl <- rbind(tbl, list(expo_var[i], out_var[j], round(filtered_res$q1, 2), round(filtered_res$q3, 2), unadj, round(filtered_res$Pval, 2), adj, round(filtered_adj$Pval, 2)))
      }else {
        tbl <- rbind(tbl, list("", out_var[j], round(filtered_res$q1, 2), round(filtered_res$q3, 2), unadj, round(filtered_res$Pval, 2), adj, round(filtered_adj$Pval, 2)))
      }
    }
    if (i != length(exposure)) {
      tbl <- rbind(tbl, list("","","","","","","",""))
    }
  }
  
  # format for export
  flextbl<-flextable(tbl, col_keys=names(tbl))
  flextbl <- set_header_labels(flextbl,
                               values = list("V1" = name, "V2" = "Outcome", "V3" = "Q1 Mean", "V4" = "Q3 Mean",
                                             "V5" = "Coefficient (95% CI)", "V6" = "P-value",
                                             "V7" = "Coefficient (95% CI)", "V8" = "P-value"))
  flextbl <- add_header_row(flextbl, values = c("","","","", "Unadjusted", "Fully adjusted"), colwidths=c(1,1,1,1,2,2))
  flextbl <- hline_top(flextbl, part="header", border=fp_border(color="black", width = 2))
  flextbl <- add_header_row(flextbl, values = c("","","","", "Outcome, Q3 v. Q1"), colwidths=c(1,1,1,1,4))
  flextbl <- hline_top(flextbl, part="header", border=fp_border(color="black", width = 2))
  flextbl <- align(flextbl, align = "center", part = "all")
  flextbl <- align(flextbl, j = c(1, 2), align = "left", part="all")
  flextbl <- autofit(flextbl, part = "all")
  flextbl <- set_table_properties(flextbl, width = 1)
  
  flextbl
}



#### MAIN TABLES ####
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


#### Table 2 ####

exposure <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i")
outcome <- c("laz_t2", "laz_t3", "len_velocity_t2_t3", "delta_laz_t2_t3")
expo_var <- c("IPF(2a)-III", "2,3-dinor-iPF(2a)-III", "iPF(2a)-VI", "8,12-iso-iPF(2a)-VI")
out_var <- c("LAZ Year 1", "LAZ Year 2", "Length velocity Year 1 and Year 2", "Change in LAZ Year 1 to Year 2")

tbl2 <- growth_tbl("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj)
tbl2flex <- growth_tbl_flex("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj)

#### Table 3 ####

exposure <- c("t3_cort_slope", "t3_residual_cort", "t3_saa_slope", "t3_residual_saa")
outcome <- c("laz_t3")
expo_var <- c("Pre to post-stress change in cortisol", "Cortisol residualized gain score", "Pre to post-stress change in sAA", "sAA residualized gain score")
out_var <- c("LAZ Year 2")

tbl3 <- growth_tbl("Salivary stress biomarker", expo_var, out_var, exposure, outcome, H2, H2adj)
tbl3flex <- growth_tbl_flex("Salivary stress biomarker", expo_var, out_var, exposure, outcome, H2, H2adj)

#### Table 4 ####

exposure <- c("t3_map", "t3_hr_mean")
outcome <- c("laz_t3")
expo_var <- c("Mean arterial pressure", "Mean resting heart rate")
out_var <- c("LAZ Year 2")

tbl4 <- growth_tbl("Resting SAM biomarker", expo_var, out_var, exposure, outcome, H3, H3adj)
tbl4flex <- growth_tbl_flex("Resting SAM biomarker", expo_var, out_var, exposure, outcome, H3, H3adj)


#### Table 5 ####

exposure <- c("t3_gcr_mean", "t3_gcr_cpg12")
outcome <- c("laz_t3")
expo_var <- c("Entire promoter region (39 assayed CpG sites)", "NGFI-A transcription factor binding site (CpG site #12)")
out_var <- c("LAZ Year 2")

tbl5 <- growth_tbl("Methylation site", expo_var, out_var, exposure, outcome, H4, H4adj)
tbl5flex <- growth_tbl_flex("Methylation site", expo_var, out_var, exposure, outcome, H4, H4adj)


#### Supplementary Tables ####
#### Table S1 ####

exposure <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i")
outcome <- c("waz_t2", "whz_t2", "hcz_t2", "waz_t3", "whz_t3", "hcz_t3",
             "wei_velocity_t2_t3", "hc_velocity_t2_t3",
             "delta_waz_t2_t3", "delta_whz_t2_t3", "delta_hcz_t2_t3")
expo_var <- c("IPF(2a)-III", "2,3-dinor-iPF(2a)-III", "iPF(2a)-VI", "8,12-iso-iPF(2a)-VI")
out_var <- c("WAZ Year 1", "WLZ Year 1", "HCZ Year 1",
             "WAZ Year 2", "WLZ Year 2", "HCZ Year 2",
             "Weight velocity (kg/month) Year 1 to Year 2",
             "Head circumference velocity (cm/month) Year 1 to Year 2",
             "Change in child WAZ from Year 1 to Year 2",
             "Change in WLZ from Year 1 to Year 2",
             "Change in HCZ from Year 1 to Year 2")

tbls1 <- growth_tbl("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj)
tbls1flex <- growth_tbl_flex("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj)


#### Table S2 ####

exposure <- c("t3_cort_slope", "t3_residual_cort", "t3_saa_slope", "t3_residual_saa")
outcome <- c("waz_t3", "whz_t3", "hcz_t3")
expo_var <- c("Pre to post-stress change in cortisol", "Cortisol residualized gain score", "Pre to post-stress change in sAA", "sAA residualized gain score")
out_var <- c("WAZ Year 2", "WLZ Year 2", "HCZ Year 2")

tbls2 <- growth_tbl("Salivary stress biomarker", expo_var, out_var, exposure, outcome, H2, H2adj)
tbls2flex <- growth_tbl_flex("Salivary stress biomarker", expo_var, out_var, exposure, outcome, H2, H2adj)


#### Table S3 ####

exposure <- c("t3_map", "t3_hr_mean")
outcome <- c("waz_t3", "whz_t3", "hcz_t3")
expo_var <- c("Mean arterial pressure", "Mean resting heart rate")
out_var <- c("WAZ Year 2", "WLZ Year 2", "HCZ Year 2")

tbls3 <- growth_tbl("Resting SAM biomarker", expo_var, out_var, exposure, outcome, H3, H3adj)
tbls3flex <- growth_tbl_flex("Resting SAM biomarker", expo_var, out_var, exposure, outcome, H3, H3adj)



#### Table S4 ####

exposure <- c("t3_gcr_mean", "t3_gcr_cpg12")
outcome <- c("waz_t3", "whz_t3", "hcz_t3")
expo_var <- c("Entire promoter region (39 assayed CpG sites)", "NGFI-A transcription factor binding site (CpG site #12)")
out_var <- c("WAZ Year 2", "WLZ Year 2", "HCZ Year 2")

tbls4 <- growth_tbl("Methylation site", expo_var, out_var, exposure, outcome, H4, H4adj)
tbls4flex <- growth_tbl_flex("Methylation site", expo_var, out_var, exposure, outcome, H4, H4adj)


#### SAVE TABLES ####

write.csv(tbl1, file=here("tables/main/stress-growth-table1.csv"))
write.csv(tbl2, here('tables/main/stress-growth-table2.csv'))
write.csv(tbl3, here('tables/main/stress-growth-table3.csv'))
write.csv(tbl4, here('tables/main/stress-growth-table4.csv'))
write.csv(tbl5, here('tables/main/stress-growth-table5.csv'))

save_as_docx("Table 2" = tbl2flex, "Table 3" = tbl3flex, "Table 4" = tbl4flex, "Table 5" = tbl5flex, path=here('tables/stress-growth main.docx'))

write.csv(tbls1, here('tables/supplementary/stress-growth-tables1.csv'))
write.csv(tbls2, here('tables/supplementary/stress-growth-tables2.csv'))
write.csv(tbls3, here('tables/supplementary/stress-growth-tables3.csv'))
write.csv(tbls4, here('tables/supplementary/stress-growth-tables4.csv'))

save_as_docx("Table S1" = tbls1flex, "Table S2" = tbls2flex, "Table S3" = tbls3flex, "Table S4" = tbls4flex, path=here('tables/stress-growth supplementary.docx'))
