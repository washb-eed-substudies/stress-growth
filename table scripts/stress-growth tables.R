rm(list=ls())

source(here::here("0-config.R"))
source(here::here("table scripts/table-functions.R"))

# load enrollment characteristics and results
#d <- read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-stress-growth-covariates-stresslab-anthro.csv"))
d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/stress_growth_data_clean.RDS"))

#stunting prevalence
prop.table(table(d$laz_t2 < -2))[2] * 100
prop.table(table(d$laz_t3 < -2))[2] * 100


d <- d %>% mutate(female=ifelse(d$sex=="female", 1, 0), firstborn=ifelse(d$birthord==1, 1, 0))
table(d$sex)
table(d$female)

#subset to kids with at least one exposure-outcome pair
d <- d %>% filter((!is.na(t2_f2_8ip) | !is.na(t2_f2_23d) | !is.na(t2_f2_VI) | !is.na(t2_f2_12i) | !is.na(t2_iso_pca) | 
                  !is.na(t3_cort_slope) | !is.na(t3_residual_cort) | !is.na(t3_saa_slope) | !is.na(t3_residual_saa) | 
                  !is.na(t3_map) | !is.na(t3_hr_mean) | !is.na(t3_gcr_mean) | !is.na(t3_gcr_cpg12)) & 
                  (!is.na(laz_t2) | !is.na(waz_t2) | !is.na(whz_t2) | !is.na(hcz_t2) | 
                  !is.na(laz_t3) | !is.na(waz_t3) | !is.na(whz_t3) | !is.na(hcz_t3)))

H1 <- readRDS(here('results/unadjusted/H1_res_clean.RDS'))
H2 <- readRDS(here('results/unadjusted/H2_res_clean.RDS'))
H3 <- readRDS(here('results/unadjusted/H3_res_clean.RDS'))
H4 <- readRDS(here('results/unadjusted/H4_res_clean.RDS'))
H1adj <- readRDS(here('results/adjusted/H1_adj_res_clean.RDS'))
H2adj <- readRDS(here('results/adjusted/H2_adj_res_clean.RDS'))
H3adj <- readRDS(here('results/adjusted/H3_adj_res_clean.RDS'))
H4adj <- readRDS(here('results/adjusted/H4_adj_res_clean.RDS'))


#### MAIN TABLES ####
#### Table 1 ####
# Characteristics of participants
N_col <- function(vector){
  length(vector[!is.na(vector)])
}

nperc <- function(vector){
  n <- sum(vector==1, na.rm=T)
  perc <- round(n/sum(!is.na(vector))*100)
  paste(n, " (", perc, "%)", sep="")
}

mediqr <- function(vector){
  quantiles <- round(quantile(vector, na.rm=T), 2)
  paste(quantiles[3], " (", quantiles[2], ", ", quantiles[4], ")", sep="")
}

n_col <- c(N_col(d$female), 
           N_col(d$t2_f2_8ip), N_col(d$t2_f2_23d), N_col(d$t2_f2_VI), N_col(d$t2_f2_12i),
               N_col(d$t3_cort_slope), N_col(d$t3_residual_cort), N_col(d$t3_saa_slope), N_col(d$t3_residual_saa),
               N_col(d$t3_map), N_col(d$t3_hr_mean), N_col(d$t3_gcr_mean), N_col(d$t3_gcr_cpg12),
               N_col(d$laz_t2), N_col(d$waz_t2), N_col(d$whz_t2), N_col(d$hcz_t2),
               N_col(d$laz_t3), N_col(d$waz_t3), N_col(d$whz_t3), N_col(d$hcz_t3),
               N_col(d$diar7d_t2), 
               N_col(d$diar7d_t3),
               N_col(d$momage), 
               N_col(d$momheight), 
               N_col(d$momeduy),
               N_col(d$cesd_sum_t2), 
               N_col(d$cesd_sum_ee_t3), 
               N_col(d$pss_sum_mom_t3), 
               N_col(d$life_viol_any_t3))


n_med_col <- c(nperc(d$female), 
               #nperc(d$firstborn), 
               mediqr(d$t2_f2_8ip),
               mediqr(d$t2_f2_23d), mediqr(d$t2_f2_VI), mediqr(d$t2_f2_12i),
               mediqr(d$t3_cort_slope), mediqr(d$t3_residual_cort), mediqr(d$t3_saa_slope), mediqr(d$t3_residual_saa),
               mediqr(d$t3_map), mediqr(d$t3_hr_mean), mediqr(d$t3_gcr_mean), mediqr(d$t3_gcr_cpg12),
               mediqr(d$laz_t2), mediqr(d$waz_t2), mediqr(d$whz_t2), mediqr(d$hcz_t2),
               mediqr(d$laz_t3), mediqr(d$waz_t3), mediqr(d$whz_t3), mediqr(d$hcz_t3),
               nperc(d$diar7d_t2), 
               nperc(d$diar7d_t3),
               mediqr(d$momage), 
               mediqr(d$momheight), 
               mediqr(d$momeduy),
               mediqr(d$cesd_sum_t2), 
               mediqr(d$cesd_sum_ee_t3), 
               mediqr(d$pss_sum_mom_t3), 
               nperc(d$life_viol_any_t3))

tbl1 <- data.table("C1" = c("Child","","","","","","","","","","","","","","","","","","","","","","","Mother","","","","","",""),
                   "C2" = c("", "Urinary F2-isoprostanes (Year 1)","","","", "Salivary cortisol reactivity (Year 2)","", "sAA reactivity (Year 2)","",
                           "SAM biomarkers (Year 2)","", "Glucocorticoid receptor","", "Anthropometry (14 months, Year 1)","","","",
                           "Anthropometry (28 months, Year 2)","","","", "Diarrhea (14 months, Year 1)", "Diarrhea (28 months, Year 2)","",
                           "Anthropometry at enrollment", "Education", "Depression at Year 1", "Depression at Year 2", "Perceived stress at Year 2", 
                           "Intimate partner violence"),
                   "C3" = c("Female", "iPF(2a)-III", "2,3-dinor-iPF(2a)-III", "iPF(2a-VI", "8,12-iso-iPF(2a)-VI", 
                           "Change in slope between pre- and post-stressor cortisol", "Cortisol residualized gain score", 
                           "Change in slope between pre- and post-stressor sAA change", "sAA residualized gain score",
                           "Mean arterial pressure", "Resting heart rate", "NR3C1 exon 1F promoter methylation", "NGFI-A transcription factor binding site methylation",
                           "Length-for-age Z score", "Weight-for-age Z score", "Weight-for-length Z score", "Head circumference-for-age Z score",
                           "Length-for-age Z score", "Weight-for-age Z score", "Weight-for-length Z score", "Head circumference-for-age Z score",
                           "Caregiver-reported 7-day recall", "Caregiver-reported 7-day recall", "Age (years)", "Height (cm)", "Schooling completed (years)",
                           "CES-D score", "CES-D score", "Perceived Stress Scale score", "Any lifetime exposure"),
                   "C4" = n_col,
                   "C5" = n_med_col)

tbl1flex <- flextable(tbl1, col_keys=names(tbl1))
tbl1flex <- set_header_labels(tbl1flex,
                        values = list("C1" = "", "C2" = "", "C3" = "", "C4" = "N", "C5" = "n (%) or median (IQR)"))
tbl1flex <- hline_top(tbl1flex, part="header", border=fp_border(color="black", width = 1))
tbl1flex <- hline_bottom(tbl1flex, part="all", border=fp_border(color="black", width = 1))
tbl1flex <- autofit(tbl1flex, part = "all")
tbl1flex <- align(tbl1flex, j = c(1, 2, 3), align = "left", part="all")
tbl1flex <- align(tbl1flex, j = 4, align = "center", part="all")
tbl1flex <- fit_to_width(tbl1flex, max_width=8)
names(tbl1)<- c("","","","N","n (%) or median (IQR)")

write.csv(tbl1, file=here("tables/main/stress-growth-table1.csv"))

#### Table 2 new ####

tbl2_main <- data.frame(Hypothesis=c("1) Oxidative status is negatively associated with concurrent and future child growth measures, as well as growth velocity between year 1 and year 2.",
                                     "2) Salivary cortisol measures are positively associated with concurrent child growth measures, and sAA measures are negatively associated with concurrent child growth measures. ",
                                     "3) SAM biomarker measures are negatively associated with concurrent child growth",
                                     "4) Glucocorticoid receptor methylation is negatively associated with concurrent child growth measures"),
                        `Exposure Biological Domain and Biomarkers`=c(
                          "Oxidative status: Individual urinary F2 isoprostane isomers and combined score at Year 1",
                          "HPA axis: Cortisol and via concentrations pre-stressor, post-stressor, and reactivity at Year 2\nSAM axis: sAA and via concentrations pre-stressor, post-stressor, and reactivity at Year 2",
                          "SAM axis: mean arterial pressure and resting heart rate at Year 2",
                          "HPA axis: Glucocorticoid receptor methylation via percentage methylation at NGFI-A transcription factor binding site and mean overall glucocorticoid receptor methylation at Year 2"),
                        Outcomes=c("LAZ, WAZ*, HCAZ* score, and WLZ** at year 1, year 2, and the growth velocity between year 1 and 2.",
                                   rep("LAZ, WAZ*, HCAZ* score, and WLZ** at year 2.",3)),
                        Finding=c("","","",""))

tbl2flex_main <- flextable(tbl2_main, col_keys=names(tbl2_main))
tbl2flex_main <- set_header_labels(tbl2flex_main, values = list("V1" = "Hypothesis", "Exposure.Biological.Domain.and.Biomarkers" = "Exposure Biological Domain and Biomarkers", "V3" = "Outcomes", "V4" = "Finding"))
tbl2flex_main <- hline(tbl2flex_main, part="header", border=fp_border(color="black"))
tbl2flex_main <- hline_bottom(tbl2flex_main, part="body", border=fp_border(color="black"))
tbl2flex_main <- hline_top(tbl2flex_main, part="header", border=fp_border(color="black"))
tbl2flex_main <- align(tbl2flex_main, align = "center", part = "all")
tbl2flex_main <- align(tbl2flex_main, j = c(1, 2), align = "left", part="all")
tbl2flex_main <- fontsize(tbl2flex_main, part = "all", size = 7)
tbl2flex_main <- width(tbl2flex_main, 1:4, width=c(4, 3, 2, 4))



#### Table 3 new ####
exposure <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_iso_pca","t3_cort_z01", "t3_cort_z03", "t3_cort_slope", "t3_residual_cort", 
              "t3_saa_z01", "t3_saa_z02", "t3_saa_slope", "t3_residual_saa","t3_map", "t3_hr_mean","t3_gcr_mean", "t3_gcr_cpg12")
outcome <- c("laz_t2","laz_t3","len_velocity_t2_t3","delta_laz_t2_t3")
expo_var <- c("IPF(2a)-III", "2,3-dinor-iPF(2a)-III", "iPF(2a)-VI", "8,12-iso-iPF(2a)-VI", "Combined urinary oxidative stress biomarkers",
              "Pre-stressor cortisol", "Post-stressor cortisol", "Pre to post-stress change in cortisol", "Cortisol residualized gain score", 
              "Pre-stressor sAA", "Post-stressor sAA", "Pre to post-stress change in sAA", "sAA residualized gain score",
              "Mean arterial pressure", "Mean resting heart rate","Entire promoter region (39 assayed CpG sites)", "NGFI-A transcription factor binding site (CpG site #12)")
out_var <- c("LAZ Year 1", "LAZ Year 2","Length velocity (cm/month) Year 1 to Year 2", "Change in LAZ from Year 1 to Year 2")

tbl3_main <- growth_tbl("", expo_var, out_var, exposure, outcome, 
                   bind_rows(H1,H2,H3,H4) %>% filter(Y %in% outcome), 
                   bind_rows(H1adj,H2adj,H3adj,H4adj) %>% filter(Y %in% outcome), T)
tbl3flex_main <- growth_tbl_flex("", expo_var, out_var, exposure, outcome,
                            bind_rows(H1,H2,H3,H4) %>% filter(Y %in% outcome), 
                            bind_rows(H1adj,H2adj,H3adj,H4adj) %>% filter(Y %in% outcome),
                            T, 1.3, .7)



#### Table 2 ####

exposure <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_iso_pca")
outcome <- c("laz_t2", "waz_t2", "whz_t2", "hcz_t2")
expo_var <- c("IPF(2a)-III", "2,3-dinor-iPF(2a)-III", "iPF(2a)-VI", "8,12-iso-iPF(2a)-VI", "Combined urinary oxidative stress biomarkers")
out_var <- c("LAZ Year 1", "WAZ Year 1", "WLZ Year 1", "HCZ Year 1")

tbl2 <- growth_tbl("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj, T)
tbl2flex <- growth_tbl_flex("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj, T, 1.3, .7)
tbl1supp <- growth_tbl("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj)
tbl1flexsupp <- growth_tbl_flex("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj, F, 1, .6)

#### Table 3 ####

exposure <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_iso_pca")
outcome <- c("laz_t3", "waz_t3", "whz_t3", "hcz_t3")
expo_var <- c("IPF(2a)-III", "2,3-dinor-iPF(2a)-III", "iPF(2a)-VI", "8,12-iso-iPF(2a)-VI", "Combined urinary oxidative stress biomarkers")
out_var <- c("LAZ Year 2", "WAZ Year 2", "WLZ Year 2", "HCZ Year 2")

tbl3 <- growth_tbl("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj, T)
tbl3flex <- growth_tbl_flex("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj, T, 1.3, .7)
tbl2supp <- growth_tbl("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj)
tbl2flexsupp <- growth_tbl_flex("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj, F, 1, .6)

#### Table 4 ####

exposure <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_iso_pca")
outcome <- c("len_velocity_t2_t3", "wei_velocity_t2_t3", "hc_velocity_t2_t3")
expo_var <- c("IPF(2a)-III", "2,3-dinor-iPF(2a)-III", "iPF(2a)-VI", "8,12-iso-iPF(2a)-VI", "Combined urinary oxidative stress biomarkers")
out_var <- c("Length velocity (cm/month) Year 1 to Year 2", 
             "Weight velocity (kg/month) Year 1 to Year 2",
             "Head circumference velocity (cm/month) Year 1 to Year 2")

tbl4 <- growth_tbl("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj, T)
tbl4flex <- growth_tbl_flex("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj, T, 1, 1.4)
tbl3supp <- growth_tbl("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj)
tbl3flexsupp <- growth_tbl_flex("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj)

#### Table 5 ####

exposure <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_iso_pca")
outcome <- c("delta_laz_t2_t3", "delta_waz_t2_t3", "delta_whz_t2_t3", "delta_hcz_t2_t3")
expo_var <- c("IPF(2a)-III", "2,3-dinor-iPF(2a)-III", "iPF(2a)-VI", "8,12-iso-iPF(2a)-VI", "Combined urinary oxidative stress biomarkers")
out_var <- c("Change in LAZ from Year 1 to Year 2", 
             "Change in WAZ from Year 1 to Year 2",
             "Change in WLZ from Year 1 to Year 2",
             "Change in HCZ from Year 1 to Year 2")

tbl5 <- growth_tbl("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj, T)
tbl5flex <- growth_tbl_flex("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj, T, 1.1, 1.2)
tbl4supp <- growth_tbl("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj)
tbl4flexsupp <- growth_tbl_flex("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj)

#### Table 6 ####

exposure <- c("t3_cort_z01", "t3_cort_z03", "t3_cort_slope", "t3_residual_cort", 
              "t3_saa_z01", "t3_saa_z02", "t3_saa_slope", "t3_residual_saa")
outcome <- c("laz_t3", "waz_t3", "whz_t3", "hcz_t3")
expo_var <- c("Pre-stressor cortisol", "Post-stressor cortisol", "Pre to post-stress change in cortisol", "Cortisol residualized gain score", 
              "Pre-stressor sAA", "Post-stressor sAA", "Pre to post-stress change in sAA", "sAA residualized gain score")
out_var <- c("LAZ Year 2", "WAZ Year 2", "WLZ Year 2", "HCZ Year 2")

tbl6 <- growth_tbl("Salivary stress biomarker", expo_var, out_var, exposure, outcome, H2, H2adj, T)
tbl6flex <- growth_tbl_flex("Salivary stress biomarker", expo_var, out_var, exposure, outcome, H2, H2adj, T, 1.4, .7)
tbl5supp <- growth_tbl("Salivary stress biomarker", expo_var, out_var, exposure, outcome, H2, H2adj)
tbl5flexsupp <- growth_tbl_flex("Salivary stress biomarker", expo_var, out_var, exposure, outcome, H2, H2adj, F, 1.1, .7)

#### Table 7 ####

exposure <- c("t3_map", "t3_hr_mean")
outcome <- c("laz_t3", "waz_t3", "whz_t3", "hcz_t3")
expo_var <- c("Mean arterial pressure", "Mean resting heart rate")
out_var <- c("LAZ Year 2", "WAZ Year 2", "WLZ Year 2", "HCZ Year 2")

tbl7 <- growth_tbl("Resting SAM biomarker", expo_var, out_var, exposure, outcome, H3, H3adj, T)
tbl7flex <- growth_tbl_flex("Resting SAM biomarker", expo_var, out_var, exposure, outcome, H3, H3adj, T, 1.5, .7)
tbl6supp <- growth_tbl("Resting SAM biomarker", expo_var, out_var, exposure, outcome, H3, H3adj)
tbl6flexsupp <- growth_tbl_flex("Resting SAM biomarker", expo_var, out_var, exposure, outcome, H3, H3adj, F, 1, .7)


#### Table 8 ####

exposure <- c("t3_gcr_mean", "t3_gcr_cpg12")
outcome <- c("laz_t3", "waz_t3", "whz_t3", "hcz_t3")
expo_var <- c("Entire promoter region (39 assayed CpG sites)", "NGFI-A transcription factor binding site (CpG site #12)")
out_var <- c("LAZ Year 2", "WAZ Year 2", "WLZ Year 2", "HCZ Year 2")

tbl8 <- growth_tbl("Methylation site", expo_var, out_var, exposure, outcome, H4, H4adj, T)
tbl8flex <- growth_tbl_flex("Methylation site", expo_var, out_var, exposure, outcome, H4, H4adj, T, 1.3, .7)
tbl7supp <- growth_tbl("Methylation site", expo_var, out_var, exposure, outcome, H4, H4adj)
tbl7flexsupp <- growth_tbl_flex("Methylation site", expo_var, out_var, exposure, outcome, H4, H4adj, F, 1.1, .7)


# #### Supplementary Tables ####
# #### Table S1 ####
# 
# exposure <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_iso_pca")
# outcome <- c("waz_t2", "whz_t2", "hcz_t2", "waz_t3", "whz_t3", "hcz_t3",
#              "wei_velocity_t2_t3", "hc_velocity_t2_t3",
#              "delta_waz_t2_t3", "delta_whz_t2_t3", "delta_hcz_t2_t3")
# expo_var <- c("IPF(2a)-III", "2,3-dinor-iPF(2a)-III", "iPF(2a)-VI", "8,12-iso-iPF(2a)-VI", "Combined urinary oxidative stress biomarkers")
# out_var <- c("WAZ Year 1", "WLZ Year 1", "HCZ Year 1",
#              "WAZ Year 2", "WLZ Year 2", "HCZ Year 2",
#              "Weight velocity (kg/month) Year 1 to Year 2",
#              "Head circumference velocity (cm/month) Year 1 to Year 2",
#              "Change in child WAZ from Year 1 to Year 2",
#              "Change in WLZ from Year 1 to Year 2",
#              "Change in HCZ from Year 1 to Year 2")
# 
# tbls1 <- growth_tbl("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj)
# tbls1flex <- growth_tbl_flex("Urinary oxidative stress biomarker", expo_var, out_var, exposure, outcome, H1, H1adj)
# 
# 
# #### Table S2 ####
# 
# exposure <- c("t3_cort_z01", "t3_cort_z03", "t3_cort_slope", "t3_residual_cort", 
#               "t3_saa_z01", "t3_saa_z02", "t3_saa_slope", "t3_residual_saa")
# outcome <- c("waz_t3", "whz_t3", "hcz_t3")
# expo_var <- c("Pre-stressor cortisol", "Post-stressor cortisol", "Pre to post-stress change in slope of cortisol", "Cortisol residualized gain score", 
#               "Pre-stressor sAA", "Post-stressor sAA", "Pre to post-stress change in slope of sAA", "sAA residualized gain score")
# out_var <- c("WAZ Year 2", "WLZ Year 2", "HCZ Year 2")
# 
# tbls2 <- growth_tbl("Salivary stress biomarker", expo_var, out_var, exposure, outcome, H2, H2adj)
# tbls2flex <- growth_tbl_flex("Salivary stress biomarker", expo_var, out_var, exposure, outcome, H2, H2adj)
# 
# 
# #### Table S3 ####
# 
# exposure <- c("t3_map", "t3_hr_mean")
# outcome <- c("waz_t3", "whz_t3", "hcz_t3")
# expo_var <- c("Mean arterial pressure", "Mean resting heart rate")
# out_var <- c("WAZ Year 2", "WLZ Year 2", "HCZ Year 2")
# 
# tbls3 <- growth_tbl("Resting SAM biomarker", expo_var, out_var, exposure, outcome, H3, H3adj)
# tbls3flex <- growth_tbl_flex("Resting SAM biomarker", expo_var, out_var, exposure, outcome, H3, H3adj)
# 
# 
# 
# #### Table S4 ####
# 
# exposure <- c("t3_gcr_mean", "t3_gcr_cpg12")
# outcome <- c("waz_t3", "whz_t3", "hcz_t3")
# expo_var <- c("Entire promoter region (39 assayed CpG sites)", "NGFI-A transcription factor binding site (CpG site #12)")
# out_var <- c("WAZ Year 2", "WLZ Year 2", "HCZ Year 2")
# 
# tbls4 <- growth_tbl("Methylation site", expo_var, out_var, exposure, outcome, H4, H4adj)
# tbls4flex <- growth_tbl_flex("Methylation site", expo_var, out_var, exposure, outcome, H4, H4adj)


#### SAVE TABLES ####

write.csv(tbl1, file=here("tables/main/stress-growth-table1.csv"))
write.csv(tbl2, here('tables/main/stress-growth-table2.csv'))
write.csv(tbl3, here('tables/main/stress-growth-table3.csv'))
write.csv(tbl4, here('tables/main/stress-growth-table4.csv'))
write.csv(tbl5, here('tables/main/stress-growth-table5.csv'))
write.csv(tbl6, here('tables/main/stress-growth-table6.csv'))
write.csv(tbl7, here('tables/main/stress-growth-table7.csv'))
write.csv(tbl8, here('tables/main/stress-growth-table8.csv'))

save_as_docx("Table 1: Descriptive statistics of sample population" = tbl1flex, 
             "Table 2: Study exposures, outcomes, and hypothesized relationships" = tbl2flex_main,
             "Table 3: Associations between child stress measures and linear growth outcomes" = tbl3flex_main,
              path=paste0(here::here(),"/tables/stress-growth main v11.docx"),
             pr_section = sect_properties)

save_as_docx("Table S1: Associations between isoprostanes and growth measures at 14 months" = tbl2flex,
             "Table S2: Associations between isoprostanes and growth measures at 28 months" = tbl3flex,
             "Table S3: Associations between isoprostanes and growth velocity measures" = tbl4flex, 
             "Table S4: Associations between isoprostanes and change in growth Z-score" = tbl5flex, 
             "Table S5: Associations between salivary biomarkers and growth measures at 28 months" = tbl6flex, 
             "Table S6: Associations between sympathetic-adreno-medullar biomarkers and growth measures at 28 months" = tbl7flex, 
             "Table S7: Associations between methylation and growth measures at 28 months" = tbl8flex, 
             path=paste0(here::here(),"/tables/stress-growth supplementary v11.docx"),
             pr_section = sect_properties)

# write.csv(tbl1supp, here('tables/supplementary/stress-growth-tables1.csv'))
# write.csv(tbl2supp, here('tables/supplementary/stress-growth-tables2.csv'))
# write.csv(tbl3supp, here('tables/supplementary/stress-growth-tables3.csv'))
# write.csv(tbl4supp, here('tables/supplementary/stress-growth-tables4.csv'))
# write.csv(tbl5supp, here('tables/supplementary/stress-growth-tables5.csv'))
# write.csv(tbl6supp, here('tables/supplementary/stress-growth-tables6.csv'))
# write.csv(tbl7supp, here('tables/supplementary/stress-growth-tables7.csv'))
# 
# 
# save_as_docx("Table S1: Comparisons of unadjusted and adjusted associations between isoprostanes and growth measures at 14 months" = tbl1flexsupp,
#              "Table S2: Comparisons of unadjusted and adjusted associations between isoprostanes and growth measures at 28 months" = tbl2flexsupp,
#              "Table S3: Comparisons of unadjusted and adjusted associations between isoprostanes and growth velocity measures" = tbl3flexsupp, 
#              "Table S4: Comparisons of unadjusted and adjusted associations between isoprostanes and change in growth Z-scores" = tbl4flexsupp, 
#              "Table S5: Comparisons of unadjusted and adjusted associations between salivary biomarkers and growth measures at 28 months" = tbl5flexsupp, 
#              "Table S6: Comparisons of unadjusted and adjusted associations between sympathetic-adreno-medullar biomarkers and growth measures at 28 months" = tbl6flexsupp, 
#              "Table S7: Comparisons of unadjusted and adjusted associations between methylation and growth measures at 28 months" = tbl7flexsupp, 
#              path=paste0(here::here(),"/tables/stress-growth supplementary v10.docx"),
#              pr_section = sect_properties)

