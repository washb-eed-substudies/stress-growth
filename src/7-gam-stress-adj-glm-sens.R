



rm(list=ls())

source(here::here("0-config.R"))

d <- box_read("880476682582")

#glm function
fit_RE_glm <- function (d, Y, X, W = NULL, forcedW = NULL, V = NULL, id = "clusterid", family = "gaussian", pval = 0.2, print = TRUE){
  
  cat("\nNon-prescreened covariates: ", paste(forcedW, 
                                              sep = "", collapse = ", "), "\n")
  set.seed(12345)
  require(mgcv)
  require(dplyr)
  require(faraway)
  if (!is.null(V)) {
    require(lmtest)
  }
  if (!is.null(W)) {
    W <- subset(d, select = W)
  }
  Y <- subset(d, select = Y)
  colnames(Y) <- "Y"
  X <- subset(d, select = X)
  colnames(X) <- "X"
  id <- subset(d, select = id)
  colnames(id) <- "id"
  if (!is.null(V)) {
    Vvar <- subset(d, select = V)
    colnames(Vvar) <- "V"
  }
  else {
    Vvar <- data.frame(V = rep(1, nrow(d)))
  }
  collinear_vars <- NULL
  if (!is.null(W)) {
    gamdat <- data.frame(Y, X, id, Vvar, W)
  }
  else {
    gamdat <- data.frame(Y, X, id, Vvar)
  }
  if (!is.null(W)) {
    if (sum(is.na(forcedW)) != 0) {
      colnamesW <- names(W)
    }
    else {
      if (is.null(forcedW)) {
        Wnames <- names(W)
        forcedW <- c(Wnames[Wnames == "tr" | grepl("age_", 
                                                   Wnames) | grepl("agedays_", Wnames) | 
                              grepl("ageday_", Wnames)])
      }
      cat("\nNon-prescreened covariates: ", paste(forcedW, 
                                                  sep = "", collapse = ", "), "\n")
      colnamesW <- names(W)[!(names(W) %in% forcedW)]
    }
    screenW <- subset(gamdat, select = colnamesW)
  }
  else {
    screenW <- NULL
  }
  if (!is.null(screenW)) {
    if (print == TRUE) {
      cat("\n-----------------------------------------\nPre-screening the adjustment covariates:\n-----------------------------------------\n")
    }
    suppressWarnings(Wscreen <- washb_prescreen(Y = gamdat$Y, 
                                                Ws = screenW, family = family, pval = pval, print = print))
    if (!is.null(forcedW)) {
      Wscreen <- c(as.character(Wscreen), as.character(forcedW))
    }
    W <- subset(gamdat, select = Wscreen)
    Wdf <- W
    Wdf$constant <- rep(1, nrow(gamdat))
    for (i in 1:ncol(W)) {
      tmp <- glm(constant ~ ., data = Wdf, family = family)
      todrop <- NULL
      todrop <- suppressWarnings(names(tmp$coefficients)[-1][as.vector(vif(tmp)) > 
                                                               10][1])
      if (!is.null(todrop) & !is.na(todrop)) {
        collinear_vars <- c(collinear_vars, todrop)
        Wdf <- Wdf[, colnames(Wdf) != todrop]
      }
    }
    to_keep <- colnames(W)[!(colnames(W) %in% collinear_vars)]
    if (length(to_keep) != length(colnames(W))) {
      cat("\nDropped for collinearity with other covariates:\n", 
          colnames(W)[!(colnames(W) %in% to_keep)])
    }
    W_processed <- W[which(colnames(W) %in% to_keep)]
    Wscreen <- colnames(W_processed)
    cat("\n\nCovariated included in model:\n", Wscreen)
  }
  else {
    Wscreen = NULL
  }
  if (!is.null(Wscreen)) {
    d <- subset(gamdat, select = c("Y", "X", 
                                   "id", "V", Wscreen))
  }
  else {
    d <- subset(gamdat, select = c("Y", "X", 
                                   "id", "V"))
  }
  fullrows <- nrow(d)
  d <- d %>% filter(!is.na(Y))
  Yrows <- nrow(d)
  cat("\nRows dropped due to missing outcome: ", fullrows - 
        Yrows, "\n")
  d <- d %>% filter(!is.na(X))
  Xrows <- nrow(d)
  cat("Rows dropped due to missing exposure: ", Yrows - 
        Xrows, "\n")
  if (!is.null(W) & length(Wscreen) > 0) {
    cat("Percent missingness by covariate:\n")
    print(sapply(d[, -c(1:3)], function(x) round(sum(is.na(x))/nrow(X) * 
                                                   100, 1)))
    d <- d[complete.cases(d), ]
    cat("\nRows dropped due to missing covariates: ", 
        Xrows - nrow(d), "\n")
  }
  cat("Final sample size: ", nrow(d), "\n")
  d$dummy <- 1
  if (!is.null(W) & length(Wscreen) > 0) {
    Ws <- subset(gamdat, select = c(Wscreen))
    W_factors <- colnames(Ws)[(grepl("factor", sapply(Ws, 
                                                      class)) | grepl("character", sapply(Ws, class)))]
    W_numeric <- colnames(Ws)[(grepl("integer", sapply(Ws, 
                                                       class)) | grepl("numeric", sapply(Ws, class)))]
    indicator_vec <- rep(TRUE, length(W_numeric))
    for (i in 1:length(W_numeric)) {
      N_unique <- length(unique(Ws[, W_numeric[i]]))
      if (N_unique > 20) {
        indicator_vec[i] <- FALSE
      }
    }
    W_indicator <- W_numeric[indicator_vec]
    W_continious <- W_numeric[!indicator_vec]
    if (length(W_continious) > 0) {
      eq_num <- paste0(W_continious, 
                       collapse = " + ")
    }
    else {
      eq_num = NULL
    }
    if (length(W_factors) + length(W_indicator) > 0) {
      eq_fact <- paste0(" + ", paste0(c(W_factors, 
                                        W_indicator), collapse = " + "))
    }
    else {
      eq_fact = NULL
    }
    if (length(unique(d$X)) > 2) {
      if (!is.null(V)) {
        form <- paste0("Y~X + V + X*V+", 
                       eq_fact, " +", eq_num, "+ s(id,bs=\"re\",by=dummy)")
        form <- gsub("+ +", "+", form, fixed = TRUE)
        equation <- as.formula(form)
        form_null <- paste0("Y~X + V + ", 
                            eq_fact, " +", eq_num, "+ s(id,bs=\"re\",by=dummy)")
        form_null <- gsub("+ +", "+", form_null, 
                          fixed = TRUE)
        equation_null <- as.formula(form_null)
      }
      else {
        form <- paste0("Y~X+", eq_fact, 
                       " +", eq_num, "+ s(id,bs=\"re\",by=dummy)")
        form <- gsub("+ +", "+", form, fixed = TRUE)
        equation <- as.formula(form)
      }
    }
    else {
      if (!is.null(V)) {
        form <- paste0("Y~X+ V + X*V +", eq_fact, 
                       " +", eq_num, "+ s(id,bs=\"re\",by=dummy)")
        form <- gsub("+ +", "+", form, fixed = TRUE)
        equation <- as.formula(form)
        form_null <- paste0("Y~X+ V + ", eq_fact, 
                            " +", eq_num, "+ s(id,bs=\"re\",by=dummy)")
        form_null <- gsub("+ +", "+", form_null, 
                          fixed = TRUE)
        equation_null <- as.formula(form_null)
      }
      else {
        form <- paste0("Y~X+", eq_fact, " +", 
                       eq_num, "+ s(id,bs=\"re\",by=dummy)")
        form <- gsub("+ +", "+", form, fixed = TRUE)
        equation <- as.formula(form)
      }
    }
    fit <- mgcv::gam(formula = equation, data = d)
    if (!is.null(V)) {
      fit_null <- mgcv::gam(formula = equation_null, data = d)
      LRp <- lrtest(fit, fit_null)[2, 5]
    }
  }
  else {
    if (length(unique(d$X)) > 2) {
      if (!is.null(V)) {
        equation <- as.formula(paste0("Y~X + V + X*V + s(id,bs=\"re\",by=dummy)"))
        fit <- mgcv::gam(formula = equation, data = d)
        equation_null <- as.formula(paste0("Y~X + V + s(id,bs=\"re\",by=dummy)"))
        fit_null <- mgcv::gam(formula = equation_null, 
                              data = d)
        LRp <- lrtest(fit, fit_null)[2, 5]
      }
      else {
        fit <- mgcv::gam(Y ~ s(X, bs = "cr") + 
                           s(id, bs = "re", by = dummy), data = d)
      }
    }
    else {
      if (!is.null(V)) {
        equation <- as.formula(paste0("Y~X + V + X*V + s(id,bs=\"re\",by=dummy)"))
        fit <- mgcv::gam(formula = equation, data = d)
        equation_null <- as.formula(paste0("Y~X + V + s(id,bs=\"re\",by=dummy)"))
        fit_null <- mgcv::gam(formula = equation_null, 
                              data = d)
        LRp <- lrtest(fit, fit_null)[2, 5]
      }
      else {
        fit <- mgcv::gam(Y ~ X + s(id, bs = "re", 
                                   by = dummy), data = d)
      }
    }
  }
  if (!is.null(V)) {
    cat("\nInteraction p-value: ", LRp, "\n")
    return(list(fit = fit, dat = d, int.p = LRp, covars = Wscreen, 
                collinear_vars = collinear_vars))
  }
  else {
    return(list(fit = fit, dat = d, covars = Wscreen, collinear_vars = collinear_vars))
  }
}

predict_glm_diff <- function(fit, d, quantile_diff = c(0.25, 0.75), Xvar, Yvar, binaryX = FALSE){
  
  res <- summary(fit)
  res <- res$p.table[2,]
  res <- data.frame(t(res))
  
  lb.diff <- res$Estimate - 1.96 * res$Std..Error
  ub.diff <- res$Estimate + 1.96 * res$Std..Error
  res <- data.frame(Y = Yvar, X = Xvar, N = nrow(d), q1 = NA, 
                       q3 = NA, pred.q1 = NA, pred.q3 = NA, 
                       point.diff=res$Estimate, lb.diff = lb.diff, ub.diff = ub.diff, Pval = res$Pr...t..)
  
  return(list(res = res))
}



#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "HHwealth_scaled",
         "cesd_sum_t2", "diar7d_t2", "tr", "life_viol_any_t3")

Wvars[!(Wvars %in% colnames(d))]



# time varying covariates:
Wvars2_anthro<-c("ageday_at2", "month_at2")
Wvars3_anthro<-c("ageday_at3", "month_at3", "diar7d_t3", "cesd_sum_ee_t3", "pss_sum_mom_t3", "life_viol_any_t3")  

Wvars2_F2<-c("ageday_ut2", "month_ut2") 
Wvars3_vital<-c("ageday_t3_vital", "month_vt3", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t3", "life_viol_any_t3") 
Wvars3_salimetrics<-c("ageday_t3_salimetrics", "month_lt3", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t3", "life_viol_any_t3") 
Wvars3_oragene<-c("ageday_t3_oragene", "month_ot3", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t3", "life_viol_any_t3") 


# add time-varying covariates
# add hcz and time of day measurement later in pick covariates function 
W2_F2.W2_anthro <- c(Wvars, Wvars2_F2 ,Wvars2_anthro, "laz_t1_cat", "waz_t1_cat") %>% unique(.)
W2_F2.W3_anthro <- c(Wvars, Wvars2_F2 ,Wvars3_anthro, 
                     "laz_t2", "waz_t2") %>% unique(.)
#W2_F2.W23_anthro <- c(Wvars, Wvars2_F2, Wvars2_anthro, Wvars3_anthro, "laz_t2_cat", "waz_t2_cat") %>% unique(.)
W2_F2.W23_anthro <- c(Wvars, Wvars2_F2, Wvars2_anthro, Wvars3_anthro) %>% unique(.)
W3_vital.W3_anthro <- c(Wvars, Wvars3_vital, Wvars3_anthro, "laz_t2_cat", "waz_t2_cat") %>% unique(.)
W3_salimetrics.W3_anthro <- c(Wvars, Wvars3_salimetrics, Wvars3_anthro, "laz_t2_cat", "waz_t2_cat") %>% unique(.)
W3_oragene.W3_anthro <- c(Wvars, Wvars3_oragene, Wvars3_anthro, "laz_t2_cat", "waz_t2_cat") %>% unique(.)


pick_covariates <- function(i, j){
  # i is exposure as string
  # j is outcome as string
  # choose correct/build correct adjustment set based on exposure and outcome
  if(grepl("t2_", i)){
    if(grepl("_t2_t3", j)){Wset = W2_F2.W23_anthro}
    else if(grepl("_t2", j)){Wset = W2_F2.W2_anthro}
    else if(grepl("_t3", j)){Wset = W2_F2.W3_anthro}}
  else if(grepl("saa|cort", i)){
    if(grepl("residual", i)){Wset = W3_salimetrics.W3_anthro}
    else{Wset = c(W3_salimetrics.W3_anthro, "t3_col_time_z01_cont")}}
  else if(i %in% c("t3_map", "t3_hr_mean")){Wset = W3_vital.W3_anthro}
  else{Wset = W3_oragene.W3_anthro}
  
  if(j=="hcz_t3"){
    if(grepl("t2_",i)){Wset=c(Wset, "hcz_t2")}
    else{Wset=c(Wset, "hcz_t2_cat")}}
  if(j=="hcz_t2"){Wset=c(Wset, "hcz_t1_cat")}
  return(Wset)
}



#Loop over exposure-outcome pairs

##Hypothesis 1a
#Urinary creatine-adjusted F2-isoprostanes isomer score  at Year 1 is negatively associated with 
#concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 1.

# Exposure: Quartile of F2-isoprostanes isomer score
# Primary Outcome  : Child LAZ at Year 1
# Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 1
# Tertiary Outcomes: Child WLZ at Year 1

##Hypothesis 1b
#Urinary creatine-adjusted F2-isoprostanes isomer score at Year 1 is negatively 
#associated with child growth velocity (kg/month or cm/month) between the Year 1 and Year 2 visits.	

#Exposure: Quartile of F2-isoprostanes isomer score
#Primary Outcome: Child length velocity (in cm/month) from Year 1 to Year 2
#Secondary Outcome: Child weight velocity (in kg/month) and head circumference velocity (in cm/month) from Year 1 to Year 2

## Hypothesis 1c
#Urinary creatine-adjusted F2-isoprostanes isomer score at Year 1 is negatively 
#associated with subsequent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2. 

#Exposure: Quartile of F2-isoprostanes isomer score
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcome: Child WLZ at Year 2

##Hypothesis 1d
#Urinary creatine-adjusted F2-isoprostanes isomer score at Year 1 is negatively 
#associated with the change in child LAZ, WAZ, WLZ, and head circumference-for-age Z score from Year 1 to Year 2. 

#Exposure: Quartiles of F2-isoprostanes isomer score
#Primary Outcome: Change in child LAZ from Year 1 to Year 2
#Secondary Outcome: Change in child WAZ and head circumference-for-age Z score from Year 1 to Year 2
#Tertiary Outcomes: Change in child WLZ from Year 1 to Year 2

Xvars <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_iso_pca")            
Yvars <- c("laz_t2", "waz_t2", "whz_t2" ,"hcz_t2", 
           "len_velocity_t2_t3", "wei_velocity_t2_t3", "hc_velocity_t2_t3",
           "laz_t3", "waz_t3", "whz_t3", "hcz_t3",
           "delta_laz_t2_t3", "delta_waz_t2_t3", "delta_whz_t2_t3", "delta_hcz_t2_t3")

#Fit models
H1_glm_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-pick_covariates(i, j)
    res_glm <- fit_RE_glm(d=d, X=i, Y=j,  W=Wset)
    res <- data.frame(X=i, Y=j, fit=I(list(res_glm$fit)), dat=I(list(res_glm$dat)))
    H1_glm_models <- bind_rows(H1_glm_models, res)
  }
}


#Get primary contrasts
H1_glm_res <- NULL
for(i in 1:nrow(H1_glm_models)){
  res <- data.frame(X=H1_glm_models$X[i], Y=H1_glm_models$Y[i])
  preds <- predict_glm_diff(fit=H1_glm_models$fit[i][[1]], d=H1_glm_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1_glm_res <-  bind_rows(H1_glm_res , preds$res)
}


#Save results
saveRDS(H1_glm_res, here("results/sensitivity/H1_glm_res.RDS"))



## Hypothesis 2a
#Change in slope between pre- and post-stressor cortisol measured at Year 2 is positively associated 
#with concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of pre- and post-stressor cortisol at Year 2
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcome: Child WLZ at Year 2

##Hypothesis 2b
#Residualized gain score for cortisol measured at Year 2 is positively associated 
#with concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of pre- and post-stressor cortisol at Year 2
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcomes: Child WLZ at Year 2

##Hypothesis 2c
#Change in slope between pre- and post-stressor alpha-amylase measured at Year 2 is negatively associated 
#with concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of pre- and post-stressor alpha-amylase at Year 2
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcome: Child WLZ at Year 2

##Hypothesis 2d
#Residualized gain score for alpha-amylase measured at Year 2 is negatively associated 
#with concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of pre- and post-stressor alpha-amylase at Year 2
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcome: Child WLZ at Year 2

Xvars <- c("t3_cort_slope", "t3_residual_cort", "t3_cort_z01", "t3_cort_z03",
           "t3_saa_slope", "t3_residual_saa", "t3_saa_z01", "t3_saa_z02")          
Yvars <- c("laz_t3", "waz_t3", "whz_t3", "hcz_t3")



#Fit models
H2_glm_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-pick_covariates(i, j)
    res_glm <- fit_RE_glm(d=d, X=i, Y=j,  W=Wset)
    res <- data.frame(X=i, Y=j, fit=I(list(res_glm$fit)), dat=I(list(res_glm$dat)))
    H2_glm_models <- bind_rows(H2_glm_models, res)
  }
}





#Get primary contrasts
H2_glm_res <- NULL
for(i in 1:nrow(H2_glm_models)){
  res <- data.frame(X=H2_glm_models$X[i], Y=H2_glm_models$Y[i])
  preds <- predict_glm_diff(fit=H2_glm_models$fit[i][[1]], d=H2_glm_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H2_glm_res <-  bind_rows(H2_glm_res , preds$res)
}

#Save results
saveRDS(H2_glm_res, here("results/sensitivity/H2_glm_res.RDS"))



##Hypothesis 3a
#Mean arterial pressure measured at Year 2 is negatively associated with concurrent 
#child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of mean arterial pressure at Year 2
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcomes: Child WLZ at Year 2

##Hypothesis 3b
#Resting heart rate measured at Year 2 is negatively associated with concurrent 
#child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of resting heart rate at Year 2
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcomes: Child WLZ at Year 2

Xvars <- c("t3_map", "t3_hr_mean")            
Yvars <- c("laz_t3", "waz_t3", "whz_t3", "hcz_t3")


#Fit models
H3_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-pick_covariates(i, j)
    res_glm <- fit_RE_glm(d=d, X=i, Y=j,  W=Wset)
    res <- data.frame(X=i, Y=j, fit=I(list(res_glm$fit)), dat=I(list(res_glm$dat)))
    H3_models <- bind_rows(H3_models, res)
  }
}

#Get primary contrasts
H3_res <- NULL
for(i in 1:nrow(H3_models)){
  res <- data.frame(X=H3_models$X[i], Y=H3_models$Y[i])
  preds <- predict_glm_diff(fit=H3_models$fit[i][[1]], d=H3_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H3_res <-  bind_rows(H3_res , preds$res)
}



#Save results
saveRDS(H3_res, here("results/sensitivity/H3_glm_res.RDS"))


##Hypothesis 4a
#Glucocorticoid receptor (NR3C1) exon 1F promoter methylation in saliva samples at Year 2 
#is negatively associated with concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of overall percentage of methylation across the entire promoter region at Year 2 post-intervention
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcomes: Child WLZ at Year 2

##Hypothesis 4b
#Glucocorticoid receptor NGFI-A transcription factor binding site methylation in saliva samples at Year 2 
#is negatively associated with concurrent child LAZ, WAZ, WLZ, and head circumference-for-age Z score at Year 2.

#Exposure: Quartiles of percentage methylation at NGFI-A transcription factor binding 	site (CpG site #12)
#Primary Outcome: Child LAZ at Year 2
#Secondary Outcome: Child WAZ and head circumference-for-age Z score at Year 2
#Tertiary Outcomes: Child WLZ at Year 2

Xvars <- c("t3_gcr_mean", "t3_gcr_cpg12")            
Yvars <- c("laz_t3", "waz_t3", "whz_t3", "hcz_t3")

#Fit models
H4_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-pick_covariates(i, j)
    res_glm <- fit_RE_glm(d=d, X=i, Y=j,  W=Wset)
    res <- data.frame(X=i, Y=j, fit=I(list(res_glm$fit)), dat=I(list(res_glm$dat)))
    H4_models <- bind_rows(H4_models, res)
  }
}

#Get primary contrasts
H4_res <- NULL
for(i in 1:nrow(H4_models)){
  res <- data.frame(X=H4_models$X[i], Y=H4_models$Y[i])
  preds <- predict_glm_diff(fit=H4_models$fit[i][[1]], d=H4_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H4_res <-  bind_rows(H4_res , preds$res)
}



#Save results
saveRDS(H4_res, here("results/sensitivity/H4_glm_res.RDS"))



