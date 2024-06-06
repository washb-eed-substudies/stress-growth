
#-------------------------------------
# EE substudies analysis 

# configure data directories
# source base functions
# load libraries
#-------------------------------------

library(tidyverse)
library(haven)
library(washb)
library(foreign)
library(data.table)
library(tmle)
library(SuperLearner)
library(devtools)
library(kableExtra)
library(here)
library(cowplot)
library(mgcv)
library(psych)
#library(boxr)
library(RColorBrewer)

#box_auth()


if(!require(washbgam)){
  devtools::install_github("washb-eed-substudies/washbgam")
  library(washbgam)
}

dropboxDir <- NULL
if(dir.exists("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/")){ 
  dropboxDir <- "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/"
}
if(dir.exists("/Users/audrielin/Dropbox/WBB-EE-analysis/")){ 
  dropboxDir <- "/Users/audrielin/Dropbox/WBB-EE-analysis/"
}
if(dir.exists("C:/Users/Sophia/Dropbox/WASH/")){ 
  dropboxDir <- "C:/Users/Sophia/Dropbox/WASH/"
}
if(dir.exists("/Users/lisa/Dropbox/WASH/")){ 
  dropboxDir <- "/Users/lisa/Dropbox/WASH/"
}



theme_ki<-function(){
  theme_bw() %+replace%
    theme(
      strip.background = element_blank(),
      legend.position="none",
      plot.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(size=14),
      axis.title = element_text(size=12),
      axis.text.y = element_text(size=10),
      axis.text.x = element_text(size=10, angle = 0, hjust = 0.5, vjust=.1)
    )
}

theme_set(theme_ki())

tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F",
               "#BCBD22","#17BECF")


#save R package versions

# # Only run thise lines once when project is initialized 
# #Call renv::init() to initialize a new project-local environment with a private R library,
# renv::init(project=here()) 
# 
# # Only run thise line when packages are updated
# #Call renv::snapshot() to save the state of the project library to the lockfile (called renv.lock),
# renv::snapshot()
# 
# # Only run these lines when needed (upon initialization and then when package versions need to be restored)
# #call renv::restore() to  revert to the previous state as encoded in the lockfile 
# renv::restore()


#-------------------------------------------------------------------------------
# Functions
#-------------------------------------------------------------------------------

clean_res <- function(res){

  res = clean_biomarker_names(res)
  res = clean_growth_variables(res)
  
  res <- res %>% mutate(group = case_when(grepl("t2_f2",X)~"Oxidative stress (Year 1)",
                                          grepl("t2_iso",X)~"Oxidative stress (Year 1)",
                                          grepl("saa",X)~"Sympathetic adrenomedullary axis (Year 2)",
                                          grepl("cort",X)~"Hypothalamic-pituitary-adrenal axis (Year 2)",
                                          grepl("map",X)~"Sympathetic adrenomedullary axis (Year 2)",
                                          grepl("hr",X)~"Sympathetic adrenomedullary axis (Year 2)",
                                          grepl("gcr",X)~"Hypothalamic-pituitary-adrenal axis (Year 2)",
                                          grepl("cpg",X)~"Hypothalamic-pituitary-adrenal axis (Year 2)")) %>%
    mutate(group=factor(group, levels=c("Oxidative stress (Year 1)", "Hypothalamic-pituitary-adrenal axis (Year 2)", "Sympathetic adrenomedullary axis (Year 2)"))) %>%
    arrange(group)
  return(res)
}

clean_growth_variables <- function(res){
  res$Y <- gsub("_t1_t2","",res$Y)
  res$Y <- gsub("_t2_t3","",res$Y)
  res$Y <- gsub("delta","Change in",res$Y)
  res$Y <- gsub("_"," ",res$Y)
  
  res$Y <- gsub("laz","LAZ",res$Y)
  res$Y <- gsub("waz","WAZ",res$Y)
  res$Y <- gsub("whz","WLZ",res$Y)
  res$Y <- gsub("hcz","HCAZ",res$Y)
  
  res$Y <- gsub("t1","-3 months",res$Y)
  res$Y <- gsub("t2","-Year 1",res$Y)
  res$Y <- gsub("t3","-Year 2",res$Y)
  
  res$Y <- gsub("len","Length",res$Y)
  res$Y <- gsub("wei","Weight",res$Y)
  res$Y <- gsub("hc","Head Circ.",res$Y)
  
  unique(res$Y)
  res$Y <- factor(res$Y, levels = (c("LAZ -3 months","WAZ -3 months","HCAZ -3 months",  "WLZ -3 months",
                                     "LAZ -Year 1","WAZ -Year 1","HCAZ -Year 1",  "WLZ -Year 1",             
                                        "LAZ -Year 2",   "WAZ -Year 2","HCAZ -Year 2", "WLZ -Year 2",               
                                        "Change in LAZ","Change in WAZ","Change in HCAZ","Change in WLZ",
                                        "Length velocity","Weight velocity","Head Circ. velocity")))
  
  res <- res %>% droplevels()
  return(res)
}

clean_biomarker_names <- function(d){
  
  d <- d %>% mutate(
    X_label = case_when(
      X=="t2_f2_8ip"~"iPF(2a)-III (ng/mg creatinine)", 
      X=="t2_f2_23d"~"2,3-dinor-iPF(2a)-III (ng/mg creatinine)", 
      X=="t2_f2_VI"~"iPF(2a)-VI (ng/mg creatinine)", 
      X=="t2_f2_12i"~"8,12-iso-iPF(2a)-VI (ng/mg creatinine)", 
      X=="t2_iso_pca"~"First principal component of isoprostanes", 
      X=="t3_saa_z01"~"Pre-stressor salivary alpha-amylase (U/ml)",
      X=="t3_saa_z02"~"Post-stressor salivary alpha-amylase (U/ml)", 
      X=="t3_saa_slope"~"Slope between pre- and  \n post-stressor alpha-amylase (U/ml/min)", 
      X=="t3_residual_saa"~"Residualized gain score\nfor alpha-amylase (U/ml)", 
      X=="t3_cort_z01"~"Pre-stressor salivary cortisol (\u03bcg/dl)", 
      X=="t3_cort_z03"~"Post-stressor salivary cortisol (\u03bcg/dl)",
      X=="t3_cort_slope"~"Slope between pre- and  \n post-stressor cortisol (\u03bcg/dl/min)", 
      X=="t3_residual_cort"~"Residualized gain score\nfor cortisol (\u03bcg/dl)", 
      X=="t3_map"~"Mean arterial pressure (mmHg)", 
      X=="t3_hr_mean"~"Resting heart rate (bpm)", 
      X=="t3_gcr_mean"~"Logit-transformed *NR3C1* exon \n1F promoter methylation", 
      X=="t3_gcr_cpg12"~"Logit-transformed NGFI-A transcription \nfactor binding site methylation"
    ),
    X_f = case_when(
      X=="t2_f2_8ip"~"iPF(2a)-III", 
      X=="t2_f2_23d"~"2,3-dinor-iPF(2a)-III", 
      X=="t2_f2_VI"~"iPF(2a)-VI", 
      X=="t2_f2_12i"~"8,12-iso-iPF(2a)-VI", 
      X=="t2_iso_pca"~"Isoprostanes'\n1st principal component", 
      X=="t3_saa_z01"~"Pre-stressor salivary alpha-amylase",
      X=="t3_saa_z02"~"Post-stressor salivary alpha-amylase", 
      X=="t3_saa_slope"~"Alpha-amylase slope", 
      X=="t3_residual_saa"~"Residualized gain score for alpha-amylase", 
      X=="t3_cort_z01"~"Pre-stressor salivary cortisol", 
      X=="t3_cort_z03"~"Post-stressor salivary cortisol",
      X=="t3_cort_slope"~"Cortisol slope", 
      X=="t3_residual_cort"~"Residualized gain score for cortisol", 
      X=="t3_map"~"Mean arterial pressure", 
      X=="t3_hr_mean"~"Resting heart rate", 
      X=="t3_gcr_mean"~"NR3C1 exon 1F promoter methylation", 
      X=="t3_gcr_cpg12"~"NGFI-A transcription factor binding site methylation"
    ),
    X_f=factor(X_f, levels = rev(c("iPF(2a)-III", "2,3-dinor-iPF(2a)-III", "iPF(2a)-VI", "8,12-iso-iPF(2a)-VI", "Isoprostanes'\n1st principal component", 
                               "Pre-stressor salivary alpha-amylase", "Post-stressor salivary alpha-amylase", "Alpha-amylase slope", 
                               "Residualized gain score for alpha-amylase", "Pre-stressor salivary cortisol", "Post-stressor salivary cortisol",
                               "Cortisol slope", "Residualized gain score for cortisol", "Mean arterial pressure", "Resting heart rate", 
                               "NR3C1 exon 1F promoter methylation", "NGFI-A transcription factor binding site methylation")))
  )
  return(d)
}



#fix VIF error in gam function
fit_RE_gam <- function (d, Y, X, W = NULL, forcedW = NULL, V = NULL, id = "clusterid", 
          family = "gaussian", pval = 0.2, print = TRUE){
  cat("\nNon-prescreened covariates: ", paste(forcedW, sep = "", 
                                              collapse = ", "), "\n")
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
                                                   Wnames) | grepl("agedays_", Wnames) | grepl("ageday_", 
                                                                                               Wnames)])
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
      todrop <- try(suppressWarnings(names(tmp$coefficients)[-1][as.vector(vif(tmp)) > 10][1]))
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
    d <- subset(gamdat, select = c("Y", "X", "id", "V", Wscreen))
  }
  else {
    d <- subset(gamdat, select = c("Y", "X", "id", "V"))
  }
  fullrows <- nrow(d)
  d <- d %>% filter(!is.na(Y))
  Yrows <- nrow(d)
  cat("\nRows dropped due to missing outcome: ", fullrows - 
        Yrows, "\n")
  d <- d %>% filter(!is.na(X))
  Xrows <- nrow(d)
  cat("Rows dropped due to missing exposure: ", Yrows - Xrows, 
      "\n")
  if (!is.null(W) & length(Wscreen) > 0) {
    cat("Percent missingness by covariate:\n")
    print(sapply(d[, -c(1:3)], function(x) round(sum(is.na(x))/nrow(X) * 
                                                   100, 1)))
    d <- d[complete.cases(d), ]
    cat("\nRows dropped due to missing covariates: ", Xrows - 
          nrow(d), "\n")
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
      eq_num <- paste0("s(", W_continious, ", bs=\"cr\")", 
                       collapse = " + ")
    }
    else {
      eq_num = NULL
    }
    if (length(W_factors) + length(W_indicator) > 0) {
      eq_fact <- paste0(" + ", paste0(c(W_factors, W_indicator), 
                                      collapse = " + "))
    }
    else {
      eq_fact = NULL
    }
    if (length(unique(d$X)) > 2) {
      if (!is.null(V)) {
        form <- paste0("Y~s(X, bs=\"cr\")+ V + X*V+", 
                       eq_fact, " +", eq_num, "+ s(id,bs=\"re\",by=dummy)")
        form <- gsub("+ +", "+", form, fixed = TRUE)
        equation <- as.formula(form)
        form_null <- paste0("Y~s(X, bs=\"cr\")+ V + ", 
                            eq_fact, " +", eq_num, "+ s(id,bs=\"re\",by=dummy)")
        form_null <- gsub("+ +", "+", form_null, fixed = TRUE)
        equation_null <- as.formula(form_null)
      }
      else {
        form <- paste0("Y~s(X, bs=\"cr\")+", eq_fact, 
                       " +", eq_num, "+ s(id,bs=\"re\",by=dummy)")
        form <- gsub("+ +", "+", form, fixed = TRUE)
        equation <- as.formula(form)
      }
    }
    else {
      if (!is.null(V)) {
        form <- paste0("Y~X+ V + X*V +", eq_fact, " +", 
                       eq_num, "+ s(id,bs=\"re\",by=dummy)")
        form <- gsub("+ +", "+", form, fixed = TRUE)
        equation <- as.formula(form)
        form_null <- paste0("Y~X+ V + ", eq_fact, " +", 
                            eq_num, "+ s(id,bs=\"re\",by=dummy)")
        form_null <- gsub("+ +", "+", form_null, fixed = TRUE)
        equation_null <- as.formula(form_null)
      }
      else {
        form <- paste0("Y~X+", eq_fact, " +", eq_num, 
                       "+ s(id,bs=\"re\",by=dummy)")
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
        equation <- as.formula(paste0("Y~s(X, bs=\"cr\")+ V + X*V + s(id,bs=\"re\",by=dummy)"))
        fit <- mgcv::gam(formula = equation, data = d)
        equation_null <- as.formula(paste0("Y~s(X, bs=\"cr\")+ V + s(id,bs=\"re\",by=dummy)"))
        fit_null <- mgcv::gam(formula = equation_null, 
                              data = d)
        LRp <- lrtest(fit, fit_null)[2, 5]
      }
      else {
        fit <- mgcv::gam(Y ~ s(X, bs = "cr") + s(id, 
                                                 bs = "re", by = dummy), data = d)
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
        fit <- mgcv::gam(Y ~ X + s(id, bs = "re", by = dummy), 
                         data = d)
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