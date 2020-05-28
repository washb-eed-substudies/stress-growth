
#---------------------------------------
# preamble
#---------------------------------------
rm(list=ls())
source(here::here("0-config.R"))

stress <- readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/clean_stress_dataset.RDS"))
anthro <- read.csv(paste0(dropboxDir,"Data/Cleaned/Andrew/BD-EE-anthro.csv"))

stress = stress %>% select(dataid, childNo, clusterid, block, tr,t2_f2_8ip, t2_f2_23d, t2_f2_VI, 
                   t2_f2_12i, t3_saa_z01, t3_cort_z01,
                    t3_saa_z02, t3_cort_z03, t3_map, t3_hr_mean, t3_gcr_mean, t3_gcr_cpg12,
                    t3_saa_slope,t3_cort_slope,t3_residual_saa,t3_residual_cort)


anthro = anthro %>% select( dataid,    childNo,aged2,     agem2,     agey2,     month2,    length2,   headcir2, 
                            laz2,      waz2,      whz2,      bmiz2,     hcz2,   
                            lazminus22,lazminus32,wazminus22,wazminus32,whzminus22,whzminus32,
                            date3,     aged3,     agem3,     agey3,     month3,    length3,   headcir3,  laz3,     
                            waz3,      whz3,      bmiz3,     hcz3,      
                            lazminus23, lazminus33, wazminus23, wazminus33, whzminus23, whzminus33)


d <- left_join(stress, anthro, by=c("dataid", "childNo"))



# Example GAM analysis
df <- d %>% select(clusterid, t3_hr_mean, laz3) %>%
  mutate(dummy=1) %>%
  rename(Y=laz3, x=t3_hr_mean)



fit <- mgcv::gam(Y~s(x, bs="cr")+s(clusterid,bs="re",by=dummy),data=df)




#Get model predictions
pd<-df
pd <- pd[order(pd$x),]
newd <- pd %>% mutate(dummy=0)
Xp <- predict(fit,newdata=newd,type="lpmatrix")

#fit_ci <- gamCI(m=fit,newdata=newd,nreps=10000)



# order the prediction matrix in the order of the exposure
Xp <- Xp[order(pd$x),]

# take difference from the first X
diff <- t(apply(Xp,1,function(x) x - Xp[1,]))

# calculate the predicted differences
point.diff <- diff %*% coef(fit)
point.diff_RE <- diff_RE %*% coef(fitRE)

# calculate the SE
se.diff <- sqrt(diag( diff%*% vcov(fit) %*%t(diff) ) )

lb.diff <- point.diff - 1.96*se.diff
ub.diff <- point.diff + 1.96*se.diff


plotdf<-data.frame(x=pd$x, point.diff, lb.diff=lb.diff, ub.diff=ub.diff)



p <- ggplot(plotdf) + geom_ribbon(aes(x=x, ymin=lb.diff, ymax=ub.diff), alpha=0.25) + 
  geom_path(aes(x=x, y=lb.diff), color="blue")+
  geom_path(aes(x=x, y=ub.diff), color="red")+
  geom_path(aes(x=x, y=point.diff), color="black") +
  coord_cartesian(expand = c(0,0))








washb_gam <- function(Y, tr, pair = NULL, W = NULL, forcedW = NULL, V = NULL, 
                      id, contrast, family = "gaussian", pval = 0.2, print = TRUE, 
                      verbose = FALSE){
  require(sandwich)
  require(lmtest)
  Subgroups = NULL
  if (!is.null(W)) {
    W <- data.frame(W)
  }
  if (!is.null(pair)) {
    if (!is.null(W)) {
      gamdat <- data.frame(id, Y, tr, pair, W)
    }
    else {
      gamdat <- data.frame(id, Y, tr, pair)
    }
    gamdat$tr <- factor(gamdat$tr, levels = contrast[1:2])
    gamdat$pair <- factor(gamdat$pair)
  }
  else {
    if (!is.null(W)) {
      gamdat <- data.frame(id, Y, tr, W)
    }
    else {
      gamdat <- data.frame(id, Y, tr)
    }
    gamdat$tr <- factor(gamdat$tr, levels = contrast[1:2])
  }
  gamdat <- subset(gamdat, tr == contrast[1] | tr == contrast[2])
  gamdat$tr <- factor(gamdat$tr, levels = contrast[1:2])
  if (!is.null(pair)) {
    n.orig <- dim(gamdat)[1]
    miss <- NULL
    activeOnly <- ((subset(gamdat, tr == contrast[1])))
    nomiss <- sort(unique(activeOnly$pair))
    miss1 <- (unique(pair)[which(!(unique(pair) %in% (nomiss)))])
    activeOnly2 <- ((subset(gamdat, tr == contrast[2])))
    nomiss2 <- sort(unique(activeOnly2$pair))
    miss2 <- (unique(pair)[which(!(unique(pair) %in% (nomiss2)))])
    miss <- append(miss1, miss2)
    gamdat <- subset(gamdat, !(pair %in% miss))
    n.sub <- dim(gamdat)[1]
    if (print == TRUE) 
      if (n.orig > n.sub) 
        cat("\n-----------------------------------------\n", 
            "Starting N:  ", n.orig, "\nN after block dropping: ", 
            n.sub)
    if (print == TRUE) 
      if (n.orig > n.sub) 
        cat("\n-----------------------------------------\n", 
            "Pairs/blocks dropped due to missingness in at least one treatment level:\n", 
            sort(unique(miss)), "\n\nDropping", n.orig - 
              n.sub, "observations due to missing pairs.", 
            "\n-----------------------------------------\n")
  }
  n.orig <- dim(gamdat)[1]
  rowdropped <- rep(1, nrow(gamdat))
  rowdropped[which(complete.cases(gamdat))] <- 0
  gamdat <- gamdat[complete.cases(gamdat), ]
  n.sub <- dim(gamdat)[1]
  if (print == TRUE) 
    if (n.orig > n.sub) 
      cat("\n-----------------------------------------\nDropping", 
          n.orig - n.sub, "observations due to missing values in 1 or more variables\n", 
          "Final sample size:", n.sub, "\n-----------------------------------------\n")
  if (!is.null(W)) {
    colnamesW <- names(W)
  }
  if (!is.null(W)) {
    if (!is.null(V)) {
      forcedW = c(V, forcedW)
    }
    if (!is.null(forcedW)) {
      screenW <- subset(gamdat, select = colnamesW)
      toexclude <- names(screenW) %in% forcedW
      if (length(which(toexclude == TRUE)) != length(forcedW)) 
        stop("A forcedW variable name is not a variable within the W data frame.")
      screenW = screenW[!toexclude]
      if (ncol(screenW) == 0) {
        screenW <- NULL
      }
      if (print == TRUE) {
        cat("\n-----------------------------------------\nInclude the following adjustment covariates without screening:\n-----------------------------------------\n")
        print(forcedW, sep = "\n")
      }
    }
    else {
      screenW <- subset(gamdat, select = colnamesW)
    }
  }
  else {
    screenW <- NULL
  }
  if (!is.null(screenW)) {
    if (print == TRUE) 
      cat("\n-----------------------------------------\nPre-screening the adjustment covariates:\n-----------------------------------------\n")
    suppressWarnings(Wscreen <- washb_prescreen(Y = gamdat$Y, 
                                                Ws = screenW, family = family, pval = pval, print = print))
  }
  else {
    Wscreen = NULL
  }
  if (!is.null(pair)) {
    if (!is.null(forcedW)) {
      if (!is.null(Wscreen)) {
        dmat <- subset(gamdat, select = c("Y", 
                                          "tr", forcedW, Wscreen, "pair"))
      }
      else {
        dmat <- subset(gamdat, select = c("Y", 
                                          "tr", forcedW, "pair"))
      }
    }
    else {
      if (!is.null(Wscreen)) {
        dmat <- subset(gamdat, select = c("Y", 
                                          "tr", Wscreen, "pair"))
      }
      else {
        dmat <- subset(gamdat, select = c("Y", 
                                          "tr", "pair"))
      }
    }
  }
  else {
    if (!is.null(forcedW)) {
      if (!is.null(Wscreen)) {
        dmat <- subset(gamdat, select = c("Y", 
                                          "tr", forcedW, Wscreen))
      }
      else {
        dmat <- subset(gamdat, select = c("Y", 
                                          "tr", forcedW))
      }
    }
    else {
      if (!is.null(Wscreen)) {
        dmat <- subset(gamdat, select = c("Y", 
                                          "tr", Wscreen))
      }
      else {
        dmat <- subset(gamdat, select = c("Y", 
                                          "tr"))
      }
    }
  }
  if (family[1] == "binomial" | family[1] == "poisson" | 
      family[1] == "gaussian") {
    if (!is.null(V)) {
      colnames(dmat)[which(colnames(dmat) == V)] <- "V"
      if (class(dmat$V) == "factor") 
        Subgroups <- levels(dmat$tr:dmat$V)
      if (class(dmat$V) != "factor") 
        warning("V is not a factor variable within the W covariate data frame. An interaction term will be added to the model but not linear combination of coefficients will be calculated.")
      suppressWarnings(fit <- gam(Y ~ tr * V + ., family = family, 
                                  data = dmat))
      vcovCL <- sandwichSE(dmat, fm = fit, cluster = gamdat$id)
      rfit <- coeftest(fit, vcovCL)
    }
    else {
      suppressWarnings(fit <- gam(Y ~ ., family = family, 
                                  data = dmat))
      vcovCL <- sandwichSE(dmat, fm = fit, cluster = gamdat$id)
      rfit <- coeftest(fit, vcovCL)
    }
    modelfit <- washb_gamFormat(gamModel = fit, rfit = rfit, 
                                dmat = dmat, rowdropped = rowdropped, contrast = contrast, 
                                pair = pair, vcovCL = vcovCL, family = family, V = V, 
                                Subgroups = Subgroups, print = print, verbose = verbose)
    return(modelfit)
  }
  else {
    stop("Error in family specified. Must choose Gaussian, Poisson, Binomial, Binomial(link-log), or neg.binom.")
  }
}














