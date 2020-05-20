
source(here::here("0-config.R"))
library(sandwich)
# GAM_est <- function(dat=d, 
#                           Y="delta_laz_t2_t3", 
#                           W=NULL, 
#                           A="delta_TS", 
#                           id="block",
#                           family="gaussian")


#load covariates, exposures, outcomes dataset
load(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.RData"))
stress <- readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/clean_stress_dataset.RDS"))
stress$dataid <- as.character(stress$dataid)
df <- left_join(d, stress, by = c("dataid"))


#-------------------------------------
# GAM fit with splines for x and w
#-------------------------------------

outcomes <- c("t2_f2_8ip","t2_f2_23d","t2_f2_VI", "t2_f2_12i",
              "t3_map","t3_hr_mean",
              "t3_saa_z01","t3_saa_z02","t3_cort_z01","t3_cort_z03",
              "t3_gcr_mean","t3_gcr_cpg12","t3_saa_slope","t3_cort_slope","t3_residual_saa","t3_residual_cort")



# df$x <- df$st_aged3
# #df$Y <- df$t3_saa_z01
# 
# df$Y <- (5*df$x^3)/1000000000 + rnorm(nrow(df),.3)
# 
# df <- df %>% filter(!is.na(x), !is.na(Y), !is.na(clusterid.x))

#simulate data
## simulate nested random effects....
set.seed(0)
n.g <- 10
n<-n.g*10*4
dat <- gamSim(1,n=n,scale=2)
f <- dat$f
fa <- as.factor(rep(1:10,rep(4*n.g,10)))
ra <- rep(rnorm(10),rep(4*n.g,10))
# fb <- as.factor(rep(rep(1:4,rep(n.g,4)),10))
# rb <- rep(rnorm(4),rep(n.g,4))
for (i in 1:9) rb <- c(rb,rep(rnorm(4),rep(n.g,4)))

## simulate auto-correlated errors within groups
e<-array(0,0)
for (i in 1:40) {
  eg <- rnorm(n.g, 0, sd(f))
  for (j in 2:n.g) eg[j] <- eg[j-1]*0.6+ eg[j]
  e<-c(e,eg)
}

dat$y <- f + ra + rb + e
dat$id <- fa
dat$fb <- fb
df <- data.frame(Y=dat$y, x=dat$f, clusterid.x=dat$id)


ggplot(df, aes(x=x, y=Y)) + geom_point() + geom_smooth()

#fit model and make predictions
# fit <- gam(Y~ s(x,bs="cr"), family=gaussian,data=df)
# summary(fit)

#Add clustered standard errors
#https://drmowinckels.io/blog/gamm-random-effects/
#?gamm
fullfit <- mgcv::gamm(Y~ s(x,bs="cr"), random=list(clusterid.x=~1), family=gaussian, data=df, correlation=corAR1())
fit <- fullfit$gam
summary(fit)

fit <- gam(Y~ s(x,bs="cr"), family=gaussian,data=df)


#Get clustered standard errors
#https://r.789695.n4.nabble.com/mgcv-GAM-with-clustered-standard-errors-td4671349.html


# vcovCL(fit, cluster= df$clusterid.x)
# 
# vcovCL <- sandwichSE(df, fm = fit, cluster = df$clusterid.x)
# rfit <- coeftest(fit, vcovCL)

pd <- df
pd <- pd[order(pd$x),]
Xp <- predict(fit,newdata=pd,type="lpmatrix")

# Xp <- predict(rfit,newdata=pd,type="lpmatrix")

# Xp %*% coef(fit) gives the linear predictor: (E(y))
# so the required difference is computed by subtracting off
# the reference group value, here assumed to be min(x)
# but this will depend on your specific exposure / scientific question

# order the prediction matrix in the order of the exposure
Xp <- Xp[order(pd$x),]
# take difference on  from the min(x)
diff <- t(apply(Xp,1,function(x) x - Xp[floor(nrow(Xp)/2),]))

# calculate the log PR
point.diff <- diff %*% coef(fit)

# calculate the pointwise SE for log PR
se.diff <- sqrt(diag( diff%*%vcovCL(fit, cluster=df$clusterid.x)%*%t(diff) ) )
se.diff <- sqrt(diag( diff%*%vcovCL(fit, cluster=df %>% select(clusterid.x))%*%t(diff) ) )



# calculate upper and lower bounds
lb.diff <- point.diff - 1.96*se.diff
ub.diff <- point.diff + 1.96*se.diff

plotdf<-data.frame(x=pd$x, point.diff, lb.diff, ub.diff)


# # make the spikey predictions smooth with another spline
# smooth.diff    <- smooth.spline(x=pd$x, y=point.diff)
# smooth.lb.diff <- smooth.spline(x=pd$x, y=lb.diff)
# smooth.ub.diff <- smooth.spline(x=pd$x, y=ub.diff)

#-------------------------------------
# plot the diff as a function of x
#-------------------------------------

ggplot(plotdf) + geom_ribbon(aes(x=x, ymin=lb.diff, ymax=ub.diff), alpha=0.1) + 
  geom_path(aes(x=x, y=lb.diff), color="blue")+
  geom_path(aes(x=x, y=ub.diff), color="red")+
  geom_path(aes(x=x, y=point.diff), color="black")


#Compare to GAM
fit <- gam(Y~ s(x,bs="cr"), family=gaussian,data=df)
pd <- df
pd <- pd[order(pd$x),]
Xp <- predict(fit,newdata=pd,type="lpmatrix")
Xp <- Xp[order(pd$x),]
diff <- t(apply(Xp,1,function(x) x - Xp[floor(nrow(Xp)/2),]))
point.diff <- diff %*% coef(fit)
se.diff <- sqrt(diag( diff%*%vcov(fit)%*%t(diff) ) )
lb.diff <- point.diff - 1.96*se.diff
ub.diff <- point.diff + 1.96*se.diff

plotdf<-data.frame(x=pd$x, point.diff, lb.diff, ub.diff)

ggplot(plotdf) + geom_ribbon(aes(x=x, ymin=lb.diff, ymax=ub.diff), alpha=0.1) + 
  geom_path(aes(x=x, y=lb.diff), color="blue")+
  geom_path(aes(x=x, y=ub.diff), color="red")+
  geom_path(aes(x=x, y=point.diff), color="black")








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
















washb_glmFormat<-function(glmModel = glmModel, rfit, dmat, rowdropped, contrast, 
          pair, vcovCL, family = family, V = NULL, Subgroups = NULL, 
          print = print, verbose = verbose) 
{
  if (family[1] == "binomial" | family[1] == "poisson" | 
      family[1] == "neg.binom") {
    expcoef <- round(exp(rfit[, 1]), 8)
    RR <- data.frame(round(exp(rfit[, 1]), 8), round(exp(rfit[, 
                                                              1] - 1.96 * rfit[, 2]), 8), round(exp(rfit[, 1] + 
                                                                                                      1.96 * rfit[, 2]), 8))
  }
  else {
    if (family[1] == "gaussian") {
      RR <- data.frame(round((rfit[, 1]), 8), round((rfit[, 
                                                          1] - 1.96 * rfit[, 2]), 8), round((rfit[, 1] + 
                                                                                               1.96 * rfit[, 2]), 8))
    }
    else {
      stop("\nError: argument \"family\" is not a valid option.\n")
    }
  }
  if (family[1] == "binomial") {
    if (family[2] != "log" | length(family) == 1) {
      colnames(RR) <- c("OR", "2.5%", "97.5%")
    }
    else {
      colnames(RR) <- c("PR", "2.5%", "97.5%")
    }
  }
  else {
    if (family[1] == "poisson" | family[1] == "neg.binom") {
      if (family[2] != "log" | length(family) == 
          1) {
        colnames(RR) <- c("IRR", "2.5%", 
                          "97.5%")
      }
      else {
        colnames(RR) <- c("PR", "2.5%", "97.5%")
      }
    }
    else {
      colnames(RR) <- c("Coef.", "2.5%", "97.5%")
    }
  }
  if (family[1] == "gaussian") {
    fit <- cbind(RR, (rfit[, 2:4]))
    TR <- fit[2, ]
  }
  else {
    fit <- cbind(RR, (rfit[, ]))
    TR <- fit[2, ]
  }
  if (!is.null(V)) {
    if (class(dmat$V) != "factor") {
      fit <- fit[c(1:3, nrow(fit), 4:(nrow(fit) - 1)), 
                 ]
    }
    if (class(dmat$V) == "factor") {
      lincom <- (matrix(0, nrow = length(levels(dmat$V)), 
                        ncol = 6))
      lincom_index <- matrix(0, nrow = length(levels(dmat$V)), 
                             ncol = nrow(fit))
      for (i in 1:length(levels(dmat$V))) {
        temp <- rep(0, nrow(fit))
        temp[2] = 1
        if (i != 1) {
          temp[nrow(fit) - length(levels(dmat$V)) + i] <- 1
        }
        lincom_index[i, ] <- temp
        if (family[1] == "gaussian") {
          lincom[i, ] <- suppressWarnings(washb_lincom(lc = lincom_index[i, 
                                                                         ], fit = fit, vcv = vcovCL, measure = "RD", 
                                                       flag = 1))
        }
        if (family[1] != "gaussian") {
          lincom[i, ] <- suppressWarnings(washb_lincom(lc = lincom_index[i, 
                                                                         ], fit = fit, vcv = vcovCL, measure = "RR", 
                                                       flag = 1))
        }
      }
      lincom <- data.frame(levels(dmat$V), lincom)
      colnames(lincom) <- c("Tr vs. C by Subgroup", 
                            "est", "se.est", "est.lb", 
                            "est.ub", "Z", "P")
    }
  }
  if (print == TRUE) {
    if (!is.null(V) & class(dmat$V) == "factor") {
      cat("\n\n-----------------------------------------\n", 
          paste("GLM Fit:", contrast[2], "vs.", 
                contrast[1]), " by Subgroup: '", V, "'\n-----------------------------------------\n")
      print(lincom)
    }
    else {
      cat("\n\n-----------------------------------------\n", 
          paste("GLM Fit:", contrast[2], "vs.", 
                contrast[1]), "\n-----------------------------------------\n")
      print(TR)
    }
    if (!is.null(V)) {
      cat("\n\n-----------------------------------------\n Significance of effect modification variables \n-----------------------------------------\n")
      print(fit[(nrow(fit) - length(levels(dmat$V)) + 2):nrow(fit), 
                c(colnames(RR)[1], "Pr(>|z|)")])
    }
    if (ncol(dmat) > 3 & is.null(V) | ncol(dmat) > 4) {
      if (family[1] == "gaussian") {
        cat("\n Coef of covariates\n")
      }
      if (family[1] != "gaussian") {
        cat("\n RR of covariates\n")
      }
      if (!is.null(pair)) {
        print(RR[2:(nrow(RR) - (length(unique(dmat$pair)) - 
                                  1)), ])
      }
      else {
        print(RR[3:(nrow(RR)), ])
      }
    }
    if (verbose == TRUE) {
      cat("\n Type \"`modelname'$TR\" to return the treatment effect.")
      cat("\n Type \"`modelname'$fit\" to return full glm model estimates.")
      cat("\n Type \"`modelname'$vcv\" to return the variance-covariance matrix.")
      cat("\n Type \"`modelname'$rowdropped\" to return the vector list of observations included in the model fit")
      if (!is.null(V)) {
        cat("\n Type \"`modelname'$lincom\" to return subgroup-specific conditional relative risk estimates if a subgroup V is specified")
      }
      cat("\n Type \"`modelname'$glmModel\" to return the glm model fit to be used with predict() to return model predictions of the outcome")
    }
  }
  if (!is.null(V) & class(dmat$V) == "factor") {
    modelfit = list(TR = TR, fit = fit, vcv = vcovCL, rowdropped = rowdropped, 
                    glmModel = glmModel, lincom = lincom)
  }
  else {
    modelfit = list(TR = TR, fit = fit, vcv = vcovCL, rowdropped = rowdropped, 
                    glmModel = glmModel)
  }
  return(modelfit)
}