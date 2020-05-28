
# load(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.RData"))
# 
# Y="laz_t3"
# X="delta_TS"
# W=c("sex","n_chicken")
# id="clusterid"
# family = "gaussian"
# pval = 0.2
# print=TRUE
# 
# 
# 
# res <- fit_RE_gam(d=d, Y="laz_t3", X="delta_TS", W=c("sex","n_chicken"))
# preds <- predict_gam_diff(fit=res$fit, d=res$dat, quantile_diff=c(0.25,0.75))


#Note: update to allow 
plot_gam_diff <- function(plotdf){
  p<-ggplot(plotdf) + geom_ribbon(aes(x=x, ymin=lb.diff, ymax=ub.diff), alpha=0.5) + 
    geom_path(aes(x=x, y=lb.diff), color="blue")+
    geom_path(aes(x=x, y=ub.diff), color="red")+
    geom_path(aes(x=x, y=point.diff), color="black") + 
    geom_vline(aes(xintercept=q1)) +
    geom_vline(aes(xintercept=q3)) + 
    xlab("Exposure") + 
    ylab("GAM-estimated differences from 25th percentile of exposure")
  return(p)
}



#Note:
#Update code so that any continious variable with less than 20 unique values is not modeled with a spline
fit_RE_gam <- function(d, Y, X, W=NULL, id="clusterid", family = "gaussian", pval = 0.2, print=TRUE){
  
  if(!is.null(W)){
    W <- subset(d, select = W)
  }
   Y <- subset(d, select = Y)
   colnames(Y) <- "Y"
   X <- subset(d, select = X)
   colnames(X) <- "X"
   id <- subset(d, select = id)
   colnames(id) <- "id"
   
    if(!is.null(W)){
      gamdat <- data.frame(Y, X, id, W)
    }else{
      gamdat <- data.frame(Y, X, id)
    }
  
   
  n.orig <- dim(gamdat)[1]
  rowdropped <- rep(1, nrow(gamdat))
  rowdropped[which(complete.cases(gamdat))] <- 0
  gamdat <- gamdat[complete.cases(gamdat), ]
  n.sub <- dim(gamdat)[1]
  if(print == TRUE){ 
    if (n.orig > n.sub){ 
      cat("\n-----------------------------------------\nDropping", 
          n.orig - n.sub, "observations due to missing values in 1 or more variables\n", 
          "Final sample size:", n.sub, "\n-----------------------------------------\n")
    }
  }
  if(!is.null(W)){
    colnamesW <- names(W)
    screenW <- subset(gamdat, select = colnamesW)
    }else{
    screenW <- NULL
  }
  if(!is.null(screenW)){
    if (print == TRUE) 
      cat("\n-----------------------------------------\nPre-screening the adjustment covariates:\n-----------------------------------------\n")
    suppressWarnings(Wscreen <- washb_prescreen(Y = gamdat$Y, 
                                                Ws = screenW, family = family, pval = pval, print = print))
  }else{
    Wscreen = NULL
  }

      if(!is.null(Wscreen)){
        d <- subset(gamdat, select = c("Y","X","id", Wscreen))
      }else{
        d <- subset(gamdat, select = c("Y","X","id"))
      }
  
  d$dummy<-1

  
  if(!is.null(W)){
    
    #Make formula for adjusted model
    Ws <- subset(gamdat, select = c(Wscreen))
    
    W_factors <- colnames(Ws)[(grepl("factor", sapply(Ws, class))|grepl("character", sapply(Ws, class)))]
    W_numeric <- colnames(Ws)[(grepl("integer", sapply(Ws, class))|grepl("numeric", sapply(Ws, class)))]
    eq_fact <- paste0("s(", W_numeric, ", bs=\"cr\")", collapse=" + ")
    eq_num <- paste0(" + ",paste0(W_factors, collapse=" + "))
    equation <- as.formula(paste0("Y~s(X, bs=\"cr\")+",eq_fact,eq_num,"+ s(id,bs=\"re\",by=dummy)"))

    
    fit <- mgcv::gam(formula = equation,data=d)
  }else{
    fit <- mgcv::gam(Y~s(X, bs="cr")+s(id,bs="re",by=dummy),data=d)
  }
  
  return(list(fit=fit, dat=d))
}



Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
  
predict_gam_diff <- function(fit, d, quantile_diff=c(0.25,0.75), Xvar, Yvar){
  
  d$dummy<-0
  
  Wvars <- colnames(d)[!(colnames(d) %in% c("Y","X" ,"id" ,"dummy"))]
  #set covariates to the median/mode
  for(i in Wvars){
   if(class(d[,i])=="character"|class(d[,i])=="factor"){
     d[,i] <- Mode(d[,i])
   }else{
     d[,i] <- median(d[,i])
   }
  }

  d <- d[order(d$X),]
  
  Xp <- predict(fit,newdata=d,type="lpmatrix")
  # order the prediction matrix in the order of the exposure
  Xp <- Xp[order(d$X),]

  # take difference from the 25th percentile of X
  diff <- t(apply(Xp,1,function(x) x - Xp[round(nrow(d)*quantile_diff[1],0),]))
  #diff <- t(apply(Xp,1,function(x) x - Xp[1,]))

  # calculate the predicted differences
  point.diff <- diff %*% coef(fit)

  # calculate the pointwise SE - naive SE
  se.diff <- sqrt(diag( diff%*%vcov(fit)%*%t(diff) ) )

  # calculate upper and lower bounds
  lb.diff <- point.diff - 1.96*se.diff
  ub.diff <- point.diff + 1.96*se.diff
  
  plotdf<-data.frame(Y=Yvar, X= Xvar, q1=d$X[round(nrow(d)*quantile_diff[1],0)], q3=d$X[round(nrow(d)*quantile_diff[2],0)], point.diff, lb.diff=lb.diff, ub.diff=ub.diff)

  
  res <- plotdf[round(nrow(d)*quantile_diff[2],0),]
  return(list(res=res, plotdf=plotdf))
}





gam_simul_CI <- function(m,newdata,nreps=10000) {
  require(mgcv)
  require(dplyr)
  Vb <- vcov(m,unconditional = TRUE)
  pred <- predict(m, newdata, se.fit = TRUE)
  fit <- pred$fit
  se.fit <- pred$se.fit
  BUdiff <- MASS::mvrnorm(n=nreps, mu = rep(0, nrow(Vb)), Sigma = Vb)
  Cg <- predict(m, newdata, type = "lpmatrix")
  simDev <- Cg %*% t(BUdiff)
  absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
  masd <- apply(absDev, 2L, max)
  crit <- quantile(masd, prob = 0.95, type = 8)
  pred <- data.frame(newdata,fit=pred$fit,se.fit=pred$se.fit)
  pred <- mutate(pred,
                 uprP = fit + (2 * se.fit),
                 lwrP = fit - (2 * se.fit),
                 uprS = fit + (crit * se.fit),
                 lwrS = fit - (crit * se.fit)
  )
  return(pred)
}






