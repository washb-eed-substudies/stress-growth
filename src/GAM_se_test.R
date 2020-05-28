
rm(list=ls())
library(tidyverse)
library(mgcv)
library(sandwich)

source(here::here("0-config.R"))
source(here::here("src/0-base-functions.R"))
#load covariates, exposures, outcomes dataset
load(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.RData"))
df <- d %>% subset(., select = c(clusterid, TS_t2, sex, laz_t3)) %>%
  rename(x=TS_t2, Y=laz_t3) %>%
  filter(!is.na(Y) & !is.na(x)) %>%
  mutate(dummy=1)






#Diff between the 25th and 75th percentile
point.diff_RE <- summary(newdata$x)[5] %*% coef(fitRE) - summary(newdata$x)[2] %*% coef(fitRE)
point.diff_RE

naive.se.diff <- sqrt(diag( diff%*%vcov(fit)%*%t(diff) ) )


point.diff_RE <- diff_RE %*% coef(fitRE)
naive.se.diff <- sqrt(diag( diff%*%vcov(fit)%*%t(diff) ) )


#Simulate data with non-linear x-y relationship and clustered obs
set.seed(1234)

n<-1000
x <- rnorm(n)
clusterid <- factor(rep(1:100, each=10))

df <- data.frame(x, clusterid)
df <- df %>% group_by(clusterid) %>% mutate(clust_mean=rnorm(1, 0, 10)) %>%
  ungroup() %>%
  mutate(clust_mean=clust_mean + rnorm(1,0, 1),
         dummy=1)

df$Y <-  2*df$x^2 - 5*df$x + df$clust_mean #+ rnorm(n)
df <- df %>% arrange(x) %>% mutate(dummy=1, clusterid=factor(clusterid))

#calculate ICC
library(ICC)
ICCest(x=clusterid, y=Y, data=df)


#data and simple spline
ggplot(df, aes(x=x, y=Y)) + geom_point() + geom_smooth()

#Fit simple GAM model
fit <- gam(Y~ s(x,bs="cr"), family=gaussian,data=df)



#----------------------------------
# add a random effect for clustering
#----------------------------------
# fit GAM with a spline for age
fitRE_old <- mgcv::gam(Y~s(x, bs="cr")+s(clusterid,bs="re",by=dummy),data=df)
fitRE <- mgcv::gam(Y~s(x, bs="cr")+s(clusterid,bs="re"),data=df)




#Get model predictions
pd <- df
pd <- pd[order(pd$x),]
newd <- pd %>% mutate(dummy=0)
Xp <- predict(fit,newdata=pd,type="lpmatrix")
Xp_RE_old <- predict(fitRE_old,newdata=newd,type="lpmatrix")
Xp_RE <- predict(fitRE,newdata=newd,type="lpmatrix", exclude = "s(clusterid)")
head(Xp_RE_old[,1:10])
head(Xp_RE[,1:10])


Xp_RE_test <- predict(fitRE,newdata=pd,exclude = "s(clusterid)", se.fit=TRUE)
Xp_RE_test2 <- predict(fitRE_old,newdata=newd, se.fit=TRUE)
head(Xp_RE_test$fit)
head(Xp_RE_test2$fit)


# estimate simultaneous CIs around the curve
# for the prediction data, set the dummy to 0 to 
# zero out all of the random effects
# see posts on Stack Exchange for explanation:
# https://stats.stackexchange.com/questions/131106/predicting-with-random-effects-in-mgcv-gam/131116#131116
# https://stats.stackexchange.com/questions/189384/predicting-mean-smooth-in-gam-with-smooth-by-random-factor-interaction
fit2ci <- gamCI(m=fitRE,newdata=newd,nreps=10000)
fit2ci <- gamCI(m=fitRE_old,newdata=newd,nreps=10000)



# order the prediction matrix in the order of the exposure
Xp <- Xp[order(pd$x),]
Xp_RE <- Xp_RE[order(pd$x),]

# take difference from the median X
# diff <- t(apply(Xp,1,function(x) x - Xp[floor(nrow(Xp)/2),]))
# diff_RE <- t(apply(Xp_RE,1,function(x) x - Xp_RE[floor(nrow(Xp_RE)/2),]))
diff <- t(apply(Xp,1,function(x) x - Xp[1,]))
diff_RE <- t(apply(Xp_RE,1,function(x) x - Xp_RE[1,]))

# calculate the predicted differences
point.diff <- diff %*% coef(fit)
point.diff_RE <- diff_RE %*% coef(fitRE)
#point.diff_RE <- diff_RE[,1:10] %*% coef(fitRE)[1:10]

# calculate the pointwise SE - naive SE
naive.se.diff <- sqrt(diag( diff%*%vcov(fit)%*%t(diff) ) )

# calculate the pointwise SE - clustered SE
#cluster.se.diff <- sqrt(diag( diff%*%vcovCL(fit, cluster=df$clusterid)%*%t(diff) ) )

# calculate the GAMM SE
gamm.se.diff <- sqrt(diag( diff_RE%*% vcov(fitRE) %*%t(diff_RE) ) )
# gamm.se.diff <- sqrt(diag( diff_RE[,1:10]%*% vcov(fitRE)[1:10,1:10] %*%t(diff_RE[,1:10]) ) )


#Examine SE dist by method
summary(naive.se.diff)
summary(gamm.se.diff)


# calculate upper and lower bounds
lb.diff.naive <- point.diff - 1.96*naive.se.diff
ub.diff.naive <- point.diff + 1.96*naive.se.diff

lb.diff.cluster <- point.diff - 1.96*cluster.se.diff
ub.diff.cluster <- point.diff + 1.96*cluster.se.diff

lb.diff_RE <- point.diff_RE - 1.96*gamm.se.diff
ub.diff_RE <- point.diff_RE + 1.96*gamm.se.diff

plotdf.naive<-data.frame(x=pd$x, point.diff, lb.diff=lb.diff.naive, ub.diff=ub.diff.naive)
plotdf.clustered<-data.frame(x=pd$x, point.diff, lb.diff=lb.diff.cluster, ub.diff=ub.diff.cluster)
plotdf.RE<-data.frame(x=pd$x, point.diff_RE, lb.diff=lb.diff_RE, ub.diff=ub.diff_RE)

plotdf.test<-data.frame(x=pd$x, point.diff=Xp_RE_test$fit, 
                        lb.diff=Xp_RE_test$fit-1.96*Xp_RE_test$se.fit, 
                        ub.diff=Xp_RE_test$fit+1.96*Xp_RE_test$se.fit)



#Plot the 3 methods
ggplot(plotdf.naive) + geom_ribbon(aes(x=x, ymin=lb.diff, ymax=ub.diff), alpha=0.5) + 
  geom_path(aes(x=x, y=lb.diff), color="blue")+
  geom_path(aes(x=x, y=ub.diff), color="red")+
  geom_path(aes(x=x, y=point.diff), color="black")

ggplot(plotdf.test) + geom_ribbon(aes(x=x, ymin=lb.diff, ymax=ub.diff), alpha=0.5) + 
  geom_path(aes(x=x, y=lb.diff), color="blue")+
  geom_path(aes(x=x, y=ub.diff), color="red")+
  geom_path(aes(x=x, y=point.diff), color="black")


# ggplot(plotdf.clustered) + geom_ribbon(aes(x=x, ymin=lb.diff, ymax=ub.diff), alpha=0.5) + 
#   geom_path(aes(x=x, y=lb.diff), color="blue")+
#   geom_path(aes(x=x, y=ub.diff), color="red")+
#   geom_path(aes(x=x, y=point.diff), color="black")


ggplot(plotdf.RE) + geom_ribbon(aes(x=x, ymin=lb.diff, ymax=ub.diff), alpha=0.5) + 
  geom_path(aes(x=x, y=lb.diff), color="blue")+
  geom_path(aes(x=x, y=ub.diff), color="red")+
  geom_path(aes(x=x, y=point.diff_RE ), color="black")


fit2ci <- fit2ci %>% arrange(x)
ggplot(fit2ci) + geom_ribbon(aes(x=x, ymin=lwrS, ymax=uprS), alpha=0.5) + 
  geom_path(aes(x=x, y=lwrS), color="blue")+
  geom_path(aes(x=x, y=uprS), color="red")+
  geom_path(aes(x=x, y=fit ), color="black")








