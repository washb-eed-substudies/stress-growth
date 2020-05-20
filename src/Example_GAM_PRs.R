
#-------------------------------------
# gam-pr-for-stella-v3.R
#
# ben arnold (benarnold@berkeley.edu)
#
#
# simulate fake data, and illustrate
# how to calculate and plot a point-wise
# prevalence ratio (PR) for a 
# continuous exposure and a binary
# outcome
#
# version 3 (8 apr 2015)
# * updated the plotting smoother to smooth.spline
# * fixed the plot code so that it references the smooth.spline object
#
# version 2 (2 apr 2015)
# * added smoothed predictions to plot
# * added the rug plot to bottom
#
# version 1 (2 apr 2015)
#-------------------------------------

#-------------------------------------
# input files:
#   none
#
# output files:
#   none
#-------------------------------------

#-------------------------------------
# preamble
#-------------------------------------
rm(list=ls())
library(mgcv)
library(RColorBrewer)

#-------------------------------------
# create some fake data
#-------------------------------------
set.seed(3723)
w  <- rnorm(1000,2,1)
x  <- rnorm(1000,0,1)
py <- 1/(1+exp(-x))
y  <- rbinom(1000,prob=py,size=1)
df <- data.frame(y,x,w)

#-------------------------------------
# GAM fit with splines for x and w
#-------------------------------------
# 
fit <- gam(y~s(x,bs="cr") +s(w,bs="cr"), family=poisson,data=df)
summary(fit)

#-------------------------------------
# default plot of fitted values 
# (linear predictor)
#-------------------------------------
plot(fit,pages=1)

#-------------------------------------
# Calculate the pointwise PR
# (easier)
# and get the pointwise SE(PR)
# (more complicated)
# 
# adapted solution based on another
# solution by Dr. Simon Wood
# (author of mgcv)
# https://stat.ethz.ch/pipermail/r-help//2012-May/314387.html
#-------------------------------------

# note:
# log(PR) = log(E(y_1)/E(y_0)) = s(x_1) - s(x_0)
# where y_0 is the reference group
# and so the PR is estimated by the difference in the linear predictor

# make predictions at the reference value
# for your exposure, x, values of x over the range
# sort by x just for convenience
pd <- df
pd <- pd[order(pd$x),]
Xp <- predict(fit,newdata=pd,type="lpmatrix")

# Xp %*% coef(fit) gives the linear predictor: log(E(y))
# so the required difference is computed by subtracting off
# the reference group value, here assumed to be min(x)
# but this will depend on your specific exposure / scientific question

# order the prediction matrix in the order of the exposure
Xp <- Xp[order(pd$x),]
# take difference on the log scale from the min(x)
diff <- t(apply(Xp,1,function(x) x - Xp[1,]))

# calculate the log PR
logPR <- diff %*% coef(fit)

# calculate the pointwise SE for log PR
se.logPR <- sqrt(diag( diff%*%vcov(fit)%*%t(diff) ) )

# calculate upper and lower bounds
lb.logPR <- logPR - 1.96*se.logPR
ub.logPR <- logPR + 1.96*se.logPR

# make the spikey predictions smooth with another spline
smooth.logPR    <- smooth.spline(x=pd$x,y=logPR)
smooth.lb.logPR <- smooth.spline(x=pd$x,y=lb.logPR)
smooth.ub.logPR <- smooth.spline(x=pd$x,y=ub.logPR)

#-------------------------------------
# plot the PR as a function of x
#-------------------------------------

# grab some nicer colors
rugcol <- brewer.pal(9,"Set1")[3]
linecol <- "black"
cicol <- "gray90"

# order of x (for plotting)
# (redundant because sorted above)
plotx <- pd$x[order(pd$x)]

ytics <- c(1,2,4,8,16)
plot(smooth.logPR$x,exp(smooth.logPR$y),
     type="n",bty="n",las=1,
     ylab="",log="y",yaxt="n",ylim=range(ytics),
     xlab="Exposure (x)"
)
axis(2,at=ytics,las=1)
mtext("PR",side=2,line=2.5,las=1)

# add CIs (dashed)
# lines(plotx,exp(smooth.lb.logPR),lty=2)
# lines(plotx,exp(smooth.ub.logPR),lty=2)

# add CIs (shaded)
polygon(x=c(smooth.lb.logPR$x,rev(smooth.ub.logPR$x)),
        y=exp(c(smooth.lb.logPR$y,rev(smooth.ub.logPR$y))),
        col=cicol,border=NA)

# add the predicted values
lines(smooth.logPR$x,exp(smooth.logPR$y),lwd=2,col=linecol)

# add the rug
segments(x0=smooth.logPR$x,y0=min(ytics)-0.1,y1=min(ytics)-0.05,col=rugcol)