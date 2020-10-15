
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


source(here("src/0-gam-functions.R"))

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
