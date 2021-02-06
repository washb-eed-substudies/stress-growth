


rm(list=ls())

source(here::here("0-config.R"))


d<-read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-stress-growth-covariates-stresslab-anthro.csv"))




#---------------------------------------------------------------------------------------------
# transform outcome distributions
#---------------------------------------------------------------------------------------------
d <- d %>% 
  mutate(
    t3_saa_z01_raw=t3_saa_z01, 
    t3_saa_z02_raw=t3_saa_z02, 
    t3_cort_z01_raw=t3_cort_z01, 
    t3_cort_z03_raw=t3_cort_z03, 
    t2_f2_8ip_raw=t2_f2_8ip, 
    t2_f2_23d_raw=t2_f2_23d, 
    t2_f2_VI_raw=t2_f2_VI,
    t2_f2_12i_raw=t2_f2_12i, 
    t3_gcr_mean_raw=t3_gcr_mean, 
    t3_gcr_cpg12_raw=t3_gcr_cpg12,
    t3_saa_z01=log(t3_saa_z01), 
    t3_saa_z02=log(t3_saa_z02), 
    t3_cort_z01=log(t3_cort_z01), 
    t3_cort_z03=log(t3_cort_z03), 
    t2_f2_8ip=log(t2_f2_8ip), 
    t2_f2_23d=log(t2_f2_23d), 
    t2_f2_VI=log(t2_f2_VI),
    t2_f2_12i=log(t2_f2_12i), 
    t3_gcr_mean2=logit(t3_gcr_mean/100), 
    t3_gcr_cpg12=logit(t3_gcr_cpg12/100))


#Clean inf values
d <- do.call(data.frame,lapply(d, function(x) replace(x, is.infinite(x),NA)))



#---------------------------------------------------------------------------------------------
# calc combined iso variable
#---------------------------------------------------------------------------------------------



# Combined exposures: To determine overall oxidative stress, we will combine our four measures of urinary F2-
# isoprostanes: iPF(2a)-III, 2,3-dinor-iPF(2a)-III, iPF(2a)-VI, and 8,12-iso-iPF(2a)-VI that are
# consistent with prior oxidative stress operationalization into a single score.29 We will use
# the first principal component of a principal components analysis of the four measures of
# urinary F2-isoprostanes as the oxidative stress score if all measures are correlated with each other (P-value < 0.2), 
# otherwise we will analyze the urinary F2-isoprostanes separately.

#Get correlation of isoprostanes
iso <- d %>% select(c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i"))       

cor(iso, use="pairwise.complete.obs")
cor.test(iso[,1], iso[,2])$p.value < 0.2
cor.test(iso[,1], iso[,3])$p.value < 0.2
cor.test(iso[,1], iso[,4])$p.value < 0.2
cor.test(iso[,2], iso[,3])$p.value < 0.2
cor.test(iso[,2], iso[,4])$p.value < 0.2
cor.test(iso[,3], iso[,4])$p.value < 0.2


#isoprostanes are significantly correlated, so calculate 1st principal component

df <-  d %>% select(c("childid","t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i")) %>% as.data.frame()
dim(df)
df <- df[complete.cases(df),]
dim(df)

# #Select assets and seperate out ID
id<-subset(df, select=c("childid")) #drop subjectid
df<-df[,which(!(colnames(df) %in% c("childid")))]

##Computing the principal component using eigenvalue decomposition ##
princ.return <- princomp(df) 


## To get the first principal component in a variable ##
load <- loadings(princ.return)[,1]   

pr.cp <- as.matrix(df) %*% load  ## Matrix multiplication of the input data with the loading for the 1st PC gives us the 1st PC in matrix form. 

df$t2_iso_pca <- as.numeric(pr.cp) ## Gives us the 1st PC in numeric form in pr.

#examine combined score
hist(df$t2_f2_8ip)
hist(df$t2_f2_23d)
hist(df$t2_f2_VI)
hist(df$t2_f2_12i)
hist(df$t2_iso_pca)

#check direction between individual isoprostanes and the combined score
ggplot(df, aes(x=t2_f2_8ip, y=t2_iso_pca)) + geom_point() + geom_smooth()
ggplot(df, aes(x=t2_f2_23d, y=t2_iso_pca)) + geom_point() + geom_smooth()
ggplot(df, aes(x=t2_f2_VI, y=t2_iso_pca)) + geom_point() + geom_smooth()
ggplot(df, aes(x=t2_f2_12i, y=t2_iso_pca)) + geom_point() + geom_smooth()

#merge combined score back into main dataset
df.pca <- data.frame(childid=id, t2_iso_pca=df$t2_iso_pca)

dfull <- left_join(d, df.pca, by="childid")
hist(dfull$t2_iso_pca)



## add household wealth
d_hhwealth <- read.csv("C:/Users/Sophia/Documents/ee-secondary/sophia scripts/hhwealth.csv")
dfull <- left_join(dfull, d_hhwealth, by="dataid")



## convert time of day of pre-stressor measurement of cortisol and sAA into continuous variable
time_day <- dfull$t3_col_time_z01
time_split <- str_split(time_day, ":")
cont_time <- function(list_hr_min){
  # takes in list of time
  # first number is hour of the day
  # second number in list is minute of the hour
  num_time <- as.numeric(unlist(list_hr_min))
  num_time[1]+num_time[2]/60
}
dfull$t3_col_time_z01_cont <- sapply(time_split, cont_time)



## check covariate missingness
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "roof", "HHwealth",
         "tr","cesd_sum_t2", "diar7d_t2", "tr", "life_viol_any_t3") %>% unique()

Wvars2_anthro<-c("ageday_at2", "month_at2")
Wvars3_anthro<-c("ageday_at3", "month_at3", "diar7d_t3", "cesd_sum_ee_t3", "pss_sum_mom_t3", "life_viol_any_t3")  

Wvars2_F2<-c("ageday_ut2", "month_ut2") 
Wvars3_vital<-c("laz_t2", "waz_t2", "ageday_t3_vital", "month_vt3", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t3", "life_viol_any_t3") 
Wvars3_salimetrics<-c("laz_t2", "waz_t2", "ageday_t3_salimetrics", "month_lt3", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t3", "life_viol_any_t3") 
Wvars3_oragene<-c("laz_t2", "waz_t2", "ageday_t3_oragene", "month_ot3", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t3", "life_viol_any_t3") 


generate_miss_tbl <- function(Wvars, d){
  W <- d %>% select(all_of(Wvars))  
  miss <- data.frame(name = names(W), missing = colSums(is.na(W))/nrow(W), row.names = c(1:ncol(W)))
  for (i in 1:nrow(miss)) {
    miss$class[i] <- class(W[,which(colnames(W) == miss[i, 1])])
  }
  miss 
}

generate_miss_tbl(Wvars, dfull)

# add missingness category to IPV covariate
dfull$life_viol_any_t3<-as.factor(dfull$life_viol_any_t3)
summary(dfull$life_viol_any_t3)
dfull$life_viol_any_t3<-addNA(dfull$life_viol_any_t3)
levels(dfull$life_viol_any_t3)[length(levels(dfull$life_viol_any_t3))]<-"Missing"
summary(dfull$life_viol_any_t3)

# add missingness category to diarrhea covariates
summary(dfull$diar7d_t2)
dfull$diar7d_t2<-as.factor(dfull$diar7d_t2)
dfull$diar7d_t2<-addNA(dfull$diar7d_t2)
levels(dfull$diar7d_t2)[length(levels(dfull$diar7d_t2))]<-"Missing"
summary(dfull$diar7d_t2)

generate_miss_tbl(Wvars, dfull)

# check with time-varying covariates
W2_F2.W2_anthro <- c(Wvars, Wvars2_F2 ,Wvars2_anthro) %>% unique(.)
W2_F2.W3_anthro <- c(Wvars, Wvars2_F2 ,Wvars3_anthro, 
                     "laz_t2", "waz_t2") %>% unique(.)
W2_F2.W23_anthro <- c(Wvars, Wvars2_F2, Wvars2_anthro, Wvars3_anthro)
W3_vital.W3_anthro <- c(Wvars, Wvars3_vital, Wvars3_anthro) %>% unique(.)
W3_salimetrics.W3_anthro <- c(Wvars, Wvars3_salimetrics, Wvars3_anthro) %>% unique(.)
W3_oragene.W3_anthro <- c(Wvars, Wvars3_oragene, Wvars3_anthro) %>% unique(.)

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
  
  if(j=="hcz_t3"){Wset=c(Wset, "hcz_t2")}
  return(Wset)
}

Xvars <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_iso_pca")            
Yvars <- c("laz_t2", "waz_t2", "whz_t2" ,"hcz_t2", 
           "len_velocity_t2_t3", "wei_velocity_t2_t3", "hc_velocity_t2_t3",
           "laz_t3", "waz_t3", "whz_t3", "hcz_t3",
           "delta_laz_t2_t3", "delta_waz_t2_t3", "delta_whz_t2_t3", "delta_hcz_t2_t3")

for (i in Xvars){
  for (j in Yvars){
    print(i)
    print(j)
    Wvars = pick_covariates(i, j)
    d_sub <- subset(dfull, !is.na(dfull[,i]) & !is.na(dfull[,j]))
    print(generate_miss_tbl(Wvars, d_sub))
  }
}

Xvars <- c("t3_cort_slope", "t3_residual_cort", "t3_cort_z01", "t3_cort_z03",
           "t3_saa_slope", "t3_residual_saa", "t3_saa_z01", "t3_saa_z02",
           "t3_map", "t3_hr_mean",
           "t3_gcr_mean", "t3_gcr_cpg12")            
Yvars <- c("laz_t3", "waz_t3", "whz_t3", "hcz_t3")

for (i in Xvars){
  for (j in Yvars){
    print(i)
    print(j)
    Wvars = pick_covariates(i, j)
    d_sub <- subset(dfull, !is.na(dfull[,i]) & !is.na(dfull[,j]))
    print(generate_miss_tbl(Wvars, d_sub))
  }
}

# add missingness category to diar7d_t3
summary(dfull$diar7d_t3)
dfull$diar7d_t3<-as.factor(dfull$diar7d_t3)
dfull$diar7d_t3<-addNA(dfull$diar7d_t3)
levels(dfull$diar7d_t3)[length(levels(dfull$diar7d_t3))]<-"Missing"
summary(dfull$diar7d_t3)


hist(dfull$laz_t2)
hist(dfull$waz_t2)
hist(dfull$hcz_t2)


saveRDS(dfull, paste0(dropboxDir,"Data/Cleaned/Andrew/stress_growth_data.RDS"))


